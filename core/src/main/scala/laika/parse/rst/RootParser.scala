/*
 * Copyright 2013-2017 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package laika.parse.rst

import com.typesafe.config.{Config, ConfigValueFactory}
import laika.api.ext.{MarkupParsers, ParserDefinition, ParserDefinitionBuilders}
import laika.directive.Directives.{Blocks, Spans}
import laika.directive.MarkupDirectiveParsers
import laika.parse.core.markup.RootParserBase
import laika.parse.core.text.TextParsers._
import laika.parse.core.{Parsed, Parser, ParserContext, Success}
import laika.parse.rst.Directives.{Directive, DirectivePart}
import laika.parse.rst.Elements.{CustomizedTextRole, FieldList, ReferenceName}
import laika.parse.rst.TextRoles.{RoleDirectivePart, TextRole}
import laika.rewrite.TreeUtil
import laika.tree.Documents.Document
import laika.tree.Elements._
import laika.tree.Paths.Path

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

/** The main parser for reStructuredText, combining the individual parsers for block and inline elements,
  * and adding functionality like directives depending on configuration.
  *
  * @author Jens Halm
  */
class RootParser(parserExtensions: ParserDefinitionBuilders = ParserDefinitionBuilders(),
                 laikaBlockDirectives: Map[String, Blocks.Directive] = Map(),
                 laikaSpanDirectives:  Map[String, Spans.Directive] = Map(),
                 blockDirectives: Seq[Directive[Block]] = Seq(),
                 spanDirectives: Seq[Directive[Span]] = Seq(),
                 textRoles: Seq[TextRole] = Seq(),
                 defaultTextRole: String = "title-reference",
                 isStrict: Boolean = false) extends RootParserBase {


  /** Parses an escaped character. For most characters it produces the character
    *  itself as the result with the only exception being an escaped space character
    *  which is removed from the output in reStructuredText.
    *
    *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#escaping-mechanism]].
    */
  override lazy val escapedChar: Parser[String] = (" " ^^^ "") | (any take 1)


  private lazy val markupParserExtensions: MarkupParsers = parserExtensions.markupParsers(this)

  private val directiveParsers =
    if (!isStrict) Some(new MarkupDirectiveParsers(this, laikaBlockDirectives, laikaSpanDirectives)) else None

  private val inlineParsers = new InlineParsers(this, defaultTextRole)
  private val blockParsers = new BlockParsers(this)
  private val tableParsers = new TableParsers(this)
  private val listParsers = new ListParsers(this)

  private val rstBlockDirectives: Map[String, DirectivePart[Block]] =        blockDirectives map { d => (d.name.toLowerCase, d.part(this)) } toMap
  private val rstSpanDirectives: Map[String, DirectivePart[Span]]  =         spanDirectives  map { d => (d.name.toLowerCase, d.part(this)) } toMap
  private val rstTextRoles: Map[String, RoleDirectivePart[String => Span]] = textRoles       map { r => (r.name.toLowerCase, r.part(this)) } toMap

  private val exParsers = new ExplicitBlockParsers(this, rstBlockDirectives, rstSpanDirectives, rstTextRoles, defaultTextRole)

  private val textRoleElements = textRoles map { role => CustomizedTextRole(role.name, role.default) }


  private def toParser (definition: ParserDefinition[Block]): Parser[Block] =
    definition.startChar.fold(definition.parser){_ ~> definition.parser} // TODO - temporary until startChar is processed

  protected lazy val spanParsers: Map[Char,Parser[Span]] = {

    val mainParsers = inlineParsers.spanParsers
    val extSpans = markupParserExtensions.spanParserMap
    val directiveSpans = directiveParsers.map(_.spanParsers).getOrElse(Map())

    val withExt = mergeSpanParsers(mainParsers, extSpans)
    mergeSpanParsers(withExt, directiveSpans)
  }


  protected lazy val topLevelBlock: Parser[Block] = {

    val blockDirectives = directiveParsers.map(_.blockDirective).toSeq
    val extBlocks = markupParserExtensions.blockParsers.map(toParser)

    mergeBlockParsers(blockDirectives ++ extBlocks ++ mainBlockParsers)
  }

  protected lazy val nestedBlock: Parser[Block] = {

    val blockDirectives = directiveParsers.map(_.blockDirective).toSeq
    val extBlocks = markupParserExtensions.blockParsers.filter(_.useInRecursion).map(toParser)

    mergeBlockParsers(blockDirectives ++ extBlocks ++ mainBlockParsers)
  }

  protected lazy val nonRecursiveBlock: Parser[Block] = {
    val extBlocks = markupParserExtensions.blockParsers.filterNot(_.isRecursive).map(toParser)

    mergeBlockParsers(extBlocks :+ (exParsers.comment | blockParsers.paragraph))
  }


  private lazy val mainBlockParsers = Seq(
    listParsers.bulletList,
    listParsers.enumList,
    listParsers.fieldList,
    listParsers.lineBlock,
    listParsers.optionList,
    exParsers.explicitBlockItem,
    tableParsers.gridTable,
    tableParsers.simpleTable,
    blockParsers.doctest,
    blockParsers.blockQuote,
    blockParsers.headerWithOverline,
    blockParsers.transition,
    blockParsers.headerWithUnderline,
    listParsers.definitionList,
    blockParsers.paragraph
  )


  override def parseDocument (reader: ParserContext, path: Path): Document = {
    def extractDocInfo (config: Config, root: RootElement) = {
      val docStart = root.content dropWhile { case c: Comment => true; case h: DecoratedHeader => true; case _ => false } headOption
      val docInfo = docStart collect { case FieldList(fields,_) => fields map (field => (TreeUtil.extractText(field.name),
        field.content collect { case p: Paragraph => TreeUtil.extractText(p.content) } mkString)) toMap }
      docInfo map (i => config.withValue("docInfo", ConfigValueFactory.fromMap(i.asJava))) getOrElse config
    }

    val (config, root) = parseConfigAndRoot(reader, path)
    val finalConfig = extractDocInfo(config, root)
    val finalRoot = root.copy(content = root.content ++ textRoleElements)
    Document(path, finalRoot, TreeUtil.extractFragments(root.content), finalConfig)
  }


  /** Builds a parser for a list of blocks based on the parser for a single block.
    *
    *  Overridden to add the processing required for cases where a block has influence
    *  on the parsing or processing of the subsequent block.
    *
    *  This includes checking each Paragraph for a double colon ending which turns
    *  the following block into a literal block as well as processing internal
    *  link targets and section headers.
    *
    *  @param parser the parser for a single block element
    *  @return a parser for a list of blocks
    */
  override def blockList (parser: => Parser[Block]): Parser[List[Block]] = Parser { in =>
    case object Mock extends Block { val options = NoOpt }

    val defaultBlock = parser <~ opt(blankLines)
    val litBlock = (blockParsers.literalBlock | defaultBlock) <~ opt(blankLines)
    val elems = new ListBuffer[Block]
    elems += Mock

    def processLiteralMarker (par: Paragraph) = {
      par.content.lastOption match {
        case Some(Text(text,opt)) if text.trim.endsWith("::") =>
          val drop = if (text.length > 2 && text.charAt(text.length-3) == ' ') 3 else 1
          val spans = par.content.init.toList ::: List(Text(text.dropRight(drop),opt))
          (Paragraph(spans,par.options), litBlock)
        case _ => (par, defaultBlock)
      }
    }
    def toLinkId (h: DecoratedHeader) = ReferenceName(TreeUtil.extractText(h.content)).normalized

    def result = {
      elems += Mock
      val processed = elems.toList.sliding(3).foldLeft(new ListBuffer[Block]()) {
        case (buffer, _ :: (InternalLinkTarget(Id(id1))) :: (InternalLinkTarget(Id(id2))) :: Nil) =>
          buffer += LinkAlias(id1, id2)
        case (buffer, _ :: (InternalLinkTarget(Id(id))) :: (et: ExternalLinkDefinition) :: Nil) =>
          buffer += et.copy(id = id)
        case (buffer, _ :: (it: InternalLinkTarget) :: (h: DecoratedHeader) :: Nil) => buffer
        case (buffer, _ :: (it: InternalLinkTarget) :: (c: Customizable) :: Nil) =>
          if (c.options.id.isDefined) buffer += it else buffer
        case (buffer, _ :: (it: InternalLinkTarget) :: _ :: Nil) => buffer += it

        case (buffer, (it: InternalLinkTarget) :: (h @ DecoratedHeader(_,_,oldOpt)) :: _) =>
          buffer += h.copy(options = oldOpt + Id(toLinkId(h)), content = it +: h.content)
        case (buffer, _ :: (h @ DecoratedHeader(_,_,oldOpt)) :: _) =>
          buffer += h.copy(options = oldOpt + Id(toLinkId(h)))

        case (buffer, (InternalLinkTarget(Id(id))) :: (et: ExternalLinkDefinition) :: _ :: Nil) =>
          buffer += et
        case (buffer, (InternalLinkTarget(Id(id))) :: (c: Customizable) :: _ :: Nil) if c.options.id.isEmpty =>
          buffer += TreeUtil.setId(c, id)

        case (buffer, _ :: _ :: Nil)   => buffer // only happens for empty results (with just the 2 mocks)
        case (buffer, _ :: other :: _) => buffer += other
        case (buffer, _)          => buffer
      }
      processed.toList
    }

    @tailrec
    def parse (p: Parser[Block], in: ParserContext): Parsed[List[Block]] = p.parse(in) match {
      case Success(Paragraph(Text(txt,_) :: Nil,_), rest) if txt.trim == "::" => parse(litBlock, rest)
      case Success(p: Paragraph, rest) =>
        val (paragraph, parser) = processLiteralMarker(p)
        elems += paragraph
        parse(parser, rest)
      case Success(x, rest) => elems += x; parse(defaultBlock, rest)
      case _                => Success(result, in)
    }

    parse(defaultBlock, in)
  }

  override def config (path: Path): Parser[Either[InvalidBlock,Config]] =
    directiveParsers.map(_.config(path)).getOrElse(super.config(path))

}
