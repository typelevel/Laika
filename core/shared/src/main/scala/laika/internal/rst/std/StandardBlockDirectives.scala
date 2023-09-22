/*
 * Copyright 2012-2020 the original author or authors.
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

package laika.internal.rst.std

import cats.data.NonEmptySet
import laika.ast.*
import laika.api.config.ConfigValue.{ ObjectValue, StringValue }
import laika.api.config.{ Field, Origin }
import laika.config.LaikaKeys
import laika.internal.rst.ast.{ Contents, FieldList, Include, RstStyle }
import laika.internal.rst.ext.Directives.{ BlockDirective, Directive, DirectivePartBuilder }
import laika.parse.{ GeneratedSource, SourceFragment }
import laika.parse.builders.~
import laika.parse.markup.RecursiveParsers
import laika.internal.rst.ext.Directives.Parts.*
import StandardDirectiveParts.*

import scala.collection.immutable.TreeSet

/** Defines all supported standard block directives of the reStructuredText reference parser.
  *
  *  The following directives are fully supported:
  *
  *  - `compound`
  *  - `container`
  *  - `admonition`
  *  - `attention`
  *  - `caution`
  *  - `danger`
  *  - `error`
  *  - `hint`
  *  - `important`
  *  - `note`
  *  - `tip`
  *  - `warning`
  *  - `topic`
  *  - `sidebar`
  *  - `rubric`
  *  - `epigraph`
  *  - `highlights`
  *  - `pull-quote`
  *  - `parsed-literal`
  *  - `table`
  *  - `contents`
  *  - `sectnum`
  *  - `figure`
  *  - `image`
  *  - `header`
  *  - `footer`
  *  - `title`
  *  - `meta`
  *
  *  The following directives are supported with some limitations:
  *
  *  - `code` does currently not support syntax highlighting
  *    (it allows to set the language so client-side highlighters can be integrated if required)
  *
  *  - `sectnum` does currently not support the `prefix`, `suffix` and `start` options.
  *
  *  - `raw` does not support the `file` or `url` options.
  *
  *  - `include` does not support any of the options apart from the filename. See the API entry for this directive
  *    for details.
  *
  *  Finally, for some directives there is currently no support planned:
  *
  *  - `math` (would require external tools)
  *  - `csv-table`, `list-table` (would just require some work, contributions are welcome)
  *  - `target-notes`, `class` (these would require processing beyond the directive itself, therefore would require new API)
  *
  *  @author Jens Halm
  */
private[rst] class StandardBlockDirectives {

  private def positiveInt(value: SourceFragment) = try {
    val i = value.input.toInt
    if (i > 0) Right(i) else Left(s"Not a posivitve number: $i")
  }
  catch {
    case _: NumberFormatException => Left(s"Not a number: $value")
  }

  /** The compound directive,
    *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#compound-paragraph]] for details.
    */
  lazy val compound: DirectivePartBuilder[Block] = {
    (blockContent ~ stdOpt).map { case content ~ opt =>
      BlockSequence(content, opt + RstStyle.compound)
    }
  }

  /** The container directive,
    *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#container]] for details.
    */
  lazy val container: DirectivePartBuilder[Block] = {
    (optArgument(withWS = true) ~ blockContent ~ nameOpt).map { case styles ~ content ~ id =>
      BlockSequence(content, Options(id, styles.map(_.split(" ").toSet).getOrElse(Set())))
    }
  }

  /** The admonition directive,
    *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#generic-admonition]] for details.
    */
  private lazy val genericAdmonition: DirectivePartBuilder[Block] = {
    (spanArgument ~ blockContent ~ stdOpt).map { case title ~ content ~ opt =>
      TitledBlock(title, content, opt + RstStyle.admonition)
    }
  }

  /** The attention, caution, danger, error, hint, important, note, tip and warning directives,
    *  which are all identical apart from their title which can be specified with the style parameter.
    *  See [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#specific-admonitions]] for details.
    */
  def admonition(style: String, title: String): DirectivePartBuilder[Block] = {
    (blockContent ~ stdOpt).map { case content ~ opt =>
      TitledBlock(List(Text(title)), content, opt + Styles(style))
    }
  }

  /** The topic directive,
    *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#topic]] for details.
    */
  lazy val topic: DirectivePartBuilder[Block] = {
    (spanArgument ~ blockContent ~ stdOpt).map { case title ~ content ~ opt =>
      TitledBlock(title, content, opt + RstStyle.topic)
    }
  }

  /** The rubric directive,
    *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#rubric]] for details.
    */
  lazy val rubric: DirectivePartBuilder[Block] = {
    (spanArgument ~ stdOpt).map { case text ~ opt =>
      Paragraph(text, opt + RstStyle.rubric)
    }
  }

  /** The sidebar directive,
    *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#sidebar]] for details.
    */
  def sidebar(p: RecursiveParsers): DirectivePartBuilder[Block] = {
    (spanArgument ~ optField("subtitle", StandardDirectiveParsers.standardSpans(p)) ~
      blockContent ~ stdOpt).map { case title ~ subtitle ~ content ~ opt =>
      val titleAndContent = subtitle.map(s => Paragraph(s, RstStyle.subtitle)).toList ++ content
      TitledBlock(title, titleAndContent, opt + RstStyle.sidebar)
    }
  }

  /** The title directive,
    *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#metadata-document-title]] for details.
    */
  private lazy val titleDirective: DirectivePartBuilder[EmbeddedConfigValue] =
    argument().map(EmbeddedConfigValue(LaikaKeys.title.toString, _))

  /** The meta directive,
    *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#meta]] for details.
    *
    *  In Laika there is no special document tree element for metadata.
    *  Therefore the result will be accessible through the generic
    *  config property in the `Document` class.
    */
  lazy val meta: DirectivePartBuilder[Block] = blockContent map {
    case FieldList(fields, _) :: Nil =>
      val values = fields map { field =>
        Field(
          SpanSequence(field.name).extractText,
          StringValue(field.content.collect { case p: Paragraph => p.extractText }.mkString),
          Origin.root
        )
      }
      EmbeddedConfigValue("meta", ObjectValue(values))
    case _                           =>
      InvalidBlock(
        RuntimeMessage(
          MessageLevel.Error,
          "The meta directive expects a FieldList as its only block content"
        ),
        GeneratedSource
      )
  }

  /** The header directive,
    *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#document-header-footer]] for details.
    */
  lazy val header: DirectivePartBuilder[Block] = blockContent map { blocks =>
    DocumentFragment("header", BlockSequence(blocks))
  }

  /** The footer directive,
    *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#document-header-footer]] for details.
    */
  lazy val footer: DirectivePartBuilder[Block] = blockContent map { blocks =>
    DocumentFragment("footer", BlockSequence(blocks))
  }

  private def tuple(name: String) = optField(name, s => Right((name, s)))

  lazy val sectnum: DirectivePartBuilder[Block] =
    (tuple("depth") ~ tuple("start") ~ tuple("prefix") ~ tuple("suffix")).map {
      case depth ~ start ~ prefix ~ suffix =>
        val fields = (depth.toList ++ start.toList ++ prefix.toList ++ suffix.toList).map {
          case (name, value) =>
            Field(name, StringValue(value.input), Origin.root)
        }
        EmbeddedConfigValue(LaikaKeys.autonumbering.toString, ObjectValue(fields))
    }

  lazy val contents: DirectivePartBuilder[Block] =
    (optArgument(withWS = true) ~ optField("depth", positiveInt) ~ optField("local") ~ optField(
      "class"
    )).map { case title ~ depth ~ local ~ style =>
      Contents(
        title.getOrElse("Contents"),
        GeneratedSource,
        depth.getOrElse(Int.MaxValue),
        local.isDefined,
        toOptions(None, style)
      )
    }

  /** The include directive,
    *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#including-an-external-document-fragment]]
    *  for details.
    *
    *  Note that the only argument supported in Laika is the default argument specifying the path of the file
    *  to include. The other options supported by reStructuredText original parser do not make sense in the
    *  execution context of Laika. They assume that the file is parsed everytime an include directive is used,
    *  whereas in Laika all files of the source tree get parsed in one go and then the include step simply
    *  references the previously parsed node tree. This is both simpler and more efficient when the same
    *  file gets included in multiple places.
    */
  lazy val include: DirectivePartBuilder[Block] = argument().map(Include(_, GeneratedSource))

  /** The epitaph, highlights and pull-quote directives, which are all identical apart from the style
    *  parameter, see
    *  [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#epigraph epigraph]],
    *  [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#highlights highlights]] and
    *  [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#pull-quote pull-quote]] for details.
    */
  def quotedBlock(style: String): DirectivePartBuilder[Block] = blockContent map { blocks =>
    blocks.lastOption match {
      case Some(p @ Paragraph(Text(text, opt) :: _, _)) if text startsWith "-- " =>
        val attr = Text(text.drop(3), opt + Style.attribution) +: p.content.tail
        QuotedBlock(blocks.init, attr, Styles(style))
      case _                                                                     =>
        QuotedBlock(blocks, Nil, Styles(style))
    }
  }

  /** The parsed-literal directive, see
    *  [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#parsed-literal-block]] for details.
    */
  private lazy val parsedLiteral: DirectivePartBuilder[Block] = {
    (spanContent ~ stdOpt).map { case content ~ opt =>
      ParsedLiteralBlock(content, opt)
    }
  }

  /** The table directive, adding a title to standard reStructuredText tables,
    *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#table]] for details.
    */
  def table(p: RecursiveParsers): DirectivePartBuilder[Block] = {
    (optSpanArgument ~ content(StandardDirectiveParsers.table(p)) ~ stdOpt).map {
      case caption ~ tableBlock ~ opt =>
        val table = tableBlock match {
          case t: Table => t
          case block    => Table(TableHead(Nil), TableBody(Seq(Row(Seq(CellType.BodyCell(block))))))
        }
        table.copy(caption = Caption(caption.toList.flatten), options = opt)
    }
  }

  /** The code directive,
    *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#code]] for details.
    *  The current implementation does not support syntax highlighting.
    */
  def code(p: RecursiveParsers): DirectivePartBuilder[Block] = {
    (argument() ~ content(Right(_)) ~ stdOpt).evalMap { case language ~ code ~ opt =>
      p.getSyntaxHighlighter(language).fold[Either[String, Seq[Span]]](
        Right(Seq(Text(code.input)))
      ) { highlighter =>
        highlighter.parse(code).toEither
      }.map { CodeBlock(language, _, Nil, opt) }
    }
  }

  /** The image directive for block elements,
    *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#image]] for details.
    */
  private def imageBlock(p: RecursiveParsers): DirectivePartBuilder[Block] = image(p) map { img =>
    val hAlign =
      Set("align-left", "align-right", "align-center") // promote horizontal align to parent block
    val (pOpt, imgOpt) = img.options.styles.foldLeft((Options.empty, Options(img.options.id))) {
      case ((pOpt, imgOpt), style) =>
        if (hAlign.contains(style)) (pOpt + Styles(style), imgOpt)
        else (pOpt, imgOpt + Styles(style))
    }
    val content        = img.withOptions(imgOpt)
    Paragraph(List(content), pOpt)
  }

  /** The figure directive,
    *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#figure]] for details.
    */
  def figure(p: RecursiveParsers): DirectivePartBuilder[Block] = {
    (image(p) ~ content(StandardDirectiveParsers.captionAndLegend(p)) ~ optField("figclass")).map {
      case image ~ captionAndLegend ~ figStyles =>
        Figure(image, captionAndLegend._1, captionAndLegend._2, toOptions(None, figStyles))
    }
  }

  /** The raw directive, which is not enabled by default,
    *  see [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#raw-data-pass-through]] for details.
    *  It can be enabled with `Transformer.from(ReStructuredText).to(HTML).withRawContent`.
    */
  lazy val rawDirective: Directive[Block] = BlockDirective("raw") {
    (argument(withWS = true) ~ content(Right(_))).map { case formats ~ content =>
      NonEmptySet.fromSet(TreeSet(formats.split(" ").toIndexedSeq: _*)) match {
        case Some(set) => RawContent(set, content.input)
        case None      => InvalidBlock("no format specified", GeneratedSource)
      }
    }
  }

  /** All standard block directives currently supported by Laika, except for
    *  the `raw` directive which needs to be enabled explicitly.
    */
  lazy val blockDirectives: List[Directive[Block]] = List(
    BlockDirective("compound")(compound),
    BlockDirective("container")(container),
    BlockDirective("topic")(topic),
    BlockDirective("rubric")(rubric),
    BlockDirective("parsed-literal")(parsedLiteral),
    BlockDirective.recursive("sidebar")(sidebar),
    BlockDirective.recursive("table")(table),
    BlockDirective.recursive("figure")(figure),
    BlockDirective.recursive("image")(imageBlock),
    BlockDirective("epigraph")(quotedBlock("epigraph")),
    BlockDirective("highlights")(quotedBlock("highlights")),
    BlockDirective("pull-quote")(quotedBlock("pull-quote")),
    BlockDirective.recursive("code")(code),
    BlockDirective("header")(header),
    BlockDirective("footer")(footer),
    BlockDirective("include")(include),
    BlockDirective("title")(titleDirective),
    BlockDirective("meta")(meta),
    BlockDirective("contents")(contents),
    BlockDirective("sectnum")(sectnum),
    BlockDirective("section-autonumbering")(sectnum),
    BlockDirective("admonition")(genericAdmonition),
    BlockDirective("attention")(admonition("attention", "Attention!")),
    BlockDirective("caution")(admonition("caution", "Caution!")),
    BlockDirective("danger")(admonition("danger", "!DANGER!")),
    BlockDirective("error")(admonition("error", "Error")),
    BlockDirective("hint")(admonition("hint", "Hint")),
    BlockDirective("important")(admonition("important", "Important")),
    BlockDirective("note")(admonition("note", "Note")),
    BlockDirective("tip")(admonition("tip", "Tip")),
    BlockDirective("warning")(admonition("warning", "Warning"))
  )

}
