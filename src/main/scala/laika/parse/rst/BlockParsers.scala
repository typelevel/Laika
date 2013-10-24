/*
 * Copyright 2013 the original author or authors.
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

import laika.tree.Elements._
import laika.tree.Documents._
import laika.parse.rst.Elements._
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._
import scala.annotation.tailrec
import scala.util.parsing.input.Reader
import laika.tree.TreeUtil
import com.typesafe.config.ConfigValueFactory
import com.typesafe.config.ConfigValue
import com.typesafe.config.Config

/** Provides the parsers for all types of block-level elements of reStructuredText. 
 *  It merges the individual traits that provide implementations for list, tables, etc. and 
 *  adds the remaining block level parsers that do not fit into any of the subcategories 
 *  supported by the other traits.
 * 
 *  Block parsers are only concerned with splitting the document into 
 *  (potentially nested) blocks. They are used in the first phase of parsing,
 *  while delegating to inline parsers for the 2nd phase.
 * 
 * @author Jens Halm
 */
trait BlockParsers extends laika.parse.BlockParsers 
                      with ListParsers 
                      with TableParsers 
                      with ExplicitBlockParsers { self: InlineParsers =>

  
  override def ws = anyOf(' ') // other whitespace has been replaced with spaces by preprocessor
                        
  
  override def parseDocument (reader: Reader[Char], path: Path) = {
    def extractConfig (config: Config, root: RootElement) = {
      val title = (root collect { case DocumentTitle(title,_) => title } headOption) map (s => ("title", ConfigValueFactory.fromAnyRef(s))) toList
      
      val meta = root collect { case DocumentMetadata(map,_) => map } reduceLeft (_ ++ _)
      val titleAndMeta = if (meta.isEmpty) title else ("meta", ConfigValueFactory.fromMap(meta)) :: title
      
      val docStart = root.content dropWhile { case c: Comment => true; case h: DecoratedHeader => true; case _ => false } headOption 
      val docInfo = docStart collect { case FieldList(fields,_) => fields map (field => (TreeUtil.extractText(field.name), 
          field.content collect { case p: Paragraph => TreeUtil.extractText(p.content) } mkString)) toMap }
      val extraConfig = (docInfo map (m => ("docInfo", ConfigValueFactory.fromMap(m))) toList) ::: titleAndMeta
      
      (config /: extraConfig) { case (config, (name, value)) =>
        config.withValue(name, value)
      }
    }
    
    val (config, root) = parseConfigAndRoot(reader, path)
    val finalConfig = extractConfig(config, root)
    val finalRoot = root.copy(content = root.content ++ textRoleElements)
    new Document(path, finalRoot, TreeUtil.extractFragments(root.content), finalConfig, List(RewriteRules))
  }
  
  /** All the base text roles supported by this parser not including
   *  any custom roles specified within document markup.
   */
  def textRoleElements: List[CustomizedTextRole] = Nil  
  
  
  /** Parses punctuation characters as supported by transitions (rules) and 
   *  overlines and underlines for header sections.
   */
  val punctuationChar = 
    anyOf('!','"','#','$','%','&','\'','(',')','[',']','{','}','*','+',',','-','.',':',';','/','<','>','=','?','@','\\','^','_','`','|','~')

  /** Parses a transition (rule).
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#transitions]].
   */  
  val transition = (punctuationChar min 4) ~ ws ~ eol ~ guard(blankLine) ^^^ Rule()  
    
  /** Parses a single paragraph. Everything between two blank lines that is not
   *  recognized as a special reStructuredText block type will be parsed as a regular paragraph.
   */
  def paragraph: Parser[Paragraph] = 
      ((not(blankLine) ~> restOfLine) +) ^^ { lines => Paragraph(parseInline(lines mkString "\n")) }
  

  /** Parses a section header with both overline and underline.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#sections]].
   */
  def headerWithOverline: Parser[Block] = {
    (punctuationChar take 1) >> { start =>
      val char = start.charAt(0)
      anyOf(char) >> { deco =>
        val len = deco.length + 1
        (ws ~ eol ~> (anyBut('\n') max len) <~ 
         ws ~ eol ~ (anyOf(char) take len) ~
         ws ~ eol) ^^ { title => DecoratedHeader(OverlineAndUnderline(char), parseInline(title.trim)) }
      }
    }
  }
  
  /** Parses a section header with an underline, but no overline.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#sections]].
   */
  def headerWithUnderline: Parser[Block] = {
    (anyBut(' ') take 1) ~ restOfLine >> { case char ~ rest =>
      val title = (char + rest).trim
      (punctuationChar take 1) >> { start =>
        val char = start.charAt(0)
        ((anyOf(char) min (title.length - 1)) ~
         ws ~ eol) ^^ { _ => DecoratedHeader(Underline(char), parseInline(title)) }
      }
    }
  }
  
  /** Parses a doctest block. This is a feature which is very specific to the
   *  world of Python where reStructuredText originates. Therefore the resulting
   *  `DoctestBlock` tree element is not part of the standard Laika tree model.
   *  When this block type is used the corresponding special renderers must 
   *  be enabled (e.g. the `ExtendedHTML` renderer for HTML).
   *  
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#doctest-blocks]]
   */
  def doctest: Parser[Block] = ">>> " ~> restOfLine ~ 
      ((not(blankLine) ~> restOfLine) *) ^^ { case first ~ rest => DoctestBlock((first :: rest) mkString "\n") }
  
  
  /** Parses a block quote with an optional attribution. 
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#block-quotes]]
   */
  def blockQuote: Parser[Block] = {
    
    import scala.math._
    
    val attributionStart = "---" | "--" | '\u2014' // em dash
        
    def attribution (indent: Int) = (ws take indent) ~ attributionStart ~ (ws max 1) ~> 
      indentedBlock(minIndent = indent, endsOnBlankLine = true) ^^ { block => 
      parseInline(block.lines mkString "\n")
    }
      
    guard(ws take 1) ~> indentedBlock(firstLineIndented = true, linePredicate = not(attributionStart)) >> { 
      block => opt(opt(blankLines) ~> attribution(block.minIndent)) ^^ { 
        spans => QuotedBlock(parseNestedBlocks(block), spans.getOrElse(Nil)) 
      }
    }
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
    val litBlock = (literalBlock | defaultBlock) <~ opt(blankLines)  
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
    def parse (p: Parser[Block], in: Input): ParseResult[List[Block]] = p(in) match {
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
  
  /** Parses a literal block, either quoted or indented.
   *  Only used when the preceding block ends with a double colon (`::`).
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#literal-blocks]]
   */
  def literalBlock: Parser[Block] = {
    val indented = indentedBlock(firstLineIndented = true) ^^ 
      { block => LiteralBlock(block.lines mkString "\n") }
    val quoted = block(guard(punctuationChar min 1), guard(punctuationChar min 1), failure("blank line always ends quoted block")) ^^ 
      { lines => LiteralBlock(lines mkString "\n") }  
    indented | quoted
  }
  
  def nonRecursiveBlock: Parser[Block] = comment | paragraph
  
  protected def prepareBlockParsers (nested: Boolean) = 
    bulletList :: 
    enumList :: 
    fieldList ::
    lineBlock ::
    optionList ::
    explicitBlockItem ::
    gridTable ::
    simpleTable ::
    doctest ::
    blockQuote ::
    headerWithOverline ::
    transition ::
    headerWithUnderline ::
    definitionList ::
    paragraph ::
    Nil
 
  
}