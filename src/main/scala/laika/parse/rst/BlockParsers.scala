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
import laika.parse.rst.Elements._
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

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
trait BlockParsers extends BlockBaseParsers 
                      with ListParsers 
                      with TableParsers 
                      with ExplicitBlockParsers { self: InlineParsers =>

                        
  /** Parses a full document and applies the default rules for resolving
   *  all kinds of references like footnotes, citations, link references,
   *  text roles and substitutions.
   */                      
  override def document = super.document ^^ RewriteRules.applyDefaults
  
  
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
         ws ~ eol) ^^ { title => SectionHeader(char, true, parseInline(title.trim)) }
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
         ws ~ eol) ^^ { _ => SectionHeader(char, false, parseInline(title)) }
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
    
    /* The implementation of this parser is complex as we cannot use the varIndentedBlock parser
     * used in all other parsers for indented blocks because the attribution needs to be detected
     * as flushed left which depends on the current minimum indentation. Parts of the code below
     * are basically a dynamic re-implementation of the varIndentedBlock parser.
     */
    def attributionStart (minIndent: Int) = (ws take minIndent) ~ ("---" | "--" | '\u2014') // em dash
        
    def attribution (indent: Int) = attributionStart(indent) ~> 
      fixedIndentedBlock(indent, success(()), failure("block ends with blank line")) ^^ { block => 
      parseInline(block.lines mkString "\n")
    }
      
    def blocks (blocks: Seq[Block], attr: Option[Seq[Span]]) = {
      if (attr.isEmpty || attr.get.isEmpty) blocks
      else {
        blocks match {
          case SpanSequence(content,opt) :: Nil => Paragraph(content,opt) :: Nil
          case other => other
        }
      }
    }
    
    guard(ws take 1) ~> (Parser { in =>
    
      val curNestLevel = nestLevel(in)
      
      val indent = ((ws min 1) ^^ {_.length})
      
      def line (minIndent: Int) = ((not(attributionStart(minIndent)) ~> indent ~ restOfLine) ^^ 
        { case indent ~ text => (indent, text.trim) }) | (not(eof) ~ blankLine ^^^ (Int.MaxValue, ""))
  
      def nextParser (oldParser: Parser[(Int,String)], oldIndent: Int, newIndent: Int) = 
        if (newIndent < oldIndent) (newIndent, line(newIndent))
        else (oldIndent, oldParser)
        
      def result (indent: Int, lines: List[(Int,String)]): List[String] = 
        lines map (line => if (line._1 == Int.MaxValue) line._2 else " " * (line._1 - indent) + line._2)
        
      val elems = new ListBuffer[(Int,String)]
  
      @tailrec def parse (in0: Input, p: Parser[(Int,String)], minIndent: Int): ParseResult[IndentedBlock] =
        p(in0) match {
          case Success((indent, text), rest) => 
            elems += ((indent, text))
            val (newMin, newParser) = nextParser(p, minIndent, indent)
            parse(rest, newParser, newMin)
          case ns: NoSuccess => Success(IndentedBlock(curNestLevel, minIndent, result(minIndent, elems toList)), in0)
        }
  
      parse(in, line(Int.MaxValue), Int.MaxValue)
      
    } >> { block =>
      opt(attribution(block.minIndent)) ^^ { spans => QuotedBlock(blocks(parseNestedBlocks(block),spans), spans.getOrElse(Nil)) }
    }) 
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
    val defaultBlock = parser <~ opt(blankLines)
    val litBlock = literalBlock | defaultBlock 
    val elems = new ListBuffer[Block]

    def processLiteralMarker (par: Paragraph) = {
      par.content.lastOption match {
        case Some(Text(text,opt)) if text.trim.endsWith("::") => 
          val drop = if (text.length > 2 && text.charAt(text.length-3) == ' ') 3 else 1
          val spans = par.content.init.toList ::: List(Text(text.dropRight(drop),opt))
          (Paragraph(spans,par.options), litBlock)
        case _ => (par, defaultBlock) 
      }
    }
    def toLinkId (h: SectionHeader) = {
      def flattenText (spans: Seq[Span]): String = ("" /: spans) {
        case (res, Text(text,_)) => res + text
        case (res, sc: SpanContainer[_]) => res + flattenText(sc.content)
        case (res, _) => res
      }
      flattenText(h.content).replaceAll("[^a-zA-Z0-9]+","-").replaceFirst("^-","").replaceFirst("-$","").toLowerCase
    }
    def result = {
      case class FinalBlock (options: Options = NoOpt) extends Block
      elems += FinalBlock()
      val processed = elems.toList.sliding(2).foldLeft(new ListBuffer[Block]()) {
        case (buffer, (it: InternalLinkTarget) :: (rt: InternalLinkTarget) :: Nil) => 
          buffer += IndirectLinkDefinition(it.id, LinkReference(Nil, rt.id, "`" + it.id + "`_"))
        case (buffer, (it: InternalLinkTarget) :: (et: ExternalLinkDefinition) :: Nil) => 
          buffer += et.copy(id = it.id)
        case (buffer, (h @ SectionHeader(_,_,_,_)) :: _) => 
          buffer += InternalLinkTarget(toLinkId(h)) += h  
        case (buffer, other :: _) => 
          buffer += other
        case (buffer, _) => buffer // just to avoid the warnings
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
    val indented = varIndentedBlock(firstLineIndented = true) ^^ 
      { block => LiteralBlock(block.lines mkString "\n") }
    val quoted = block(guard(punctuationChar min 1), guard(punctuationChar min 1), failure("blank line always ends quoted block")) ^^ 
      { lines => LiteralBlock(lines mkString "\n") }  
    indented | quoted
  }
  
  def nonRecursiveBlock: Parser[Block] = comment | paragraph
  
  def topLevelBlock: Parser[Block] = bulletList | 
                                     enumList | 
                                     fieldList | 
                                     lineBlock |
                                     optionList | 
                                     explicitBlockItem |
                                     gridTable |
                                     simpleTable |
                                     doctest |
                                     blockQuote |
                                     headerWithOverline |
                                     transition |
                                     headerWithUnderline |
                                     definitionList |
                                     paragraph
 
  def nestedBlock: Parser[Block] = topLevelBlock
  
  
}