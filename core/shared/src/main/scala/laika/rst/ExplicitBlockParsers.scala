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

package laika.rst

import laika.ast._
import laika.bundle.{BlockParser, BlockParserBuilder}
import laika.parse.markup.RecursiveParsers
import laika.parse.builders._
import laika.parse.implicits._
import laika.parse.{Failure, LineSource, Parser, Success}
import laika.rst.ast._
import BaseParsers._
import laika.parse.text.PrefixedParser

/** Provides the parsers for all types of explicit block elements.
 *  In reStructuredText an explicit block element starts with `.. `,
 *  followed by a block where the second and subsequent lines are indented.
 * 
 * @author Jens Halm
 */
class ExplicitBlockParsers (recParsers: RecursiveParsers) {


  import recParsers._

  
  private val explicitStart = ".." ~ ws.min(1)
  
  
  /** Parses all types of explicit block items.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#explicit-markup-blocks]].
   */
  lazy val explicitBlockItem: PrefixedParser[Block] = (explicitStart ~> (footnote | citation | linkTarget | comment)) |
    (".." ~ nextIn('\n') ~> comment)
  

  /** Parses a footnote.
   *
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#footnotes]]. 
   */
  lazy val footnote: Parser[FootnoteDefinition] = {

    def reverseWS: Parser[Int] = Parser { in =>
      ws.parse(in.reverse).map(_.length) match {
        case Success(result, _) => Success(result, in)
        case Failure(msg, _, _) => Failure(msg, in)
      }
    }
    
    val prefix = reverseWS ~ ("[" ~> footnoteLabel <~ "]" ~ ws)
    
    (prefix ~ recursiveBlocks(indentedBlock())).withCursor.map { case (wsCount ~ label ~ content, src) =>
      val bodyInput = if (src.input.lastOption.contains('\n')) src.input.dropRight(1) else src.input
      val reconstructedInput = ".." + (" " * wsCount) + bodyInput
      val refCursor = src match {
        case ls: LineSource if ls.parent.offset >= 2 + wsCount => ls.parent
        case other => other.root
      }
      val reconstructedSource = LineSource(reconstructedInput, refCursor.consume((2 + wsCount) * -1))
      FootnoteDefinition(label, content, reconstructedSource)
    }
  }
  
  /** Parses a citation.
   *
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#citations]]. 
   */
  lazy val citation: Parser[Citation] = {
    val prefix = "[" ~> simpleRefName <~ "]" ~ ws
    
    (prefix ~ recursiveBlocks(indentedBlock())).mapN(Citation(_, _))
  }
  
  /** Parses a link definition, either an internal, external or indirect link.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#hyperlink-targets]].
   */
  lazy val linkTarget: Parser[Block with Span] = {
    
    val named = "_" ~> (("`" ~> escapedUntil('`') <~ ":") | escapedUntil(':')).map { ReferenceName(_).normalized }
      
    val internal = named.map(id => InternalLinkTarget(Id(id)))
    
    val external = {
      val anonymous = literal("__:").as("")
    
      ((anonymous | named) ~ ExplicitBlockParsers.linkDefinitionBody).mapN(LinkDefinition.create(_, _))
    }
    
    val indirect = {
      (named <~ ws) ~ ((opt(eol ~ ws) ~ "`" ~> escapedText(delimitedBy('`')) | simpleRefName) <~ "_" ~ wsEol) ^^ {
        case name ~ refName => LinkAlias(name, refName.replaceAll("\n", "")) 
      }
    }
    
    indirect | external | internal
  }
  
  /** Parses a comment.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#comments]].
   */
  val comment: Parser[Comment] = indentedBlock().map(src => Comment(src.input.trim))
  
}

/** Provides the parsers for all types of explicit block elements.
  * In reStructuredText an explicit block element starts with `.. `,
  * followed by a block where the second and subsequent lines are indented.
  *
  * @author Jens Halm
  */
object ExplicitBlockParsers {

  /** The parser builder for all explicit block items that start with `..` except
    * for directives which are provided by an extension.
    */
  val allBlocks: BlockParserBuilder = BlockParser.recursive { recParsers =>
    new ExplicitBlockParsers(recParsers).explicitBlockItem
  }

  private[rst] lazy val linkDefinitionBody: Parser[String] = {
    val notEmpty = not(blankLine) | lookAhead(restOfLine ~ ws.min(1) ~ not(blankLine))

    (notEmpty ~> indentedBlock()).map {
      _.lines.iterator.map(_.input.trim).mkString
    }
  }

  /**  Parses the short variant of an anonymous link definition
    *  (that starts with `__` instead of `.. __:`)
    *
    *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#anonymous-hyperlinks]].
    */
  lazy val shortAnonymousLinkTarget: BlockParserBuilder = BlockParser.standalone {
    "__ " ~> linkDefinitionBody.map(LinkDefinition.create("", _))
  }

}
