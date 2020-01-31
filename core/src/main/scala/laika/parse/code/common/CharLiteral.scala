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

package laika.parse.code.common

import laika.ast.{CategorizedCode, CodeSpan, CodeSpans, ~}
import laika.parse.code.{CodeCategory, CodeSpanParser}
import laika.parse.text.PrefixedParser
import laika.parse.text.TextParsers._

/** Configurable base parsers for character literals.
  * 
  * @author Jens Halm
  */
object CharLiteral {

  /** Configurable base parsers for character literals.
    */
  case class CharParser(delim: Char,
                        embedded: Seq[CodeSpanParser] = Nil) extends CodeSpanParser {
    
    private val categories: Set[CodeCategory] = Set(CodeCategory.CharLiteral)

    /** Embeds the specified parsers for child spans inside a character literal.
      * 
      * This is usually used for allowing escape sequences inside the literal.
      */
    def embed(childSpans: CodeSpanParser*): CharParser = {
      copy(embedded = embedded ++ childSpans)
    }

    lazy val parsers: Seq[PrefixedParser[CategorizedCode]] = CodeSpanParser(delim) {

      def plainChar(char: String) = anyBut('\'', '\n').take(1).map(CodeSpan(_, categories))
      val delimParser = anyOf(delim).take(1).map(CodeSpan(_, categories))

      (delim ~> lookAhead(any.take(1))).flatMap { char =>
        (EmbeddedCodeSpans.parserMap(embedded).getOrElse(char.head, plainChar(char)) ~ delimParser).map { 
          case span ~ delim => 
            val codeSpans = delim +: CodeSpans.extract(categories)(span) :+ delim
            CodeSpans.merge(codeSpans) 
        }
      }

    }.parsers

  }

  /** Parses a standard character literal enclosed by single quotes.
    */
  def standard: CharParser = CharParser('\'')
  
}
