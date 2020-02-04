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

import laika.ast.{CodeSpan, CodeSpans, ~}
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
                        embedded: Seq[CodeSpanParser] = Nil) extends CodeParserBase {
    
    private val categories: Set[CodeCategory] = Set(CodeCategory.CharLiteral)

    /** Embeds the specified parsers for child spans inside a character literal.
      * 
      * This is usually used for allowing escape sequences inside the literal.
      */
    def embed(childSpans: CodeSpanParser*): CharParser = {
      copy(embedded = embedded ++ childSpans)
    }

    lazy val underlying: PrefixedParser[Seq[CodeSpan]] = {

      def plainChar(char: String) = oneNot('\'', '\n').map(CodeSpan(_, categories))
      val delimParser = oneOf(delim).map(CodeSpan(_, categories))

      (delim ~> lookAhead(oneChar)).flatMap { char =>
        (PrefixedParser.mapAndMerge(embedded.flatMap(_.parsers)).getOrElse(char.head, plainChar(char)) ~ delimParser).map { 
          case span ~ delimSpan => 
            val codeSpans = delimSpan +: CodeSpans.extract(categories)(span) :+ delimSpan
            CodeSpans.merge(codeSpans) 
        }
      }

    }

  }

  /** Parses a standard character literal enclosed by single quotes.
    */
  def standard: CharParser = CharParser('\'')
  
}
