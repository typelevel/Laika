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

import laika.ast.CodeSpan
import laika.parse.Parser
import laika.parse.code.{CodeCategory, CodeSpanParser, CodeSpanParsers}
import laika.parse.text.TextParsers._

/** Configurable base parsers for identifiers in code blocks.
  * 
  * @author Jens Halm
  */
object Identifier {
  
  /* TODO - support for non-ASCII identifier characters requires changes in the low-level optimizer
     for the span parser. This ASCII-only support will probably already cover a large range of common use cases.
  */
  val idStartChars: Set[Char] = ('a' to 'z').toSet ++ ('A' to 'Z').toSet
  val idPartChars: Set[Char] = ('0' to '9').toSet


  /** Function that applies the `TypeName` category to identifiers starting 
    * with an uppercase letter, and the `Identifier` category to those starting
    * with a lowercase letter.
    */
  val upperCaseTypeName: String => CodeCategory = s => 
    if (s.nonEmpty && s.head.isUpper) CodeCategory.TypeName else CodeCategory.Identifier 
    

  /** Configurable base parser for identifiers in code blocks. */
  case class IdParser(idStartChars: Set[Char], 
                      idNonStartChars: Set[Char], 
                      category: String => CodeCategory = _ => CodeCategory.Identifier,
                      allowDigitBeforeStart: Boolean = false) extends CodeSpanParsers {

    import NumberLiteral._

    /** Applies a function to the parser result to determine the code category.
      */
    def withCategoryChooser(f: String => CodeCategory): IdParser = {
      copy(category = f)
    }

    /** Adds the specified characters to the set of characters allowed to start an identifier.
      * Will also be added to the set of characters for the parser of the rest of the identifier.
      */
    def withIdStartChars(chars: Char*): IdParser = copy(idStartChars = idStartChars ++ chars.toSet)

    /** Adds the specified characters to the set of characters allowed as part of an identifier,
      * but not allowed as the first character.
      */
    def withIdPartChars(chars: Char*): IdParser = copy(idNonStartChars = idNonStartChars ++ chars.toSet)

    // TODO - 0.14 - remove or make private
    private [code] val idRestParser: Parser[String] = anyOf((idStartChars ++ idNonStartChars).toSeq:_*)

    lazy val parsers: Seq[CodeSpanParser] = CodeSpanParsers(idStartChars) {
        
      val prevChar = lookBehind(1, anyWhile(c => (!Character.isDigit(c) || allowDigitBeforeStart) && !Character.isLetter(c)).take(1)) | atStart

      val idStart = any.take(1)
      
      (prevChar ~> idStart ~ idRestParser).concat.map(id => Seq(CodeSpan(id, category(id))))
      
    }.parsers

    // TODO - 0.14 - remove or make private
    private [code] def standaloneParser: Parser[CodeSpan] = {
      val idStart = anyOf(idStartChars.toSeq:_*).take(1)
      (idStart ~ idRestParser).concat.map(id => CodeSpan(id, category(id)))
    }

  }

  /** Parses an alphanumeric identifier; digits are not allowed as start characters.
    * 
    * Other characters like underscore are not allowed by this base parser, but can be added
    * to the returned instance with the `withIdStartChars` or `withIdPartChars` methods.
    */
  def alphaNum: IdParser = IdParser(idStartChars, idPartChars)
  
}
