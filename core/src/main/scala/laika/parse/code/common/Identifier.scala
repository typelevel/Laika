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

import cats.data.NonEmptySet
import cats.implicits._
import laika.ast.{CategorizedCode, CodeSpan, ~}
import laika.parse.code.{CodeCategory, CodeSpanParser}
import laika.parse.text.TextParsers._
import laika.parse.text.{CharGroup, PrefixedParser}

/** Configurable base parsers for identifiers in code blocks.
  * 
  * @author Jens Halm
  */
object Identifier {
  
  /* TODO - support for non-ASCII identifier characters requires changes in the low-level optimizer
     for the span parser. This ASCII-only support will probably already cover a large range of common use cases.
  */

  /** Function that applies the `TypeName` category to identifiers starting 
    * with an uppercase letter, and the `Identifier` category to those starting
    * with a lowercase letter.
    */
  val upperCaseTypeName: String => CodeCategory = s => 
    if (s.nonEmpty && s.head.isUpper) CodeCategory.TypeName else CodeCategory.Identifier 
    

  /** Configurable base parser for identifiers in code blocks. */
  case class IdParser(idStartChars: NonEmptySet[Char],
                      nonStartChars: NonEmptySet[Char],
                      category: String => CodeCategory = _ => CodeCategory.Identifier,
                      prefixParser: Option[PrefixedParser[String]] = None,
                      allowDigitBeforeStart: Boolean = false) extends PrefixedParser[CodeSpan] with CodeSpanParser {

    /** Applies a function to the parser result to determine the code category.
      */
    def withCategoryChooser(f: String => CodeCategory): IdParser = copy(category = f)

    /** Associates the result with the specified code category.
      */
    def withCategory(category: CodeCategory): IdParser = copy(category = _ => category)

    /** Adds the specified characters to the set of characters allowed to start an identifier.
      * Will also be added to the set of characters for the parser of the rest of the identifier.
      */
    def withIdStartChars(char: Char, chars: Char*): IdParser = 
      copy(idStartChars = idStartChars ++ NonEmptySet.of(char, chars:_*))

    /** Adds the specified characters to the set of characters allowed as part of an identifier,
      * but not allowed as the first character.
      */
    def withIdPartChars(char: Char, chars: Char*): IdParser = 
      copy(nonStartChars = nonStartChars ++ NonEmptySet.of(char, chars:_*))

    /** Adds the specified prefix to this identifier.
      * 
      * The resulting parser will first parse this prefix and subsequently the
      * base parser for this identifier that will still apply the rules
      * for which characters are allowed to start the identifier.
      * 
      * An example would be an identifier like `#foo` in CSS, where `#` is the prefix,
      * and `foo` follows the rules of this identifier parser.
      */
    def withPrefix (parser: PrefixedParser[String]): IdParser = copy(prefixParser = Some(parser))

    lazy val underlying: PrefixedParser[CodeSpan] = {

      val firstChar = oneOf(idStartChars)
      val idStart = prefixParser.fold[PrefixedParser[String]](firstChar)(p => (p ~ firstChar).source)
      val idDelim = delimiter(idStart).prevNot { c =>
        (Character.isDigit(c) && !allowDigitBeforeStart) || Character.isLetter(c)
      }
      val idRest = anyOf(idStartChars ++ nonStartChars)
      
      (idDelim ~ idRest).source.map(id => CodeSpan(id, category(id)))
    }

    override def parsers: Seq[PrefixedParser[CategorizedCode]] = Seq(this)
    override def startChars: NonEmptySet[Char] = underlying.startChars
    
  }

  /** Parses an alphanumeric identifier; digits are not allowed as start characters.
    * 
    * Other characters like underscore are not allowed by this base parser, but can be added
    * to the returned instance with the `withIdStartChars` or `withIdPartChars` methods.
    */
  def alphaNum: IdParser = IdParser(CharGroup.alpha, CharGroup.digit)
  
}
