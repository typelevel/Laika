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

package laika.internal.rst

import cats.data.NonEmptySet
import laika.ast._
import laika.parse.Parser
import laika.parse.implicits._
import laika.parse.builders._
import laika.parse.text.{ CharGroup, Characters }

/** @author Jens Halm
  */
private[laika] object BaseParsers {

  /** Set of punctuation characters as supported by transitions (rules) and
    * overlines and underlines for header sections.
    */
  val punctuationChars: NonEmptySet[Char] =
    NonEmptySet.of('!', '"', '#', '$', '%', '&', '\'', '(', ')', '[', ']', '{', '}', '*', '+', ',',
      '-', '.', ':', ';', '/', '<', '>', '=', '?', '@', '\\', '^', '_', '`', '|', '~')

  /** Parses punctuation characters as supported by transitions (rules) and
    * overlines and underlines for header sections.
    */
  val punctuationChar: Characters[String] = anyOf(punctuationChars)

  /** Parses a simple reference name that only allows alphanumerical characters
    * and the punctuation characters `-`, `_`, `.`, `:`, `+`.
    *
    * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#reference-names]].
    */
  val simpleRefName: Parser[String] = {
    val alphaNum = someWhile(c => Character.isDigit(c) || Character.isLetter(c))
    val symbol   = oneOf('-', '_', '.', ':', '+').void

    alphaNum.rep(symbol).min(1).source
  }

  /** Parses a size and its unit, e.g. 12px.
    * The unit is mandatory and must be CSS-compatible.
    */
  val sizeAndUnit: Parser[Length] = {
    def evalLength(value: String): Either[String, LengthUnit] =
      LengthUnit.fromString(value).toRight(s"Invalid length unit: $value")
    val digit                                                 = someOf(CharGroup.digit)
    val amount = (digit ~ opt("." ~ digit)).source.map(_.toDouble)
    val unit   = (simpleRefName | "%").evalMap(evalLength)
    (amount ~ (ws ~> unit)).mapN(Length.apply)
  }

  /** Parses any of the four supported types of footnote labels.
    *
    *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#footnote-references]].
    */
  val footnoteLabel: Parser[FootnoteLabel] = {
    val decimal         = someOf(CharGroup.digit).map(n => NumericLabel(n.toInt))
    val autonumber      = literal("#").as(Autonumber)
    val autosymbol      = literal("*").as(Autosymbol)
    val autonumberLabel = "#" ~> simpleRefName.map(AutonumberLabel.apply)

    decimal | autonumberLabel | autonumber | autosymbol
  }

}
