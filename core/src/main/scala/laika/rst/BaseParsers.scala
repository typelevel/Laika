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

import cats.implicits._
import cats.data.NonEmptySet
import laika.ast._
import laika.parse.Parser
import laika.parse.implicits._
import laika.parse.builders._
import laika.parse.text.{CharGroup, Characters, TextParsers}

/**
  * @author Jens Halm
  */
object BaseParsers {

  /** Set of punctuation characters as supported by transitions (rules) and
    * overlines and underlines for header sections.
    */
  private[laika] val punctuationChars: NonEmptySet[Char] = 
    NonEmptySet.of('!','"','#','$','%','&','\'','(',')','[',']','{','}','*','+',',','-','.',':',';','/','<','>','=','?','@','\\','^','_','`','|','~')

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
    val alphaNum = anyWhile(c => Character.isDigit(c) || Character.isLetter(c)) min 1
    val symbol = oneOf('-', '_', '.', ':', '+')

    (alphaNum ~ (symbol ~ alphaNum).rep).source
  }

  /** Parses a size and its amount, e.g. 12px.
    * The unit is mandatory and not validated.
    */
  val sizeAndUnit: Parser[Size] = {
    val digit = someOf(CharGroup.digit)
    val amount = (digit ~ opt("." ~ digit)).source.map(_.toDouble)
    (amount ~ (ws ~> (simpleRefName | "%"))).mapN(Size)
  }

  /** Parses any of the four supported types of footnote labels.
    *
    *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#footnote-references]].
    */
  val footnoteLabel: Parser[FootnoteLabel] = {
    val decimal = someOf(CharGroup.digit).map(n => NumericLabel(n.toInt))
    val autonumber = literal("#").as(Autonumber)
    val autosymbol = literal("*").as(Autosymbol)
    val autonumberLabel = "#" ~> simpleRefName.map(AutonumberLabel)

    decimal | autonumberLabel | autonumber | autosymbol
  }

}
