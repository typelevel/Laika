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

package laika.ast

/** Converts Roman numerals to integers and vice versa.
  *  Since there never have been universally accepted rules for Roman numerals,
  *  the conversion functions do not apply strict error checking, so some unusual or
  *  illegal constructs may be supported, too. They do not prevent using the same
  *  symbol more than three times in a row for example.
  *
  *  @author Jens Halm
  */
object RomanNumerals {

  private case class Symbol(roman: String, value: Int, repeatable: Boolean = false)

  private val symbols = List(
    Symbol("M", 1000, true),
    Symbol("CM", 900),
    Symbol("D", 500),
    Symbol("CD", 400),
    Symbol("C", 100, true),
    Symbol("XC", 90),
    Symbol("L", 50),
    Symbol("XL", 40),
    Symbol("X", 10, true),
    Symbol("IX", 9),
    Symbol("V", 5),
    Symbol("IV", 4),
    Symbol("I", 1, true)
  )

  /** Converts from an integer to Roman numerals.
    *  The integer has to be between 1 and 4999.
    */
  def intToRoman(value: Int): String = {
    require(value > 0 && value < 5000, "Number must be between 1 and 4999")

    symbols.foldLeft((value, "")) { case ((remaining, result), symbol) =>
      val occurrences = remaining / symbol.value
      (remaining - occurrences * symbol.value, result + symbol.roman * occurrences)
    }._2
  }

  /** Converts from Roman numerals to integer.
    */
  def romanToInt(roman: String): Either[String, Int] = {

    def convert(roman: String, lastSymbol: Symbol): Either[String, Int] = {
      symbols.filter(roman startsWith _.roman).sortBy(-_.roman.length) match {
        case (s @ Symbol(romanSymbol, value, repeatable)) :: _ =>
          if (s == lastSymbol && !repeatable) Left(s"Symbol $romanSymbol cannot be repeated")
          else if (value > lastSymbol.value)
            Left(s"Illegal ordering of symbols: ${lastSymbol.roman}$romanSymbol")
          else convert(roman.substring(romanSymbol.length), s).map(_ + value)
        case Nil if roman.isEmpty                              => Right(0)
        case Nil => Left(s"Illegal Roman Numeral: $roman")
      }
    }

    convert(roman, symbols.head)
  }

}
