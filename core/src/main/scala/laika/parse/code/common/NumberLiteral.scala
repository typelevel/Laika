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
import laika.ast.{CodeSpan, ~}
import laika.parse.code.CodeCategory
import laika.parse.api._
import laika.parse.implicits._
import laika.parse.text.{CharGroup, PrefixCharacters, PrefixedParser}
import laika.parse.Parser

/** Configurable base parsers for number literals.
  * 
  * @author Jens Halm
  */
object NumberLiteral {

  /** Parsers for common sets of digits, like hex or decimal. */
  object DigitParsers {
    val binary: PrefixCharacters[String]         = someOf('0', '1')
    val octal: PrefixCharacters[String]          = someOf(CharGroup.octalDigit)
    val decimal: PrefixCharacters[String]        = someOf(CharGroup.digit)
    val decimalNonZero: PrefixCharacters[String] = someOf(NonEmptySet.fromSetUnsafe(CharGroup.digit - '0'))
    val hex: PrefixCharacters[String]            = someOf(CharGroup.hexDigit)
  }

  private val sign: Parser[String] = anyOf('-', '+').max(1)
  private val exponent: Parser[String]       = (oneOf('E', 'e') ~ sign ~ DigitParsers.decimal.min(1)).source
  private val binaryExponent: Parser[String] = (oneOf('P', 'p') ~ sign ~ DigitParsers.decimal.min(1)).source

  private def zeroPrefix(char1: Char, char2: Char): PrefixedParser[String] = ("0" ~ oneOf(char1, char2)).source 
  
  /* Configurable base parser for number literals. */
  case class NumericParser (digits: NonEmptySet[Char],
                            prefix: Option[PrefixedParser[String]] = None,
                            underscores: Boolean = false,
                            exponent: Option[Parser[String]] = None,
                            suffix: Option[Parser[String]] = None,
                            allowFollowingLetter: Boolean = false) extends CodeParserBase {

    private val emptyString: Parser[String] = success("")

    /** Accepts underscores as visual separators in a number literal, as in `12_045`. */
    def withUnderscores: NumericParser = copy(underscores = true)

    /** Accepts a suffix after a number literal, usually to denote a concrete number type as in `123L`. */
    def withSuffix (parser: Parser[String]): NumericParser = copy(suffix = Some(parser))

    lazy val underlying: PrefixedParser[Seq[CodeSpan]] = {
      
      def firstDigit = if (exponent.isDefined) digits.add('.') else digits
      
      prefix.getOrElse(oneOf(firstDigit)) >> { prefixOrFirstDigit =>
        
        val digitParser = {
          val minDigits = if (prefix.nonEmpty || prefixOrFirstDigit == ".") 1 else 0
          if (underscores) anyOf(digits.add('_')).min(minDigits) <~ prevNot('_')
          else anyOf(digits).min(minDigits)
        }

        val number = exponent.fold(digitParser) { exp =>
          val optExp = opt(exp).source
          if (prefixOrFirstDigit == ".") {
            (digitParser ~ optExp).source
          }
          else {
            val withDot = (digitParser ~ oneOf('.') ~ digitParser ~ optExp).source
            val withoutDot = (digitParser ~ exp).source
            withDot | withoutDot
          }
        }

        val optSuffix = suffix.fold(emptyString)(opt(_).source)
        val postCondition = if (allowFollowingLetter) success(()) else nextNot(java.lang.Character.isLetter(_))

        (number ~ optSuffix <~ postCondition).source.map { rest => 
          Seq(CodeSpan(prefixOrFirstDigit + rest, CodeCategory.NumberLiteral))
        }
      }
    }

  }

  /** Parses a binary number literal.
    * It must start with  `0b` or `0B`, followed by one or more binary digits,
    * e.g. `\0b100110`.
    */
  val binary: NumericParser = NumericParser(NonEmptySet.of('0','1'), prefix = Some(zeroPrefix('b', 'B')))

  /** Parses an octal number literal.
    * It must start with  `0o` or `0O`, followed by one or more octal digits,
    * e.g. `\0o257`.
    */
  val octal: NumericParser = NumericParser(CharGroup.octalDigit, prefix = Some(zeroPrefix('o', 'O')))

  /** Parses a hexadecimal number literal.
    * It must start with  `0x` or `0X`, followed by one or more hex digits,
    * e.g. `\0x25ff7`.
    */
  val hex: NumericParser = NumericParser(CharGroup.hexDigit, prefix = Some(zeroPrefix('x', 'X')))

  /** Parses a decimal integer.
    */
  val decimalInt: NumericParser = NumericParser(CharGroup.digit) // TODO - prevent zero followed by more digits

  /** Parses a decimal float with an optional exponent. 
    */
  val decimalFloat: NumericParser = NumericParser(CharGroup.digit, exponent = Some(exponent))

  /** Parses a hexadecimal float literal.
    * It must start with  `0x` or `0X`, followed by one or more hex digits,
    * e.g. `\0x25ff7.fa`.
    */
  val hexFloat: NumericParser = NumericParser(CharGroup.hexDigit, 
    prefix = Some(zeroPrefix('x', 'X')), 
    exponent = Some(binaryExponent)
  )

}

/** Common suffixes for number literal denoting the number type.
  */
object NumericSuffix {

  val float: Parser[String] = oneOf('f', 'F', 'd', 'D')
  val long: Parser[String] = oneOf('l', 'L')
  val bigInt: Parser[String] = oneOf('n')
  val imaginary: Parser[String] = oneOf('j', 'J')
  
}
