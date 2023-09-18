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
import laika.ast.CodeSpan
import laika.parse.code.CodeCategory
import laika.parse.builders.*
import laika.parse.syntax.*
import laika.parse.text.{ CharGroup, PrefixCharacters, PrefixedParser }
import laika.parse.Parser

/** Configurable base parsers for number literals.
  *
  * @author Jens Halm
  */
object NumberLiteral {

  /** Parsers for common sets of digits, like hex or decimal. */
  object digits {
    val binary: PrefixCharacters[String]  = someOf('0', '1')
    val octal: PrefixCharacters[String]   = someOf(CharGroup.octalDigit)
    val decimal: PrefixCharacters[String] = someOf(CharGroup.digit)

    val decimalNonZero: PrefixCharacters[String] = someOf(
      NonEmptySet.fromSetUnsafe(CharGroup.digit - '0')
    )

    val hex: PrefixCharacters[String] = someOf(CharGroup.hexDigit)
  }

  /** Common suffixes for number literal denoting the number type.
    */
  object suffix {
    val float: Parser[String]     = oneOf('f', 'F', 'd', 'D')
    val long: Parser[String]      = oneOf('l', 'L')
    val bigInt: Parser[String]    = oneOf('n')
    val imaginary: Parser[String] = oneOf('j', 'J')
  }

  private val sign: Parser[String] = anyOf('-', '+').max(1)

  private val exponent: Parser[String] =
    (oneOf('E', 'e') ~ sign ~ digits.decimal.min(1)).source

  private val binaryExponent: Parser[String] =
    (oneOf('P', 'p') ~ sign ~ digits.decimal.min(1)).source

  private def zeroPrefix(char1: Char, char2: Char): PrefixedParser[String] =
    ("0" ~ oneOf(char1, char2)).source

  /* Configurable base parser for number literals. */
  class NumericParser private[NumberLiteral] (
      digits: NonEmptySet[Char],
      prefix: Option[PrefixedParser[String]] = None,
      underscores: Boolean = false,
      exponent: Option[Parser[String]] = None,
      suffix: Option[Parser[String]] = None,
      withFollowingLetter: Boolean = false
  ) extends CodeParserBase {

    private val emptyString: Parser[String] = success("")

    /** Accepts underscores as visual separators in a number literal, as in `12_045`. */
    def withUnderscores: NumericParser =
      new NumericParser(digits, prefix, underscores = true, exponent, suffix, withFollowingLetter)

    /** Accepts a prefix before a number literal, e.g. the `0x` preceding hex numbers in many languages. */
    def withPrefix(parser: PrefixedParser[String]): NumericParser =
      new NumericParser(digits, Some(parser), underscores, exponent, suffix, withFollowingLetter)

    /** Accepts a suffix after a number literal, usually to denote a concrete number type as in `123L`. */
    def withSuffix(parser: Parser[String]): NumericParser =
      new NumericParser(digits, prefix, underscores, exponent, Some(parser), withFollowingLetter)

    /** Parses the exponent part of the number literal, e.g. e+1.5. */
    def withExponent(parser: Parser[String]): NumericParser =
      new NumericParser(digits, prefix, underscores, Some(parser), suffix, withFollowingLetter)

    /** Allows a letter to follow immediately after the number literal,
      * but without being parsed as part of the literal itself.
      * This allows to support use cases like CSS length values: `1.2em`.
      */
    def allowFollowingLetter: NumericParser =
      new NumericParser(digits, prefix, underscores, exponent, suffix, withFollowingLetter = true)

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
            val withDot    = (digitParser ~ oneOf('.') ~ digitParser ~ optExp).source
            val withoutDot = (digitParser ~ exp).source
            withDot | withoutDot
          }
        }

        val optSuffix     = suffix.fold(emptyString)(opt(_).source)
        val postCondition =
          if (withFollowingLetter) success(()) else nextNot(java.lang.Character.isLetter(_))

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
  val binary: NumericParser =
    new NumericParser(NonEmptySet.of('0', '1'), prefix = Some(zeroPrefix('b', 'B')))

  /** Parses an octal number literal.
    * It must start with  `0o` or `0O`, followed by one or more octal digits,
    * e.g. `\0o257`.
    */
  val octal: NumericParser =
    new NumericParser(CharGroup.octalDigit, prefix = Some(zeroPrefix('o', 'O')))

  /** Parses a hexadecimal number literal.
    * It must start with  `0x` or `0X`, followed by one or more hex digits,
    * e.g. `\0x25ff7`.
    */
  val hex: NumericParser =
    new NumericParser(CharGroup.hexDigit, prefix = Some(zeroPrefix('x', 'X')))

  /** Parses a decimal integer.
    */
  val decimalInt: NumericParser = new NumericParser(
    CharGroup.digit
  ) // TODO - prevent zero followed by more digits

  /** Parses a decimal float with an optional exponent.
    */
  val decimalFloat: NumericParser = new NumericParser(CharGroup.digit, exponent = Some(exponent))

  /** Parses a hexadecimal float literal.
    * It must start with  `0x` or `0X`, followed by one or more hex digits,
    * e.g. `\0x25ff7.fa`.
    */
  val hexFloat: NumericParser = new NumericParser(
    CharGroup.hexDigit,
    prefix = Some(zeroPrefix('x', 'X')),
    exponent = Some(binaryExponent)
  )

  /** Creates a new numeric parser for the specified set of digit characters.
    * Optional prefixes and suffixes can be specified with the API of the returned `NumericParser`.
    */
  def forDigits(digits: NonEmptySet[Char]): NumericParser = new NumericParser(digits)

}
