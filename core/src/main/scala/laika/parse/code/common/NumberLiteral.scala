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
import laika.parse.text.TextParsers._
import laika.parse.text.{CharGroup, PrefixCharacters, PrefixedParser, TextParsers}
import laika.parse.Parser

/** Configurable base parsers for number literals.
  * 
  * @author Jens Halm
  */
object NumberLiteral {

  // TODO - 0.14 - promote to core parser and make them public
  private[laika] implicit class String2ParserOps (val p: Parser[String ~ String]) extends AnyVal {
    def concat: Parser[String] = p.map { case a ~ b => a + b }
  }

  private[laika] implicit class String2PrefixedParserOps (val p: PrefixedParser[String ~ String]) extends AnyVal {
    def concat: PrefixedParser[String] = p.map { case a ~ b => a + b }
  }

  private[laika] implicit class String3ParserOps (val p: Parser[String ~ String ~ String]) extends AnyVal {
    def concat: Parser[String] = p.map { case a ~ b ~ c => a + b + c }
  }

  private[laika] implicit class String3PrefixedParserOps (val p: PrefixedParser[String ~ String ~ String]) extends AnyVal {
    def concat: PrefixedParser[String] = p.map { case a ~ b ~ c => a + b + c }
  }

  private[laika] implicit class String4ParserOps (val p: Parser[String ~ String ~ String ~ String]) extends AnyVal {
    def concat: Parser[String] = p.map { case a ~ b ~ c ~ d => a + b + c + d }
  }

  private[laika] implicit class PrependParserOps[T] (val p: Parser[T ~ Seq[T]]) extends AnyVal {
    def concat: Parser[Seq[T]] = p.map { case x ~ xs => x +: xs }
  }

  private[laika] implicit class PrependPrefixedParserOps[T] (val p: PrefixedParser[T ~ Seq[T]]) extends AnyVal {
    def concat: PrefixedParser[Seq[T]] = p.map { case x ~ xs => x +: xs }
  }

  private[laika] implicit class List2ParsersOps[T] (val p: Parser[Seq[T] ~ Seq[T]]) extends AnyVal {
    def concat: Parser[Seq[T]] = p.map { case x ~ xs => x ++ xs }
  }

  private[laika] implicit class List2PrefixedParsersOps[T] (val p: PrefixedParser[Seq[T] ~ Seq[T]]) extends AnyVal {
    def concat: PrefixedParser[Seq[T]] = p.map { case x ~ xs => x ++ xs }
  }

  /** Parsers for common sets of digits, like hex or decimal. */
  object DigitParsers {
    val binary: PrefixCharacters[String] = prefix('0', '1')
    val octal: PrefixCharacters[String] = prefix(CharGroup.octalDigit)
    val decimal: PrefixCharacters[String] = prefix(CharGroup.digit)
    val decimalNonZero: PrefixCharacters[String] = prefix(NonEmptySet.fromSetUnsafe(CharGroup.digit - '0'))
    val hex: PrefixCharacters[String] = prefix(CharGroup.hexDigit)
  }

  private val sign: Parser[String] = anyOf('-', '+').max(1)
  private val exponent: Parser[String]       = (anyOf('E', 'e').take(1) ~ sign ~ DigitParsers.decimal.min(1)).concat
  private val binaryExponent: Parser[String] = (anyOf('P', 'p').take(1) ~ sign ~ DigitParsers.decimal.min(1)).concat

  private def zeroPrefix(char1: Char, char2: Char): PrefixedParser[String] = ("0" ~ anyOf(char1, char2).take(1)).concat 
  
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
      
      prefix.getOrElse(TextParsers.prefix(firstDigit).take(1)) >> { prefixOrFirstDigit =>
        
        val digitParser = {
          val minDigits = if (prefix.nonEmpty || prefixOrFirstDigit == ".") 1 else 0
          if (underscores) anyOf(digits.add('_')).min(minDigits) <~ lookBehind(1, not('_')) // TODO - 0.14 - add ensurePrevCharNot/ensureNextCharNot with options like in DelimiterParser
          else anyOf(digits).min(minDigits)
        }

        val number = exponent.fold(digitParser) { exp =>
          val optExp = opt(exp).map(_.getOrElse(""))
          if (prefixOrFirstDigit == ".") {
            (digitParser ~ optExp).concat
          }
          else {
            val withDot = (digitParser ~ anyOf('.').take(1) ~ digitParser ~ optExp).concat
            val withoutDot = (digitParser ~ exp).concat
            withDot | withoutDot
          }
        }

        val optSuffix = suffix.fold(emptyString)(opt(_).map(_.getOrElse("")))
        val postCondition = if (allowFollowingLetter) success(()) else not(anyWhile(java.lang.Character.isLetter).take(1)) // TODO - 0.14 - like above

        (number ~ optSuffix <~ postCondition).concat.map { rest => 
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

  val float: Parser[String] = anyOf('f', 'F', 'd', 'D').take(1)
  val long: Parser[String] = anyOf('l', 'L').take(1)
  val bigInt: Parser[String] = anyOf('n').take(1)
  val imaginary: Parser[String] = anyOf('j', 'J').take(1)
  
}
