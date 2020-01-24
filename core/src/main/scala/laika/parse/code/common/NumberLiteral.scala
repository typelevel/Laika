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

import laika.ast.~
import laika.parse.Parser
import laika.parse.code.{CodeCategory, CodeSpanParsers}
import laika.parse.text.Characters
import laika.parse.text.TextParsers._

/**
  * @author Jens Halm
  */
object NumberLiteral {

  // TODO - 0.14 - promote to core parser
  implicit class String2ParserOps (val p: Parser[String ~ String]) extends AnyVal {
    def concat: Parser[String] = p.map { case a ~ b => a + b }
  }

  implicit class String3ParserOps (val p: Parser[String ~ String ~ String]) extends AnyVal {
    def concat: Parser[String] = p.map { case a ~ b ~ c => a + b + c }
  }

  implicit class String4ParserOps (val p: Parser[String ~ String ~ String ~ String]) extends AnyVal {
    def concat: Parser[String] = p.map { case a ~ b ~ c ~ d => a + b + c + d }
  }

  implicit class PrependParserOps[T] (val p: Parser[T ~ Seq[T]]) extends AnyVal {
    def concat: Parser[Seq[T]] = p.map { case x ~ xs => x +: xs }
  }

  object Digits {
    val binary: Set[Char] = ('0' to '1').toSet
    val octal: Set[Char] = ('0' to '7').toSet
    val decimal: Set[Char] = ('0' to '9').toSet
    val decimalNonZero: Set[Char] = ('1' to '9').toSet
    val hex: Set[Char] = ('0' to '9').toSet ++ ('a' to 'f').toSet ++ ('A' to 'F').toSet
  }

  object DigitParsers {
    val binary: Characters[String] = anyOf(Digits.binary.toSeq:_*)
    val octal: Characters[String] = anyOf(Digits.octal.toSeq:_*)
    val decimal: Characters[String] = anyOf(Digits.decimal.toSeq:_*)
    val decimalNonZero: Characters[String] = anyOf(Digits.decimalNonZero.toSeq:_*)
    val hex: Characters[String] = anyOf(Digits.hex.toSeq:_*)
  }

  private val sign: Parser[String] = anyOf('-', '+').max(1)
  val exponent: Parser[String]       = (anyOf('E', 'e').take(1) ~ sign ~ anyOf(Digits.decimal.toSeq: _*).min(1)).concat
  val binaryExponent: Parser[String] = (anyOf('P', 'p').take(1) ~ sign ~ anyOf(Digits.decimal.toSeq: _*).min(1)).concat


  case class NumericParser (startChars: Set[Char],
                            digits: Set[Char],
                            idSequence: Option[Parser[String]] = None,
                            underscores: Boolean = false,
                            exponent: Option[Parser[String]] = None,
                            suffix: Option[Parser[String]] = None,
                            allowFollowingLetter: Boolean = false) {

    private val emptyString: Parser[String] = success("")

    def withUnderscores: NumericParser = copy(underscores = true)

    def withSuffix (parser: Parser[String]): NumericParser = copy(suffix = Some(parser))

    def build: CodeSpanParsers = CodeSpanParsers(CodeCategory.NumberLiteral, startChars) {
      
      lookBehind(1, any.take(1)) >> { startChar =>
        
        val digitParser = {
          def parse (chars: Set[Char]): Characters[String] = anyOf(chars.toSeq: _*)
          val minDigits = if (idSequence.nonEmpty || startChar == ".") 1 else 0  
          if (underscores) parse(digits + '_').min(minDigits) <~ lookBehind(1, not('_'))
          else parse(digits).min(minDigits)
        }
  
        val number = exponent.fold(digitParser) { exp =>
          val optExp = opt(exp).map(_.getOrElse(""))
          if (startChar == ".") {
            (digitParser ~ optExp).concat
          }
          else {
            val withDot = (digitParser ~ anyOf('.').take(1) ~ digitParser ~ optExp).concat
            val withoutDot = (digitParser ~ exp).concat
            withDot | withoutDot
          }
        }
        
        val id = idSequence.getOrElse(emptyString)
        val optSuffix = suffix.fold(emptyString)(opt(_).map(_.getOrElse("")))
        val postCondition = if (allowFollowingLetter) success(()) else not(anyWhile(java.lang.Character.isLetter).take(1)) // TODO - add char(Char => Boolean)
        
        (id ~ number ~ optSuffix <~ postCondition).concat
      }
    }

  }

  val binary: NumericParser = NumericParser(Set('0'), Digits.binary, Some(anyOf('b', 'B').take(1)))

  val octal: NumericParser = NumericParser(Set('0'), Digits.octal, Some(anyOf('o', 'O').take(1)))

  val hex: NumericParser = NumericParser(Set('0'), Digits.hex, Some(anyOf('x', 'X').take(1)))

  val decimalInt: NumericParser = NumericParser(Digits.decimal, Digits.decimal) // TODO - prevent zero followed by more digits
  
  val decimalFloat: NumericParser = NumericParser(Digits.decimal + '.', Digits.decimal, exponent = Some(exponent))
  
  val hexFloat: NumericParser = NumericParser(Set('0'), Digits.hex, Some(anyOf('x', 'X').take(1)), exponent = Some(binaryExponent))

}

object NumericSuffix {

  val float: Parser[String] = anyOf('f', 'F', 'd', 'D').take(1)
  val long: Parser[String] = anyOf('l', 'L').take(1)
  val bigInt: Parser[String] = anyOf('n').take(1)
  val imaginary: Parser[String] = anyOf('j', 'J').take(1)
  
}
