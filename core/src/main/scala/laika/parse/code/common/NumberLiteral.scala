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

  implicit class PrependParserOps[T] (val p: Parser[T ~ Seq[T]]) extends AnyVal {
    def concat: Parser[Seq[T]] = p.map { case x ~ xs => x +: xs }
  }

  val binaryDigits: Set[Char] = ('0' to '1').toSet
  val octalDigits: Set[Char] = ('0' to '7').toSet
  val decimalDigits: Set[Char] = ('0' to '9').toSet
  val decimalNonZero: Set[Char] = ('1' to '9').toSet
  val hexDigits: Set[Char] = ('0' to '9').toSet ++ ('a' to 'f').toSet ++ ('A' to 'F').toSet

  val sign: Parser[String] = anyOf('-', '+').max(1)
  val exponent: Parser[String] = opt((anyOf('E', 'e').take(1) ~ sign ~ anyOf(decimalDigits.toSeq: _*).min(1)).concat).map(_.getOrElse(""))
  val binaryExponent: Parser[String] = opt((anyOf('P', 'p').take(1) ~ sign ~ anyOf(decimalDigits.toSeq: _*).min(1)).concat).map(_.getOrElse(""))


  case class NumericParser (startChars: Set[Char],
                            digits: Set[Char],
                            idSequence: Option[Parser[String]] = None,
                            underscores: Boolean = false,
                            suffix: Option[Parser[String]] = None) {

    private val emptyString: Parser[String] = success("")

    def withUnderscores: NumericParser = copy(underscores = true)

    def withSuffix (parser: Parser[String]): NumericParser = copy(suffix = Some(parser))

    def build: CodeSpanParsers = CodeSpanParsers(CodeCategory.NumberLiteral, startChars) {
      
      def parse (chars: Set[Char]): Characters[String] = anyOf(chars.toSeq: _*)
      
      val minDigits = if (idSequence.isEmpty) 0 else 1
      
      val digitSequence =
        if (underscores) parse(digits + '_').min(minDigits) <~ lookBehind(1, not('_'))
        else parse(digits).min(minDigits)

      (idSequence.getOrElse(emptyString) ~ digitSequence ~ suffix.fold(emptyString)(opt(_).map(_.getOrElse("")))).concat
    }

  }

  val binary: NumericParser = NumericParser(Set('0'), binaryDigits, Some(anyOf('b', 'B').take(1)))

  val octal: NumericParser = NumericParser(Set('0'), octalDigits, Some(anyOf('o', 'O').take(1)))

  val hex: NumericParser = NumericParser(Set('0'), hexDigits, Some(anyOf('x', 'X').take(1)))

  val decimalInt: NumericParser = NumericParser(decimalDigits, decimalDigits) // TODO - prevent zero followed by more digits

}

object NumericSuffix {

  val float: Parser[String] = anyOf('f', 'F', 'd', 'D').take(1)
  val long: Parser[String] = anyOf('l', 'L').take(1)
  val bigInt: Parser[String] = anyOf('n').take(1)
  val imaginary: Parser[String] = anyOf('j', 'J').take(1)
  
}
