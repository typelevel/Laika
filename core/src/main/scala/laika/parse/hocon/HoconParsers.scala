/*
 * Copyright 2012-2019 the original author or authors.
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

package laika.parse.hocon

import laika.ast.~
import laika.parse.Parser
import laika.parse.text.TextParsers._

import scala.util.Try

/**
  * @author Jens Halm
  */
object HoconParsers {

  // TODO - promote to core parser
  implicit class String2ParserOps (val p: Parser[String ~ String]) extends AnyVal {
    def concat: Parser[String] = p.map { case a ~ b => a + b }
  }
  implicit class String3ParserOps (val p: Parser[String ~ String ~ String]) extends AnyVal {
    def concat: Parser[String] = p.map { case a ~ b ~ c => a + b + c }
  }
  implicit class PrependParserOps[T] (val p: Parser[T ~ Seq[T]]) extends AnyVal {
    def concat: Parser[Seq[T]] = p.map { case x ~ xs => x +: xs }
  }
  
  sealed trait ConfigBuilderValue
  
  case class ConcatValue(values: Seq[ConfigBuilderValue]) extends ConfigBuilderValue
  case class SubstitutionValue(ref: String) extends ConfigBuilderValue // TODO - use Path?
  
  sealed trait ConfigValue extends ConfigBuilderValue
  
  case object NullValue extends ConfigValue
  case class BooleanValue(value: Boolean) extends ConfigValue
  case class DoubleValue(value: Double) extends ConfigValue
  case class LongValue(value: Long) extends ConfigValue
  case class StringValue(value: String) extends ConfigValue
  case class ArrayValue(values: Seq[ConfigValue]) extends ConfigValue
  case class ObjectValue(values: Seq[Field]) extends ConfigValue
  
  case class Field(key: String, value: ConfigValue) // TODO - use Path
  
  val nullValue: Parser[ConfigValue] = "null" ^^^ NullValue
  val trueValue: Parser[ConfigValue] = "true" ^^^ BooleanValue(true)
  val falseValue: Parser[ConfigValue] = "false" ^^^ BooleanValue(false)
  
  val numberValue: Parser[ConfigValue] = {
    
    val zero = anyIn('0').take(1)
    val digits = anyIn('0' to '9')
    val oneToNine = anyIn('1' to '9')
    val nonZero = (oneToNine ~ digits).concat
    val negativeSign = opt('-').map(_.fold("")(_.toString))
    val sign = opt(char('-') | char('+')).map(_.fold("")(_.toString))
    
    val integer = (negativeSign ~ (zero | nonZero)).concat
    val fraction = (anyIn('.').take(1) ~ digits).concat
    val exponent = (anyIn('E','e').take(1) ~ sign ~ digits).concat

    (integer ~ (fraction ~ exponent).concat) ^^? {
      case int ~ ""         => Try(int.toLong).toEither.left.map(_.getMessage).map(LongValue)
      case int ~ doublePart => Try((int + doublePart).toDouble).toEither.left.map(_.getMessage).map(DoubleValue)
    }
  }
  
  val stringValue: Parser[StringValue] = {
    val chars = anyBut('"','\\')
    val specialChar = anyIn('b','f','n','r','t').take(1).map {
      case "b" => "\b"
      case "f" => "\f"
      case "n" => "\n"
      case "r" => "\r"
      case "t" => "\t"
    }
    val literalChar = anyIn('"','\\','/').take(1)
    val unicode = anyIn('0' to '9', 'a' to 'f', 'A' to 'F').take(4).map(Integer.parseInt(_, 16).toChar.toString)
    val escape = '\\' ~> (literalChar | specialChar | unicode)
    
    (chars | escape).rep.map(parts => StringValue(parts.mkString))
  }
  
  lazy val arrayValue: Parser[ConfigValue] = {
    val value = ws ~> anyValue <~ ws
    val values = opt(value ~ (',' ~> value).rep).map(_.fold(Seq.empty[ConfigValue]){ case v ~ vs => v +: vs })
    ('[' ~> values  <~ ']').map(ArrayValue)
  }
  
  lazy val objectValue: Parser[ConfigValue] = {
    val key = ws ~> stringValue <~ ws
    val value = ws ~> anyValue <~ ws
    val member = (key ~ (':' ~> value)).map { case k ~ v => Field(k.value, v) }
    val members = opt(member ~ (',' ~> member).rep).map(_.fold(Seq.empty[Field]){ case m ~ ms => m +: ms })
    ('[' ~> members  <~ ']').map(ObjectValue)
  }
  
  lazy val anyValue: Parser[ConfigValue] = objectValue | arrayValue | numberValue | trueValue | falseValue | nullValue | stringValue
  
}
