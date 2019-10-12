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
import laika.parse.{Parser, ParserContext}
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
  
  case class ConcatValue(first: ConfigBuilderValue, rest: Seq[ConcatPart]) extends ConfigBuilderValue
  case class ConcatPart(whitespace: String, value: ConfigBuilderValue)
  case class SubstitutionValue(ref: String, optional: Boolean) extends ConfigBuilderValue // TODO - use Path?
  case object SelfReference extends ConfigBuilderValue
  case class ArrayBuilderValue(values: Seq[ConfigBuilderValue]) extends ConfigValue
  case class ObjectBuilderValue(values: Seq[BuilderField]) extends ConfigBuilderValue
  case class BuilderField(key: String, value: ConfigBuilderValue) // TODO - use Path
  
  sealed trait ConfigValue extends ConfigBuilderValue
  
  case object NullValue extends ConfigValue
  case class BooleanValue(value: Boolean) extends ConfigValue
  case class DoubleValue(value: Double) extends ConfigValue
  case class LongValue(value: Long) extends ConfigValue
  case class StringValue(value: String) extends ConfigValue
  case class ArrayValue(values: Seq[ConfigValue]) extends ConfigValue
  case class ObjectValue(values: Seq[Field]) extends ConfigValue
  case class Field(key: String, value: ConfigValue)
  
  
  def lazily[T](parser: => Parser[T]): Parser[T] = new Parser[T] {
    lazy val p = parser
    override def parse (in: ParserContext) = p.parse(in)
  }
  
  val wsOrNl: Parser[String] = anyOf(' ','\t','\n')
  
  val nullValue: Parser[ConfigValue] = "null" ^^^ NullValue
  val trueValue: Parser[ConfigValue] = "true" ^^^ BooleanValue(true)
  val falseValue: Parser[ConfigValue] = "false" ^^^ BooleanValue(false)
  
  val numberValue: Parser[ConfigValue] = {
    
    val zero = anyIn('0').take(1)
    val digits = anyIn('0' to '9')
    val oneToNine = anyIn('1' to '9')
    val nonZero = (oneToNine.take(1) ~ digits).concat
    val negativeSign = opt('-').map(_.fold("")(_.toString))
    val sign = opt(char('-') | char('+')).map(_.fold("")(_.toString))
    
    val integer = (negativeSign ~ (zero | nonZero)).concat
    val fraction = opt((anyIn('.').take(1) ~ digits).concat).map(_.getOrElse(""))
    val exponent = opt((anyIn('E','e').take(1) ~ sign ~ digits).concat).map(_.getOrElse(""))

    (integer ~ (fraction ~ exponent).concat) ^^? {
      case int ~ ""         => Try(int.toLong).toEither.left.map(_.getMessage).map(LongValue)
      case int ~ doublePart => Try((int + doublePart).toDouble).toEither.left.map(_.getMessage).map(DoubleValue)
    }
  }
  
  val quotedString: Parser[StringValue] = {
    val chars = anyBut('"','\\').min(1)
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
    
    val value = (chars | escape).rep.map(parts => StringValue(parts.mkString))
    '"' ~> value <~ '"'
  }
  
  val multilineString: Parser[StringValue] = {
    "\"\"\"" ~> delimitedBy("\"\"\"").map(StringValue)
  }
  
  val unquotedString: Parser[StringValue] = {
    val unquotedChar = anyBut('$', '"', '{', '}', '[', ']', ':', '=', ',', '+', '#', '`', '^', '?', '!', '@', '*', '&', '\\', ' ','\t','\n').min(1)
    unquotedChar.map(StringValue)
  }
  
  lazy val concatenatedValue: Parser[ConfigBuilderValue] = {
    lazy val parts = (ws ~ anyValue).map { case s ~ v => ConcatPart(s,v) }.rep
    lazily {
      (anyValue ~ parts).map {
        case first ~ Nil => first
        case first ~ rest => ConcatValue(first, rest)
      }
    }
  }

  val concatenatedKey: Parser[String] = {
    val string = (quotedString | unquotedString).map(_.value)
    val parts = (ws ~ string).concat.rep
    (string ~ parts).map {
      case first ~ rest => (first +: rest).mkString // TODO - special handling for . in quoted string
    }
  }
  
  val substitutionValue: Parser[SubstitutionValue] = {
    ("${" ~> opt('?') ~ delimitedBy('}')).map {
      case opt ~ key => SubstitutionValue(key, opt.isDefined)
    } 
  }
  
  val separator: Parser[Any] = char(',') | eol ~ wsOrNl
  
  lazy val arrayValue: Parser[ConfigBuilderValue] = {
    lazy val value = wsOrNl ~> concatenatedValue <~ ws
    lazy val values = wsOrNl ~> opt(value ~ (separator ~> value).rep).map(_.fold(Seq.empty[ConfigBuilderValue]){ case v ~ vs => v +: vs }) <~ wsOrNl
    lazily(('[' ~> values <~ opt(',' ~ wsOrNl) <~ ']').map(ArrayBuilderValue))
  }
  
  private lazy val objectMembers: Parser[ObjectBuilderValue] = {
    lazy val key = wsOrNl ~> concatenatedKey <~ wsOrNl
    lazy val value = wsOrNl ~> concatenatedValue <~ ws
    lazy val withSeparator = ((anyOf(':','=').take(1) | "+=") ~ value).map {
      case "+=" ~ element => ConcatValue(SelfReference, Seq(ConcatPart("", ArrayBuilderValue(Seq(element))))) 
      case _ ~ v => v 
    }
    lazy val withoutSeparator = wsOrNl ~> objectValue <~ wsOrNl
    lazy val member = (key ~ (withSeparator | withoutSeparator)).map { case k ~ v => BuilderField(k, v) }
    lazy val members = opt(member ~ (separator ~> member).rep).map(_.fold(Seq.empty[BuilderField]) { case m ~ ms => m +: ms })
    (wsOrNl ~> members <~ wsOrNl <~ opt(',' ~ wsOrNl)).map(ObjectBuilderValue)
  }
  
  lazy val objectValue: Parser[ObjectBuilderValue] = lazily('{' ~> objectMembers <~ '}')
  
  lazy val rootObject: Parser[ObjectBuilderValue] = {
    val withBraces = wsOrNl ~> objectValue <~ wsOrNl
    val withoutBraces = wsOrNl ~> objectMembers <~ wsOrNl
    (withBraces | withoutBraces) <~ eof
  }
  
  lazy val anyValue: Parser[ConfigBuilderValue] = 
    objectValue | 
      arrayValue | 
      numberValue | 
      trueValue | 
      falseValue | 
      nullValue |
      substitutionValue |
      multilineString | 
      quotedString |
      unquotedString
  
}
