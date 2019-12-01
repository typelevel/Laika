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

import laika.ast.Path.Root
import laika.ast.{Path, ~}
import laika.config._
import laika.parse.text.Characters
import laika.parse.text.TextParsers._
import laika.parse.{Failure, Message, Parser, ParserContext, Success}

import scala.util.Try

/** The parser implementation for the HOCON format.
  * 
  * It currently supports the full spec as documented in
  * [[https://github.com/lightbend/config/blob/master/HOCON.md]]
  * except for file includes which might be added in a later release.
  * 
  * @author Jens Halm
  */
object HoconParsers {
  
  val consumeAllInput: Parser[Unit] = Parser { in =>
    Success((), in.consume(in.remaining))
  }

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
  
  implicit class ClosingParserOps[T] (parser: Parser[T]) {

    def closeWith[R >: T] (closingParser: Parser[Any],
                           msg: => String)
                          (captureError: (T, Failure) => R): Parser[R] = 
      closeWith[R](closingParser, anyBut('\n') ^^^ 0, msg)(captureError)
    
    def closeWith[R >: T] (closingParser: Parser[Any],
                           fallbackParser: Parser[Int],
                           msg: => String)
                           (captureError: (T, Failure) => R): Parser[R] = {

      (parser ~ (closingParser.^^^((0,())) | fallbackParser.withContext)).map {
        case res ~ ((cnt: Int, ctx: ParserContext)) =>
          captureError(res, Failure(Message.fixed(msg), ctx.consume(cnt)))
        case res ~ _ =>
          res
      }
    }
    
  }
  
  def failWith[T](fallbackParser: Parser[Int],
                  msg: => String)
                 (captureError: Failure => T): Parser[T] = {

    fallbackParser.withContext.map { case(cnt, ctx) =>
      captureError(Failure(Message.fixed(msg), ctx.consume(cnt)))
    }
    
  }
 
  case class PathFragments(fragments: Seq[StringBuilderValue]) {
    def join(other: PathFragments): PathFragments = {
      if (fragments.isEmpty || other.fragments.isEmpty) PathFragments(fragments ++ other.fragments)
      else {
        val glue = (fragments.last, other.fragments.head) match {
          case (ValidStringValue(v1), ValidStringValue(v2)) => Seq(ValidStringValue(v1 ++ v2))
          case (v1, v2) => Seq(v1, v2)
        }
        PathFragments(fragments.init ++ glue ++ other.fragments.tail)
      }
    }
  }
  object PathFragments {
    def unquoted(key: StringBuilderValue): PathFragments = {
      val fragments = key match {
        case ValidStringValue(value) => value.split("\\.", -1).toSeq.map(ValidStringValue)
        case invalid => Seq(invalid)
      }
      apply(fragments)
    }
    def quoted(key: StringBuilderValue): PathFragments = apply(Seq(key))
    def whitespace(ws: String): PathFragments = apply(Seq(ValidStringValue(ws)))
  } 
  
  def lazily[T](parser: => Parser[T]): Parser[T] = new Parser[T] {
    lazy val p = parser
    override def parse (in: ParserContext) = p.parse(in)
  }
  
  /** Parses whitespace or newline characters. */
  val wsOrNl: Characters[String] = anyOf(' ','\t','\n')

  /** Parses a null value. */
  val nullValue: Parser[ConfigBuilderValue] = "null" ^^^ ResolvedBuilderValue(NullValue)
  /** Parses a literal true value. */
  val trueValue: Parser[ConfigBuilderValue] = "true" ^^^ ResolvedBuilderValue(BooleanValue(true))
  /** Parses a literal false value. */
  val falseValue: Parser[ConfigBuilderValue] = "false" ^^^ ResolvedBuilderValue(BooleanValue(false))

  /** Parses a literal number value into a Long or Double depending on whether a fraction part is present. */
  val numberValue: Parser[ConfigBuilderValue] = {
    
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
      case int ~ ""         => 
        Try(int.toLong).toEither
          .left.map(_.getMessage)
          .map(v => ResolvedBuilderValue(LongValue(v)))
      case int ~ doublePart => 
        Try((int + doublePart).toDouble).toEither
          .left.map(_.getMessage)
          .map(v => ResolvedBuilderValue(DoubleValue(v)))
    }
  }

  /** Parses a string enclosed in quotes. */
  val quotedString: Parser[StringBuilderValue] = {
    val chars = anyBut('"','\\','\n').min(1).map(Right(_))
    val specialChar = anyIn('b','f','n','r','t').take(1).map {
      case "b" => "\b"
      case "f" => "\f"
      case "n" => "\n"
      case "r" => "\r"
      case "t" => "\t"
    }
    val literalChar = anyIn('"','\\','/').take(1)
    val unicode = anyIn('0' to '9', 'a' to 'f', 'A' to 'F').take(4).map(Integer.parseInt(_, 16).toChar.toString)
    val escape = '\\' ~> ((literalChar | specialChar | unicode).map(Right(_)) | any.take(1).withContext.map(Left(_)) )
    
    import cats.implicits._
    
    val value = (chars | escape).rep.map { parts => 
      parts.sequence.fold(
        error => InvalidStringValue(
          parts.map(_.fold(_._1, identity)).mkString, 
          Failure(Message.fixed(s"Invalid escape sequence: \\${error._1}"), error._2)
        ),
        parts => ValidStringValue(parts.mkString)
      )
    }
    ('"' ~> value).closeWith('"', "Expected closing quote")((v,f) => InvalidStringValue(v.value, f))
  }

  /** Parses a string enclosed in triple quotes. */
  val multilineString: Parser[ConfigBuilderValue] = {
    val msg = "Expected closing triple quote"
    val fallback = failWith(any.count, msg)(InvalidBuilderValue(SelfReference,_))
    "\"\"\"" ~> (delimitedBy("\"\"\"").map(ValidStringValue) | fallback)
  }

  /** Parses an unquoted string that is not allowed to contain any of the reserved characters listed in the HOCON spec. */
  def unquotedString(delimiters: Set[Char]): Parser[StringBuilderValue] = {
    val unquotedChar = anyBut('$', '"', '{', '}', '[', ']', ':', '=', ',', '+', '#', '`', '^', '?', '!', '@', '*', '&', '\\', ' ','\t','\n')
    val mainParser = unquotedChar.min(1).map(ValidStringValue)
    val closingParser = if (delimiters.isEmpty) success(()) else lookAhead(ws ~ (anyOf((delimiters + '"' + '$').toSeq:_*).take(1) | unquotedChar.take(1) | eof)) // TODO - empty delimiters are a temp workaround
    val delimMsg = if (delimiters.size == 1) " is" else "s are one of"
    val renderedDelimiters = delimiters.map {
      case '\n' => "'\\n'"
      case '+' => "'+='"
      case c => "'" + c + "'"
    }.mkString(", ")
    val msg = s"Illegal character in unquoted string, expected delimiter$delimMsg $renderedDelimiters"
    mainParser.closeWith[StringBuilderValue](closingParser, ws.count <~ anyBut((delimiters + '\n').toSeq:_*), msg)((v, f) => InvalidStringValue(v.value, f))
  }

  /** Parses any of the 3 string types (quoted, unquoted, triple-quoted). */
  def stringBuilderValue(delimiter: Set[Char]): Parser[ConfigBuilderValue] =
    multilineString | quotedString | unquotedString(delimiter)
  
  def concatenatedValue(delimiter: Set[Char]): Parser[ConfigBuilderValue] = {
    lazy val parts = (ws ~ (not(comment) ~> anyValue(delimiter))).map { case s ~ v => ConcatPart(s,v) }.rep
    lazily {
      (anyValue(delimiter) ~ parts).map {
        case first ~ Nil => first
        case first ~ rest => ConcatValue(first, rest)
      }
    }
  }

  /** Parses a key based on the HOCON rules where a '.' in a quoted string is not interpreted as a path separator. */
  def concatenatedKey(delimiter: Set[Char]): Parser[Either[InvalidStringValue, Path]] = {
    val string = quotedString.map(PathFragments.quoted) | unquotedString(delimiter).map(PathFragments.unquoted)
    val parts = (ws.map(PathFragments.whitespace) ~ string).map { case s ~ fr => s.join(fr) }
    (string ~ parts.rep).map {
      case first ~ rest =>
        val res = (first +: rest).reduce(_ join _)
        val keyStrings = res.fragments.map(_.value)
        res.fragments.toList.collect { case inv: InvalidStringValue => inv } match {
          case error :: _ => Left(InvalidStringValue(keyStrings.mkString("."), error.failure.copy(
            msgProvider = res => "Invalid key: " + error.failure.msgProvider.message(res)))
          )
          case _          => Right(Path(keyStrings.toList))
        }
    }
  }

  /** Parses a substitution variable. */
  val substitutionValue: Parser[ConfigBuilderValue] = {
    val mainParser = ("${" ~> opt('?') ~ concatenatedKey(Set('}'))).map {
      case opt ~ Right(key)  => SubstitutionValue(key, opt.isDefined)
      case _ ~ Left(invalid) => invalid
    }
    def handleError(value: ConfigBuilderValue, failure: Failure): ConfigBuilderValue = value match {
      case inv: InvalidStringValue => inv
      case other => InvalidBuilderValue(other, failure)  
    }
    mainParser.closeWith('}', "Expected closing brace '}'")(handleError)
  }
  
  val include: Parser[ConfigBuilderValue] = {
    
    def resource(kind: String, f: StringBuilderValue => IncludeResource): Parser[IncludeResource] = {
      val noOpeningQuote = failWith(anyBut('\n') ^^^ 0, "Expected quoted string")(InvalidStringValue("",_))
      val resourceId = (quotedString | noOpeningQuote)
        .closeWith(")", "Expected closing parenthesis") { 
          case (inv: InvalidStringValue, _) => inv
          case (v, failure) => InvalidStringValue(v.value, failure)
        }
      ((kind + "(") ~> resourceId).map(f)
    }
    
    val includeResource = quotedString.map(IncludeAny(_)) | 
      resource("file", IncludeFile(_)) | 
      resource("classpath", IncludeClassPath(_)) | 
      resource("url", IncludeUrl(_)) |
      failWith(anyBut('\n') ^^^ 0, "Expected quoted string")(f => IncludeAny(InvalidStringValue("", f)))
    
    val required = "required(" ~> includeResource
      .map(_.asRequired)
      .closeWith(")", "Expected closing parenthesis"){ (v,f) => 
        v.resourceId match {
          case inv: InvalidStringValue => IncludeAny(inv)
          case sv => IncludeAny(InvalidStringValue(sv.value, f))
        }
      }
    
    "include " ~> (required | includeResource).map(IncludeBuilderValue)
  }

  /** Parses a comment. */
  val comment: Parser[String] = ("//" | "#") ~> restOfLine

  /** Parses the rest of the current line if it contains either just whitespace or a comment. */
  val wsOrComment: Parser[Any] = wsOrNl ~ (comment | wsOrNl.min(1)).rep
  
  private val separator: Parser[Any] = (char(',') | eol | comment) ~ wsOrComment
  
  private val trailingComma: Parser[Any] = opt(',' ~ wsOrComment)

  /** Parses an array value recursively. */
  lazy val arrayValue: Parser[ConfigBuilderValue] = {
    lazy val value = wsOrNl ~> concatenatedValue(Set(']',',','\n','#')) <~ ws
    lazy val values = wsOrComment ~> opt(value ~ (separator ~> value).rep).map(_.fold(Seq.empty[ConfigBuilderValue]){ case v ~ vs => v +: vs }) <~ wsOrComment
    val mainParser = lazily(('[' ~> values <~ trailingComma).map(ArrayBuilderValue))
    mainParser.closeWith[ConfigBuilderValue](']', "Expected closing bracket ']'")(InvalidBuilderValue)
  }

  /** Parses the members of an object without the enclosing braces. */
  private[laika] lazy val objectMembers: Parser[ObjectBuilderValue] = {
    val keySeparators = Set(':','=','{','+')
    lazy val key = wsOrNl ~> concatenatedKey(keySeparators) <~ ws
    lazy val value = wsOrNl ~> concatenatedValue(Set('}',',','\n','#')) <~ ws
    lazy val withSeparator = ((anyOf(':','=').take(1) | "+=") ~ value).map {
      case "+=" ~ element => ConcatValue(SelfReference, Seq(ConcatPart("", ArrayBuilderValue(Seq(element))))) 
      case _ ~ v => v 
    }
    lazy val withoutSeparator = ws ~> objectValue <~ ws
    val msg = "Expected separator after key ('=', '+=', ':' or '{')"
    val fallback = failWith(ws.count <~ anyBut('\n'), msg)(InvalidBuilderValue(SelfReference,_))
    val includeField = include.map(BuilderField(Right(Root), _))
    val valueField = (key ~ (withSeparator | withoutSeparator | fallback)).map { case k ~ v => BuilderField(k, v) }
    lazy val member = includeField | valueField
    lazy val members = opt(member ~ (separator ~> member).rep).map(_.fold(Seq.empty[BuilderField]) { case m ~ ms => m +: ms })
    (wsOrComment ~> members <~ wsOrComment <~ trailingComma).map(ObjectBuilderValue)
  }

  /** Parses an object value enclosed in braces. */
  lazy val objectValue: Parser[ConfigBuilderValue] = {
    val mainParser = lazily('{' ~> objectMembers)
    mainParser.closeWith[ConfigBuilderValue]('}', "Expected closing brace '}'")(InvalidBuilderValue)
  }

  /** Parses a root configuration object where the enclosing braces may be omitted. */
  lazy val rootObject: Parser[ObjectBuilderValue] = {
    val recoveredRoot = objectValue.map {
      case obj: ObjectBuilderValue => obj
      case inv => ObjectBuilderValue(Seq(BuilderField("invalid", inv)))
    }
    val withBraces = wsOrComment ~> recoveredRoot <~ wsOrComment
    val withoutBraces = wsOrComment ~> objectMembers <~ wsOrComment
    (withBraces | withoutBraces) <~ eof
  }

  /** Parses any kind of value supported by the HOCON format. */
  def anyValue(delimiter: Set[Char]): Parser[ConfigBuilderValue] = 
    objectValue | 
      arrayValue | 
      numberValue | 
      trueValue | 
      falseValue | 
      nullValue |
      substitutionValue |
      stringBuilderValue(delimiter)
  
}
