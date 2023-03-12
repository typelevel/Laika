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

package laika.parse.combinator

import laika.parse._
import laika.parse.text.TextParsers

import scala.util.{ Try, Failure => TFailure, Success => TSuccess }

/** Generic base parsers which are not specifically tailored for parsing of text markup.
  *
  * @author Jens Halm
  */
trait Parsers {

  /** A parser for an optional element that always succeeds.
    *
    * If the underlying parser succeeds this parser will contain its result as a `Some`,
    * if it fails this parser will succeed with a `None`.
    */
  def opt[T](p: Parser[T]): Parser[Option[T]] = Parser { in =>
    p.parse(in) match {
      case Success(result, next) => Success(Some(result), next)
      case _                     => Success(None, in)
    }
  }

  /** A parser for an optional literal string that always succeeds.
    *
    * If the underlying parser succeeds this parser will contain its result as a `Some`,
    * if it fails this parser will succeed with a `None`.
    */
  def opt(value: String): Parser[Option[String]] = opt(TextParsers.literal(value))

  /** A parser that only succeeds if the specified parser fails and
    *  vice versa, it never consumes any input.
    */
  def not[T](p: Parser[T]): Parser[Unit] = Parser { in =>
    p.parse(in) match {
      case Success(_, _) => Failure(Message.ExpectedFailure, in)
      case _             => Success((), in)
    }
  }

  /** A parser that only succeeds if the parsing the specified literal value
    * fails and vice versa, it never consumes any input.
    */
  def not(value: String): Parser[Unit] = not(TextParsers.literal(value))

  /**  Applies the specified parser at the current
    *  position. Never consumes any input.
    */
  def lookAhead[T](p: Parser[T]): Parser[T] = Parser { in =>
    p.parse(in) match {
      case Success(s1, _) => Success(s1, in)
      case e              => e
    }
  }

  /** Attempts to parse the specified literal value at the current
    * position. Never consumes any input.
    */
  def lookAhead(value: String): Parser[String] = lookAhead(TextParsers.literal(value))

  /**  Applies the specified parser at the specified offset behind the current
    *  position. Never consumes any input.
    */
  def lookAhead[T](offset: Int, p: Parser[T]): Parser[T] = {
    val errMsg: Int => Message = Message.forRuntimeValue[Int] { o =>
      s"Unable to look ahead with offset $o"
    }
    Parser { in =>
      if (in.offset - offset < 0) Failure(errMsg(offset), in)
      p.parse(in.consume(offset)) match {
        case Success(s1, _) => Success(s1, in)
        case e              => e
      }
    }
  }

  /** Attempts to parse the specified literal value at the specified offset
    * behind the current position. Never consumes any input.
    */
  def lookAhead(offset: Int, value: String): Parser[String] =
    lookAhead(offset, TextParsers.literal(value))

  /** Applies the specified parser at the specified offset behind the current
    *  position. Never consumes any input.
    */
  def lookBehind[T](offset: Int, parser: => Parser[T]): Parser[T] = {
    val errMsg: Int => Message = Message.forRuntimeValue[Int] { o =>
      s"Unable to look behind with offset $o"
    }
    Parser { in =>
      if (in.offset - offset < 0) Failure(errMsg(offset), in)
      else
        parser.parse(in.consume(-offset)) match {
          case Success(result, _) => Success(result, in)
          case e                  => e
        }
    }
  }

  /** A parser that always succeeds with the specified value.
    */
  def success[T](v: T) = Parser { in => Success(v, in) }

  /** A parser that always fails with the specified message.
    */
  def failure(msg: String) = Parser { in => Failure(Message.fixed(msg), in) }

  /** A parser that succeeds if the specified parser succeeds and all input has been consumed.
    */
  def consumeAll[T](p: Parser[T]): Parser[T] = Parser[T] { in =>
    p.parse(in) match {
      case s @ Success(_, next) =>
        if (next.atEnd) s
        else Failure(Message.ExpectedEOF, next)
      case e                    => e
    }
  }

  /** Constructs a parser lazily, useful when breaking circles in recursive parsing.
    */
  def lazily[T](p: => Parser[T]): Parser[T] = {
    lazy val p0 = p
    Parser[T] { in => p0.parse(in) }
  }

  case class ParserException(result: Failure) extends RuntimeException(result.toString)

  /** Provides additional methods to `Try` via implicit conversion.
    */
  implicit class TryOps[A](t: Try[A]) {

    /** Converts this instance to an `Either[String, A]`, using the message of the `Throwable`
      *  as the value for the `Left` instance.
      */
    def toStringEither: Either[String, A] = t match {
      case TSuccess(res) => Right(res)
      case TFailure(e)   => Left(e.getMessage)
    }

  }

}

object Parsers extends Parsers
