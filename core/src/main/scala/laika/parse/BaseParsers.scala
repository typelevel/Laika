/*
 * Copyright 2013-2016 the original author or authors.
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

package laika.parse

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.Parsers
import scala.util.Try
import scala.util.{Success => TSuccess}
import scala.util.{Failure => TFailure}

/**
 * Generic base parsers which are not specifically tailored for parsing of text markup.
 * 
 * @author Jens Halm
 */
trait BaseParsers extends Parsers {

  
  /** A parser generator for repetitions where all subsequent parsers
   *  after the first depend on the result of the previous.
   * 
   *  @param first the parser to use for the first piece of input
   *  @param next a function that determines the next parser based on the result of the previous
   */
  def rep[T] (first: => Parser[T], next: T => Parser[T]): Parser[List[T]] = Parser { in =>
    val elems = new ListBuffer[T]
  
    @tailrec 
    def parse (input: Input, p: Parser[T]): ParseResult[List[T]] =
      p(input) match {
        case Success(result, rest) => 
          elems += result
          val newParser = next(result)
          parse(rest, newParser)
        case ns: NoSuccess => Success(elems.toList, input)
      }

    parse(in, first)
  }
  
  /** Uses the parser for at least the specified number of repetitions or otherwise fails. 
   *  Continues to apply the parser after the minimum has been reached until if fails.
   *  The result is the list of results from applying the parser repeatedly.
   */
  def repMin[T] (num: Int, p: => Parser[T]): Parser[List[T]] = Parser { in =>
    val elems = new ListBuffer[T]
    lazy val parser = p

    @tailrec 
    def parse (input: Input): ParseResult[List[T]]  = parser(input) match {
      case Success(x, rest)                         => elems += x ; parse(rest)
      case ns: NoSuccess if elems.length >= num   => Success(elems.toList, input)
      case ns: NoSuccess                            => ns
    }

    parse(in)
  }
  
  /** Uses the parser for at most the specified number of repetitions, always succeeds. 
   *  The result is the list of results from applying the parser repeatedly.
   */
  def repMax[T] (num: Int, p: => Parser[T]): Parser[List[T]] =
    if (num == 0) success(Nil) else Parser { in =>
      val elems = new ListBuffer[T]
      val parser = p

      @tailrec 
      def parse (input: Input): ParseResult[List[T]] =
        if (elems.length == num) Success(elems.toList, input)
        else parser(input) match {
          case Success(x, rest)   => elems += x ; parse(rest)
          case ns: NoSuccess      => Success(elems.toList, input)
        }

      parse(in)
    }

  /** Applies the specified parser at the specified offset behind the current
   *  position. Never consumes any input.
   */
  def lookBehind [T] (offset: Int, parser: => Parser[T]): Parser[T] = Parser { in =>
    if (in.offset - offset < 0) Failure("Unable to look behind with offset "+offset, in)
    else parser(in.drop(-offset)) match {
      case Success(result, _) => Success(result, in)
      case NoSuccess(msg, _)  => Failure(msg, in)
    }
  }
  
  
  
  /** Provides additional combinator methods to parsers via implicit conversion.
   */
  implicit class ParserOps [A] (parser: Parser[A]) {
    
    /** A parser combinator that applies a function to the result producing an `Either`
     *  where `Left` is interpreted as failure. It is an alternative to `^?` for scenarios 
     *  where the conditional check cannot be easily performed in a pattern match.
     *
     *  `p ^^? f` succeeds if `p` succeeds and `f` returns a `Right`; 
     *  it returns the content of `Right` obtained from applying `f` to the result of `p`.
     *
     *  @param f a function that will be applied to this parser's result.
     *  @return a parser that has the same behaviour as the current parser, but whose result is
     *         transformed by `f`.
     */
    def ^^? [B] (f: A => Either[String,B]): Parser[B] = Parser { in =>
      
      parser(in) match {
        case Success(result, next) => f(result) fold (msg => Failure(msg,in), res => Success(res,next))
        case ns: NoSuccess => ns
      }
        
    }
  }
    
  /** Provides additional methods to `Try` via implicit conversion.
   */
  implicit class TryOps [A] (t: Try[A]) {
    
    /** Converts this instance to an `Either[String, A]`, using the message of the `Throwable`
     *  as the value for the `Left` instance.
     */
    def toStringEither: Either[String,A] = t match {
      case TSuccess(res) => Right(res)
      case TFailure(e)   => Left(e.getMessage)
    }
    
  }
  
  
}



