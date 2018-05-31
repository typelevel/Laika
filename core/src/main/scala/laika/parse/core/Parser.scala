/*
 * Copyright 2013-2017 the original author or authors.
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

package laika.parse.core

import laika.parse.core.combinator.Repeat
import laika.util.~

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**  The abstract base for all parser implementations.
  *
  *  Contains the main `parse` function as well as various
  *  combinator function to create a new parser based on this one.
  *
  *  @author Jens Halm
  */
abstract class Parser[+T] {


  import laika.parse.core.combinator.Parsers._


  /** Parses the string content in the specified context
    * and returns the result.
    *
    * This is the only abstract method in `Parser` that
    * concrete implementations need to implement.
    */
  def parse (in: ParserContext): Parsed[T]

  /** Parses the specified string and returns the result.
    */
  def parse (in: String): Parsed[T] = parse(ParserContext(in))


  /** Builds a new parser by applying the specified function
    * to the result of this parser and subsequently applying
    * the parser returned by that function to the input left
    * over by this parser.
    */
  def flatMap[U] (f: T => Parser[U]): Parser[U] = Parser { in =>
    parse(in) match {
      case Success(result, next) => f(result).parse(next)
      case e: Failure => e
    }
  }

  /** Builds a new parser by applying the specified function
    * to the result of this parser.
    */
  def map[U](f: T => U): Parser[U] = Parser { in => parse(in).map(f) }


  /**  Applies the specified parser when this parser fails.
    *
    *  `a orElse b` succeeds if either of the parsers succeeds.
    *
    *  Implementation note:
    *  The parameter is by-name to allow the definition of
    *  recursive parsers. In contrast to the former SDK
    *  parser combinators this is the only place where
    *  a parser with a by-name parameter is used whereas
    *  in all other places the additional cost is avoided.
    */
  def orElse[U >: T] (p0: => Parser[U]): Parser[U] = {
    lazy val alt = p0
    Parser { in =>
      parse(in) orElse alt.parse(in)
    }
  }

  /**  Applies the specified parser to the input left over by this parser
    *  and combines the two results.
    *
    *  `a ~ b` only succeeds if both parsers succeed, with the results
    *  in a wrapper class named `~` for convenient pattern matching:
    *
    *  {{{
    *    a ~ b ~ c ^^ {
    *      case a ~ b ~ c => processResult(a, b, c)
    *    }
    *  }}}
    */
  def ~ [U] (p: Parser[U]): Parser[T ~ U] = Parser { ctx =>
    parse(ctx) match {
      case Success(aRes, next) => p.parse(next).map(bRes => new ~(aRes, bRes))
      case f: Failure => f
    }
  }

  /**  Applies the specified parser to the input left over by this parser,
    *  but only keeps the right result.
    *
    *  `a ~> b` only succeeds if both parsers succeed.
    */
  def ~> [U] (p: Parser[U]): Parser[U] = Parser { ctx =>
    parse(ctx) match {
      case Success(_, next) => p.parse(next)
      case f: Failure => f
    }
  }

  /**  Applies the specified parser to the input left over by this parser,
    *  but only keeps the left result.
    *
    *  `a <~ b` only succeeds if both parsers succeed.
    */
  def <~ [U] (p: Parser[U]): Parser[T] = Parser { ctx =>
    parse(ctx) match {
      case Success(aRes, next) => p.parse(next).map(_ => aRes)
      case f: Failure => f
    }
  }

  /**  Applies the specified parser when this parser fails.
    *
    *  `a | b` succeeds if either of the parsers succeeds.
    *
    *  Implementation note:
    *  The parameter is by-name to allow the definition of
    *  recursive parsers. In contrast to the former SDK
    *  parser combinators this is the only place where
    *  a parser with a by-name parameter is used whereas
    *  in all other places the additional cost is avoided.
    */
  def | [U >: T] (p: => Parser[U]): Parser[U] = orElse(p)

  /**  A synonym for `map`, allowing the grammar to be declared in a concise way.
    */
  def ^^ [U](f: T => U): Parser[U] = map(f)

  /**  Returns a parser that ignores the result of this parser (if it succeeds)
    *  and returns the specified result instead.
    *
    *  Subclasses may override this method to avoid any expensive
    *  result processing.
    */
  def ^^^ [U] (v: => U): Parser[U] =  new Parser[U] {
    lazy val v0 = v
    def parse (in: ParserContext) = Parser.this.parse(in) map (x => v0)
  }

  /**  Returns a parser that applies a partial function to the result of this parser.
    *
    *  `p ^? f` succeeds if `p` succeeds and `f` is defined at the result of `p`,
    *  In that case it returns `f` applied to the result of `p`.
    *
    * @param f a partial function that will be applied to this parser's result.
    * @param error an optional function that takes the same argument as `f` and produces an error message.
    */
  def ^? [U](f: PartialFunction[T, U],
             error: T => String = r => s"Constructor function not defined at $r"): Parser[U] = {
    val msg: T => Message = Message.forRuntimeValue(error)
    Parser { in =>
      parse(in) match {
        case Success(result, next) =>
          if (f.isDefinedAt(result)) Success(f(result), next)
          else Failure(msg(result), next)
        case f: Failure => f
      }


    }
  }

  /**  Returns a parser that applies a function to the result of this parser producing an `Either`
    *  where `Left` is interpreted as failure. It is an alternative to `^?` for scenarios
    *  where the conditional check cannot be easily performed in a pattern match.
    *
    *  `p ^^? f` succeeds if `p` succeeds and `f` returns a `Right` when applied to the result
    *  of `p`.
    */
  def ^^? [U] (f: T => Either[String, U]): Parser[U] = Parser { in =>

    parse(in) match {
      case Success(result, next) => f(result) fold (msg => Failure(Message.fixed(msg),in), res => Success(res,next))
      case f: Failure => f
    }

  }

  /** Operator synonym for `flatMap`.
    */
  def >>[U] (fq: T => Parser[U]) = flatMap(fq)

  /**  Returns a parser that repeatedly applies this parser.
    *  The returned parser offers an API to specify further constraints
    *  like `min` or `max`.
    */
  def rep = new Repeat(this)

  /**  Returns a parser that repeatedly applies this parser.
    *  It will always succeed, potentially with an empty list as the result.
    */
  def * = new Repeat(this)

  /** Returns a parser that repeatedly applies this parser (at least once).
    */
  def + = rep.min(1)

  /** Returns a parser that optionally parses what this parser parses.
    */
  def ? = opt(this)

  /** Returns a parser that invokes the specified function repeatedly,
    * passing the result of this parser if it succeeds, to produce new
    * parsers that get applied until one of them fails.
    *
    * The result of the returned parser is a list containing the
    * result of this parser (if it succeeds) plus the results of
    * successful invocations of the parsers returned by the specified
    * function.
    */
  def repWith[U >: T] (next: U => Parser[U]): Parser[List[U]] = Parser { in =>
    val elems = new ListBuffer[U]

    @tailrec
    def parse (input: ParserContext, p: Parser[U]): Parsed[List[U]] =
      p.parse(input) match {
        case Success(result, rest) =>
          elems += result
          val newParser = next(result)
          parse(rest, newParser)
        case _: Failure => Success(elems.toList, input)
      }

    parse(in, this)
  }

  /**  Changes the failure message produced by a parser.
    */
  def withFailureMessage (msg: String) = Parser { in =>
    parse(in) match {
      case Failure(_, next) => Failure(Message.fixed(msg), next)
      case other            => other
    }
  }

}

/** Companion factory for creating new parser instances.
  */
object Parser {

  /** Builds a new parser based on the specified function
    * that implements the behaviour of the parser.
    */
  def apply[T] (f: ParserContext => Parsed[T]): Parser[T]
    = new Parser[T] { def parse (ctx: ParserContext) = f(ctx) }

}