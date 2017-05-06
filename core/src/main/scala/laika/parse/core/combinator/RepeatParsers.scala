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

package laika.parse.core.combinator

import laika.parse.core._
import laika.parse.core.combinator.Parsers._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * @author Jens Halm
  */
trait RepeatParsers {


  // TODO - unify these all into a single implementation


  /** A parser generator for repetitions.
    *
    *  `rep(p)` repeatedly uses `p` to parse the input until `p` fails
    *  (the result is a List of the consecutive results of `p`).
    *
    * @param p a `Parser` that is to be applied successively to the input
    * @return A parser that returns a list of results produced by repeatedly applying `p` to the input.
    */
  def rep[T](p: Parser[T]): Parser[List[T]] = rep1(p) | success(List())

  /** A parser generator for non-empty repetitions.
    *
    *  `rep1(p)` repeatedly uses `p` to parse the input until `p` fails -- `p` must succeed at least
    *             once (the result is a `List` of the consecutive results of `p`)
    *
    * @param p a `Parser` that is to be applied successively to the input
    * @return A parser that returns a list of results produced by repeatedly applying `p` to the input
    *        (and that only succeeds if `p` matches at least once).
    */
  def rep1[T](p: Parser[T]): Parser[List[T]] = rep1(p, p)

  /** A parser generator for non-empty repetitions.
    *
    *  `rep1(f, p)` first uses `f` (which must succeed) and then repeatedly
    *     uses `p` to parse the input until `p` fails
    *     (the result is a `List` of the consecutive results of `f` and `p`)
    *
    * @param first a `Parser` that parses the first piece of input
    * @param rest a `Parser` that is to be applied successively to the rest of the input (if any) -- evaluated at most once, and only when necessary
    * @return A parser that returns a list of results produced by first applying `f` and then
    *         repeatedly `p` to the input (it only succeeds if `f` matches).
    */
  def rep1[T](first: Parser[T], rest: Parser[T]): Parser[List[T]] = Parser { in =>
    val elems = new ListBuffer[T]

    def continue(in: ParserContext): Parsed[List[T]] = {
      @tailrec def applyp(in0: ParserContext): Parsed[List[T]] = rest.parse(in0) match {
        case Success(x, next) => elems += x ; applyp(next)
        case _                => Success(elems.toList, in0)
      }

      applyp(in)
    }

    first.parse(in) match {
      case Success(x, rest) => elems += x ; continue(rest)
      case f: Failure       => f
    }
  }

  /** A parser generator for a specified number of repetitions.
    *
    *  `repN(n, p)` uses `p` exactly `n` time to parse the input
    *  (the result is a `List` of the `n` consecutive results of `p`).
    *
    * @param p   a `Parser` that is to be applied successively to the input
    * @param num the exact number of times `p` must succeed
    * @return    A parser that returns a list of results produced by repeatedly applying `p` to the input
    *        (and that only succeeds if `p` matches exactly `n` times).
    */
  def repN[T](num: Int, p: Parser[T]): Parser[List[T]] =
  if (num == 0) success(Nil) else Parser { in =>
    val elems = new ListBuffer[T]

    @tailrec def applyp(in0: ParserContext): Parsed[List[T]] =
      if (elems.length == num) Success(elems.toList, in0)
      else p.parse(in0) match {
        case Success(x, rest) => elems += x ; applyp(rest)
        case f: Failure       => f
      }

    applyp(in)
  }

  /** A parser generator for repetitions where all subsequent parsers
    *  after the first depend on the result of the previous.
    *
    *  @param first the parser to use for the first piece of input
    *  @param next a function that determines the next parser based on the result of the previous
    */
  def rep[T] (first: Parser[T], next: T => Parser[T]): Parser[List[T]] = Parser { in =>
    val elems = new ListBuffer[T]

    @tailrec
    def parse (input: ParserContext, p: Parser[T]): Parsed[List[T]] =
      p.parse(input) match {
        case Success(result, rest) =>
          elems += result
          val newParser = next(result)
          parse(rest, newParser)
        case _: Failure => Success(elems.toList, input)
      }

    parse(in, first)
  }

  /**  Uses the parser for at least the specified number of repetitions or otherwise fails.
    *  Continues to apply the parser after the minimum has been reached until if fails.
    *  The result is the list of results from applying the parser repeatedly.
    */
  def repMin[T] (num: Int, p: Parser[T]): Parser[List[T]] = Parser { in =>
    val elems = new ListBuffer[T]

    @tailrec
    def parse (input: ParserContext): Parsed[List[T]]  = p.parse(input) match {
      case Success(x, rest)                    => elems += x ; parse(rest)
      case _: Failure if elems.length >= num   => Success(elems.toList, input)
      case f: Failure                          => f
    }

    parse(in)
  }

  /** Uses the parser for at most the specified number of repetitions, always succeeds.
    *  The result is the list of results from applying the parser repeatedly.
    */
  def repMax[T] (num: Int, p: Parser[T]): Parser[List[T]] =
    if (num == 0) success(Nil) else Parser { in =>
      val elems = new ListBuffer[T]

      @tailrec
      def parse (input: ParserContext): Parsed[List[T]] =
        if (elems.length == num) Success(elems.toList, input)
        else p.parse(input) match {
          case Success(x, rest)   => elems += x ; parse(rest)
          case _: Failure         => Success(elems.toList, input)
        }

      parse(in)
    }


}
