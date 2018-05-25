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

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * @author Jens Halm
  */
trait RepeatParsers {


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


}
