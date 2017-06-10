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
class Repeat[+T] (parser: Parser[T], min: Int = 0) extends Parser[List[T]] {

  def min (num: Int): Repeat[T] = new Repeat(parser, num)

  def parse (ctx: ParserContext): Parsed[List[T]] = {

    val elems = new ListBuffer[T]

    @tailrec
    def rec (ctx: ParserContext): Parsed[List[T]] = parser.parse(ctx) match {
      case Success(x, next)                  => elems += x; rec(next)
      case _: Failure if elems.length >= min => Success(elems.toList, ctx)
      case f: Failure                        => f
    }

    rec(ctx)

  }


}
