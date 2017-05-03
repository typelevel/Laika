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

package laika.parse.core.text

import laika.parse.core._

import scala.annotation.tailrec

/**
  * @author Jens Halm
  */
class Characters (predicate: Char => Boolean,
                  minChar:   Int = 0,
                  maxChar:   Int = 0) extends Parser[String] {

  /** Creates and returns a new parser that fails if it does not consume the specified minimum number
    *  of characters. It may still consume more characters in case of further matches.
    */
  def min (count: Int): Characters = new Characters(predicate, count, maxChar)

  /** Creates and returns a new parser that consumes at most the specified maximum number of characters.
    *  Always succeeds, unless a minimum number of matches is also specified.
    */
  def max (count: Int): Characters = new Characters(predicate, minChar, count)

  /** Creates and returns a new parser that consumes exactly the specified number of characters.
    *  Fails if there are less matches, but succeeds in case there are more matches, simply ignoring them.
    *  Calling `take 3` for example is equivalent to calling `min 3 max 3`.
    */
  def take (count: Int): Characters = new Characters(predicate, count, count)


  private val msgFunction: Int => String = actual => s"expected at least $minChar characters, got only $actual"

  private def newMessageProvider (actual: Int): MessageProvider = new MessageFunction(actual, msgFunction)


  def parse (ctx: ParserContext): Parsed[String] = {

    val source = ctx.input
    val maxOffset = if (maxChar <= 0 || ctx.offset + maxChar < 0) source.length
                    else Math.min(ctx.offset + maxChar, source.length)

    def result (offset: Int) = {
      val consumed = offset - ctx.offset
      if (consumed >= minChar)
        Success(source.substring(ctx.offset, offset), ctx.consume(consumed))
      else
        Failure(newMessageProvider(consumed), ctx)
    }

    @tailrec
    def parse (offset: Int): Parsed[String] =
      if (offset == maxOffset || !predicate(source.charAt(offset))) result(offset)
      else parse(offset + 1)

    parse(ctx.offset)
  }

}

object Characters {

  /**  Returns an optimized, Array-based lookup function
    *  for the specified characters.
    */
  def optimizedLookup (chars: Iterable[Char]): Array[Byte] = {
    val max = chars.max
    val lookup = new Array[Byte](max + 1)

    for (c <- chars) lookup(c) = 1

    lookup
  }

  def include (chars: Seq[Char]): Characters = {
    val p: Char => Boolean = chars.length match {
      case 0 =>
        c => false
      case 1 =>
        val c = chars(0)
        _ == c
      case 2 =>
        val c1 = chars(0)
        val c2 = chars(1)
        c => c == c1 || c == c2
      case _ =>
        val lookup = optimizedLookup(chars)
        val max = lookup.length - 1
        c:Char => c <= max && lookup(c) == 1
    }
    new Characters(p)
  }

  def exclude (chars: Seq[Char]): Characters = {
    val p: Char => Boolean = chars.length match {
      case 0 =>
        c => true
      case 1 =>
        val c = chars(0)
        _ != c
      case 2 =>
        val c1 = chars(0)
        val c2 = chars(1)
        c => c != c1 && c != c2
      case _ =>
        val lookup = optimizedLookup(chars)
        val max = lookup.length - 1
        c:Char => c > max || lookup(c) == 0
    }
    new Characters(p)
  }

  def anyWhile (predicate: Char => Boolean): Characters = new Characters(predicate)

}
