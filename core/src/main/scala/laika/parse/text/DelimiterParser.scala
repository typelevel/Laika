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

package laika.parse.text

import cats.implicits._
import cats.data.NonEmptySet
import laika.parse.{Failure, Message, Parser, Success}

/**
  * @author Jens Halm
  */
class DelimiterParser (prefix: PrefixedParser[String],
                       prevNegatedPredicate: Char => Boolean = _ => false,
                       nextNegatedPredicate: Char => Boolean = _ => false) extends PrefixedParser[String] {
  
  val startChars: NonEmptySet[Char] = prefix.startChars
  
  val underlying: Parser[String] = Parser { in =>
    prefix.parse(in) match {
      case s @ Success(_, next) => 
        if (in.offset > 0 && prevNegatedPredicate(in.charAt(-1)))
          Failure(Message.fixed("delimiter predicate for preceding character not satisfied"), in)
        else if (next.remaining > 0 && nextNegatedPredicate(next.char))
          Failure(Message.fixed("delimiter predicate for following character not satisfied"), in)
        else
          s
      case f: Failure => f
    }
  }
  
  def prevNot (char: Char, chars: Char*): DelimiterParser = prevNot(NonEmptySet.of(char, chars:_*))
  
  def prevNot (chars: NonEmptySet[Char]): DelimiterParser = prevNot(chars.contains(_))
  
  def prevNot (predicate: Char => Boolean): DelimiterParser = new DelimiterParser(prefix, predicate, nextNegatedPredicate)

  def nextNot (char: Char, chars: Char*): DelimiterParser = nextNot(NonEmptySet.of(char, chars:_*))

  def nextNot (chars: NonEmptySet[Char]): DelimiterParser = nextNot(chars.contains(_))

  def nextNot (predicate: Char => Boolean): DelimiterParser = new DelimiterParser(prefix, prevNegatedPredicate, predicate)
  
  def postCondition (parser: Parser[Any]): DelimiterParser = ??? // TODO - implement
  
}
