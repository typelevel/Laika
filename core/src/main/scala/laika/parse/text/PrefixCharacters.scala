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
import cats.data.NonEmptySet

/** A variant of the Characters type that can be used as a stable prefix for an optimized
  * span parser as it is always non-empty. It's created by the `oneOf` method in `TextParsers`
  * and usually not used directly.
  * 
  * @author Jens Halm
  */
class PrefixCharacters[T] (val underlying: Characters[T], val startChars: NonEmptySet[Char]) extends PrefixedParser[T] {
  
  private def greaterThanZero(count: Int): Unit = 
    require(count > 0, "count must be positive for an optimizable prefix, use TextParsers.anyOf or anyIn if you need to allow empty sequences")
  
  /** Creates and returns a new parser that fails if it does not consume the specified minimum number
    *  of characters. It may still consume more characters in case of further matches.
    */
  def min (count: Int): PrefixCharacters[T] = {
    greaterThanZero(count)
    new PrefixCharacters[T](underlying.min(count), startChars)
  }

  /** Creates and returns a new parser that consumes at most the specified maximum number of characters.
    *  Always succeeds, unless a minimum number of matches is also specified.
    */
  def max (count: Int): PrefixCharacters[T] = {
    greaterThanZero(count)
    new PrefixCharacters[T](underlying.max(count), startChars)
  }

  /** Creates and returns a new parser that consumes exactly the specified number of characters.
    *  Fails if there are less matches, but succeeds in case there are more matches, simply ignoring them.
    *  Calling `take 3` for example is equivalent to calling `min 3 max 3`.
    */
  def take (count: Int): PrefixCharacters[T] = {
    greaterThanZero(count)
    new PrefixCharacters(underlying.take(count), startChars)
  }

  /** Creates and returns a new parser that does not produce a string result, but instead
    * only the number of characters successfully parsed as an `Int`.
    */
  def count: PrefixCharacters[Int] = new PrefixCharacters(underlying.count, startChars)

  /** Creates and returns a new parser that does not produce a result, but instead
    * only consumes the number of characters successfully parsed.
    */
  override def void: PrefixCharacters[Unit] = new PrefixCharacters(underlying.void, startChars)
  
}
