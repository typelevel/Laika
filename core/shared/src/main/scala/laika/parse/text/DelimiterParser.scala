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

/** A parser that simplifies the expression of typical conditions for start and end
  * delimiters of inline spans.
  * 
  * It allows to specify conditions for the immediately preceding and following character
  * of the parsed delimiter.
  * 
  * As an example, the following parser is only composed of base combinators
  * {{{
  *   lookBehind(1, not(' ')) ~ "**" ~ lookAhead(not(' '))
  * }}}
  * 
  * It looks for the string "**" as a delimiter and does not allow any space character
  * before or after the delimiter
  * 
  * With this API it can be rewritten as:
  * {{{
  *   delimiter("**").prevNot(' ').nextNot(' ')
  * }}}
  * 
  * The only characters that this parser will consume and return as a result
  * are those of the delimiter itself, not the surrounding characters that it validated.
  * 
  * @author Jens Halm
  */
class DelimiterParser (prefix: PrefixedParser[String],
                       prevCharInvalid: Char => Boolean = _ => false,
                       nextCharInvalid: Char => Boolean = _ => false,
                       enclosingCharsInvalid: (Char, Char) => Boolean = (_,_) => false) extends PrefixedParser[String] {
  
  val startChars: NonEmptySet[Char] = prefix.startChars

  private val prevCharInvalidMsg: Char => Message =
    Message.forRuntimeValue[Char]( value => s"delimiter predicate for preceding character '$value' not satisfied" )

  private val nextCharInvalidMsg: Char => Message =
    Message.forRuntimeValue[Char]( value => s"delimiter predicate for following character '$value' not satisfied" )

  private val enclosingCharsInvalidMsg: ((Char, Char)) => Message = Message.forRuntimeValue[(Char, Char)] {
    case (prev, next) => s"delimiter enclosed by invalid pair of characters: '$prev' and '$next'"
  }
  
  val underlying: Parser[String] = Parser { in =>
    prefix.parse(in) match {
      case s @ Success(_, next) => 
        
        if (in.offset > 0 && prevCharInvalid(in.charAt(-1)))
          Failure(prevCharInvalidMsg(in.charAt(-1)), in)
          
        else if (next.remaining > 0 && nextCharInvalid(next.char))
          Failure(nextCharInvalidMsg(next.char), in)
          
        else if (in.offset > 0 && next.remaining > 0 && enclosingCharsInvalid(in.charAt(-1), next.char))
          Failure(enclosingCharsInvalidMsg((in.charAt(-1), next.char)), in)
          
        else
          s
      case f: Failure => f
    }
  }

  /** Ensures that the character immediately preceding the delimiter
    * is not one of the specified characters.
    */
  def prevNot (char: Char, chars: Char*): DelimiterParser = prevNot(NonEmptySet.of(char, chars:_*))

  /** Ensures that the character immediately preceding the delimiter
    * is not in the specified set.
    */
  def prevNot (chars: NonEmptySet[Char]): DelimiterParser = prevNot(chars.contains(_))

  /** Ensures that the character immediately preceding the delimiter
    * does not satisfy the given predicate.
    */
  def prevNot (predicate: Char => Boolean): DelimiterParser = 
    new DelimiterParser(prefix, predicate, nextCharInvalid, enclosingCharsInvalid)

  /** Ensures that the character immediately following the delimiter
    * is not one of the specified characters.
    */
  def nextNot (char: Char, chars: Char*): DelimiterParser = nextNot(NonEmptySet.of(char, chars:_*))

  /** Ensures that the character immediately following the delimiter
    * is not in the specified set.
    */
  def nextNot (chars: NonEmptySet[Char]): DelimiterParser = nextNot(chars.contains(_))

  /** Ensures that the character immediately following the delimiter
    * does not satisfy the given predicate.
    */
  def nextNot (predicate: Char => Boolean): DelimiterParser = 
    new DelimiterParser(prefix, prevCharInvalid, predicate, enclosingCharsInvalid)

  /** Ensures that the characters immediately preceding and following the delimiter
    * do not satisfy the given predicate.
    * 
    * This option exists for the rare occasion where certain kinds of pairs of
    * characters surrounding a delimiter cannot be allowed. This logic would
    * be particularly cumbersome to express with the basic combinators.
    */
  def notEnclosedBy (predicate: (Char, Char) => Boolean): DelimiterParser = 
    new DelimiterParser(prefix, prevCharInvalid, nextCharInvalid, predicate)
  
}
