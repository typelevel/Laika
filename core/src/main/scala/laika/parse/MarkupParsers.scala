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

import laika.parse.core._
import laika.parse.core.text.Characters
import laika.util.~

/** Base parsers that provide optimized low-level renderers for typical requirements
 *  of text markup parsers. In particular they are meant as an efficient replacement
 *  for scenarios where usually regex parsers are used. In cases where different parsers
 *  need to be tried for relatively short input sequences, regex parsers tend to be less
 *  efficient. Furthermore, these base parsers may also improve readability, as it
 *  allows to combine simple low-level parsers to higher-level parsers based on the
 *  Scala SDK combinator API, instead of producing long regexes which may be hard to read.  
 * 
 *  @author Jens Halm
 */
trait MarkupParsers extends BaseParsers {

  
  /** Implicit conversion that allows to pass a single
   *  character to the range-based `anyIn` parser. 
   */
  implicit def charToTraversable (char: Char): Traversable[Char] = Set(char)


  /** Parses horizontal whitespace (space and tab).
    * Always succeeds, consuming all whitespace found.
    */
  lazy val ws: Characters = anyOf(' ','\t')

  /** Succeeds at the end of a line, including the end of the input.
   *  Produces an empty string as a result and consumes any new line characters.
   */
  val eol: Parser[Unit] = Parser { in =>
    if (in.atEnd) Success((), in)
    else if (in.char == '\n') Success((), in.consume(1))
    else if (in.char == '\r' && in.remaining > 1 && in.charAt(1) == '\n') Success((), in.consume(2))
    else Failure(Message.ExpectedEOL, in)
  }

  val wsEol: Parser[Any] = ws ~ eol
  
  /** Succeeds at the end of the input.
   */
  val eof: Parser[String] = Parser { in =>
    if (in.atEnd) Success("", in)
    else Failure(Message.ExpectedEOF, in)
  }  
  
  /** Succeeds at the start of the input.
   */
  val atStart: Parser[Unit] = Parser { in =>
    if (in.offset == 0) Success(success(()), in) 
    else Failure(Message.ExpectedStart, in)
  }

  /** Parses a simple reference name that only allows alphanumerical characters
   *  and the punctuation characters `-`, `_`, `.`, `:`, `+`.
   */
  val refName: Parser[String] = {
    val alphanum = anyWhile(c => Character.isDigit(c) || Character.isLetter(c)) min 1
    val symbol = anyOf('-', '_', '.', ':', '+') take 1
    
    alphanum ~ ((symbol ~ alphanum)*) ^^ { 
      case start ~ rest => start + (rest map { case a~b => a+b }).mkString
    }
  }

  /** Consumes any kind of input, always succeeds.
   *  This parser would consume the entire input unless a `max` constraint
   *  is specified.
   */
  val any: Characters = Characters.anyWhile(_ => true)
  
  /** Consumes any number of consecutive occurrences of the specified characters.
   *  Always succeeds unless a minimum number of required matches is specified.
   */
  def anyOf (chars: Char*): Characters = Characters.include(chars)
  
  /** Consumes any number of consecutive characters that are not one of the specified characters.
   *  Always succeeds unless a minimum number of required matches is specified.
   */
  def anyBut (chars: Char*): Characters = Characters.exclude(chars)
  
  /** Consumes any number of consecutive characters that are in one of the specified character ranges.
   *  Always succeeds unless a minimum number of required matches is specified.
   */
  def anyIn (ranges: Traversable[Char]*): Characters = Characters.include(ranges.flatten)

  /** Consumes any number of consecutive characters which satisfy the specified predicate.
    *  Always succeeds unless a minimum number of required matches is specified.
    */
  def anyWhile (p: Char => Boolean): Characters = Characters.anyWhile(p)

}

 /** Exception thrown when parsing a text markup document or fragment fails.
  *  This can only happen due to a bug in this library, as the behaviour of the parser
  *  is to treat all unknown or malformed markup as regular text and always succeed.
  *  The result property holds the `NoSuccess` instance that caused the failure.
  */
class MarkupParserException (val result: Failure) extends RuntimeException(result.toString)


class MarkupParser[T] (p: Parser[T]) extends Parser[T] {


  val parser = BaseParsers.consumeAll(p)


  def parse (ctx: ParserContext): Parsed[T] = parser.parse(ctx)


   /** Fully parses the input from the specified reader and returns the result.
    *  This function is expected to always succeed, errors would be considered a bug
    *  in this library, as the parsers treat all unknown or malformed markup as regular
    *  text.
    */
  def parseMarkup (source: String): T = parseMarkup(ParserContext(source))

   /** Fully parses the input from the specified reader and returns the result.
    *  This function is expected to always succeed, errors would be considered a bug
    *  in this library, as the parsers treat all unknown or malformed markup as regular
    *  text.
    */
  def parseMarkup (ctx: ParserContext): T = parser.parse(ctx) match {
    case Success(result, _) => result
    case ns: Failure        => throw new MarkupParserException(ns)
  }


}
