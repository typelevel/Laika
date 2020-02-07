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
import laika.ast.{Size, ~}
import laika.parse.{Failure, Message, Parser, Success}
import laika.parse.combinator.Parsers
import laika.parse.implicits._

/** Base text parsers that provide optimized low-level parsers for typical requirements
 *  of text markup parsers. In particular they are meant as an efficient replacement
 *  for scenarios where usually regex parsers are used. In cases where different parsers
 *  need to be tried for relatively short input sequences, regex parsers tend to be less
 *  efficient. Furthermore, these base parsers may also improve readability, as it
 *  allows to combine simple low-level parsers to higher-level parsers based on the
 *  Laika combinator API, instead of producing long regexes which may be hard to read.
 * 
 *  @author Jens Halm
 */
trait TextParsers extends Parsers {

  /** Creates a NonEmptySet from a Character range.
    * This set can then be passed to parsers like `anyOf` or `oneOf`
    * which expect a NonEmptySet as a parameter.
    */
  def range(fromChar: Char, toChar: Char): NonEmptySet[Char] = {
    val range = if (fromChar > toChar) toChar to fromChar else fromChar to toChar
    NonEmptySet.of(range.head, range.tail:_*)
  }

  @deprecated("use oneOf(char) or literal(char.toString) to parse a single character", "0.14.0")
  implicit def char (expected: Char): PrefixedParser[Char] = {
    val errMsg: Char => Message = Message.forRuntimeValue[Char] { found => s"'$expected' expected but $found found" }
    val p = Parser { in =>
      if (in.atEnd) Failure(Message.UnexpectedEOF, in)
      else if (in.char == expected) Success(in.char, in.consume(1))
      else Failure(errMsg(in.char), in)
    }
    PrefixedParser(expected)(p)
  }
  
  /**  A parser that matches only the specified literal string.
    *
    *  The method is implicit so that strings can automatically be lifted to their parsers.
    */
  def literal (expected: String): PrefixedParser[String] = Literal(expected)

  /** Parses horizontal whitespace (space and tab).
    * Always succeeds, consuming all whitespace found.
    */
  lazy val ws: Characters[String] = anyOf(' ','\t')

  /** Succeeds at the end of a line, including the end of the input.
   *  Produces an empty string as a result and consumes any new line characters.
   */
  val eol: Parser[Unit] = Parser { in =>
    if (in.atEnd) Success((), in)
    else if (in.char == '\n') Success((), in.consume(1))
    else if (in.char == '\r' && in.remaining > 1 && in.charAt(1) == '\n') Success((), in.consume(2))
    else Failure(Message.ExpectedEOL, in)
  }

  /** Parses any number of whitespace characters followed
    * by a newline character.
    */
  val wsEol: Parser[Unit] = ws.void ~> eol
  
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

  /** Parses a blank line from the current input offset (which may not be at the
    *  start of the line). Fails for lines that contain any non-whitespace character.
    *  Does always produce an empty string as the result, discarding any whitespace
    *  characters found in the line.
    *
    *  Since it also succeeds at the end of the input
    *  it should never be used in the form of `(blankLine *)` or `(blankLine +)`. Use
    *  the `blankLines` parser instead in these cases.
    */
  val blankLine: Parser[String] = wsEol.as("")

  /** Parses one or more blanklines, producing a list of empty strings corresponding
    *  to the number of blank lines consumed.
    */
  val blankLines: Parser[List[String]] = (not(eof) ~> blankLine)+

  /** Parses the rest of the line from the current input offset no matter whether
    *  it consist of whitespace only or some text. Does not include the eol character(s).
    */
  val restOfLine: Parser[String] = anyNot('\n','\r') <~ eol

  /** Parses a single text line from the current input offset (which may not be at the
    *  start of the line). Fails for blank lines. Does not include the eol character(s).
    */
  val textLine: Parser[String] = not(blankLine) ~> restOfLine

  /** Verifies that the previous character is not one of those specified.
    * Succeeds at the start of the input and does not consume any input
    * or produce a result when it succeeds.
    */
  def prevNot (char: Char, chars: Char*): Parser[Unit] = prevNot(NonEmptySet.of(char, chars:_*))

  /** Verifies that the previous character is not one of those specified.
    * Succeeds at the start of the input and does not consume any input
    * or produce a result when it succeeds.
    */
  def prevNot (chars: NonEmptySet[Char]): Parser[Unit] = prevNot(chars.contains(_))

  /** Verifies that the previous character does not satisfy the specified predicate.
    * Succeeds at the start of the input and does not consume any input
    * or produce a result when it succeeds.
    */
  def prevNot (predicate: Char => Boolean): Parser[Unit] = {
    val errMsg: Char => Message = Message.forRuntimeValue[Char] { found =>
      s"previous character '$found' does not satisfy the specified predicate"
    }
    Parser { in =>
      if (in.offset == 0) Success((), in)
      else if (!predicate(in.charAt(-1))) Success((), in)
      else Failure(errMsg(in.charAt(-1)), in)
    }
  }

  /** Verifies that the next character is not one of those specified.
    * Succeeds at the end of the input and does not consume any input
    * or produce a result when it succeeds.
    */
  def nextNot (char: Char, chars: Char*): Parser[Unit] = nextNot(NonEmptySet.of(char, chars:_*))

  /** Verifies that the next character is not one of those specified.
    * Succeeds at the end of the input and does not consume any input
    * or produce a result when it succeeds.
    */
  def nextNot (chars: NonEmptySet[Char]): Parser[Unit] = nextNot(chars.contains(_))

  /** Verifies that the next character does not satisfy the specified predicate.
    * Succeeds at the end of the input and does not consume any input
    * or produce a result when it succeeds.
    */
  def nextNot (predicate: Char => Boolean): Parser[Unit] = {
    val errMsg: Char => Message = Message.forRuntimeValue[Char] { found =>
      s"next character '$found' does not satisfy the specified predicate"
    }
    Parser { in =>
      if (in.remaining == 0) Success((), in)
      else if (!predicate(in.char)) Success((), in)
      else Failure(errMsg(in.char), in)
    }
  }


  /** Verifies that the previous character is one of those specified.
    * Fails at the start of the input and does not consume any input
    * or produce a result when it succeeds.
    */
  def prevIn (char: Char, chars: Char*): Parser[Unit] = prevIn(NonEmptySet.of(char, chars:_*))

  /** Verifies that the previous character is one of those specified.
    * Fails at the start of the input and does not consume any input
    * or produce a result when it succeeds.
    */
  def prevIn (chars: NonEmptySet[Char]): Parser[Unit] = prevIs(chars.contains(_))

  /** Verifies that the previous character satisfies the specified predicate.
    * Fails at the start of the input and does not consume any input
    * or produce a result when it succeeds.
    */
  def prevIs (predicate: Char => Boolean): Parser[Unit] = {
    val errMsg: Char => Message = Message.forRuntimeValue[Char] { found =>
      s"previous character '$found' does not satisfy the specified predicate"
    }
    def atStart: Message = Message.fixed("unable to check predicate on start of input")
    Parser { in =>
      if (in.offset == 0) Failure(atStart, in)
      else if (predicate(in.charAt(-1))) Success((), in)
      else Failure(errMsg(in.charAt(-1)), in)
    }
  }

  /** Verifies that the next character is one of those specified.
    * Fails at the end of the input and does not consume any input
    * or produce a result when it succeeds.
    */
  def nextIn (char: Char, chars: Char*): Parser[Unit] = nextIn(NonEmptySet.of(char, chars:_*))

  /** Verifies that the next character is one of those specified.
    * Fails at the end of the input and does not consume any input
    * or produce a result when it succeeds.
    */
  def nextIn (chars: NonEmptySet[Char]): Parser[Unit] = nextIs(chars.contains(_))

  /** Verifies that the next character does not satisfy the specified predicate.
    * Fails at the end of the input and does not consume any input
    * or produce a result when it succeeds.
    */
  def nextIs (predicate: Char => Boolean): Parser[Unit] = {
    val errMsg: Char => Message = Message.forRuntimeValue[Char] { found =>
      s"next character '$found' does not satisfy the specified predicate"
    }
    def atEnd: Message = Message.fixed("unable to check predicate on end of input")
    Parser { in =>
      if (in.remaining == 0) Failure(atEnd, in)
      else if (predicate(in.char)) Success((), in)
      else Failure(errMsg(in.char), in)
    }
  }




  /** Consumes any kind of input, always succeeds.
   *  This parser would consume the entire input unless a `max` constraint
   *  is specified.
   */
  val anyChars: Characters[String] = Characters.anyWhile(_ => true)
  
  @deprecated("renamed to anyChars", "0.14.0")
  val any: Characters[String] = Characters.anyWhile(_ => true)
  
  /** Consumes any number of consecutive occurrences of the specified characters.
   *  Always succeeds unless a minimum number of required matches is specified.
   */
  def anyOf (char: Char, chars: Char*): Characters[String] = Characters.include(char +: chars)

  /** Consumes any number of consecutive occurrences of the specified characters.
    * Always succeeds unless a minimum number of required matches is specified.
    */
  def anyOf (chars: NonEmptySet[Char]): Characters[String] = Characters.include(chars.toSortedSet.toSeq)
  
  /** Consumes any number of consecutive characters that are not one of the specified characters.
   *  Always succeeds unless a minimum number of required matches is specified.
   */
  def anyNot (char: Char, chars: Char*): Characters[String] = Characters.exclude(char +: chars)

  /** Consumes any number of consecutive occurrences that are not one of the specified characters.
    * Always succeeds unless a minimum number of required matches is specified.
    */
  def anyNot (chars: NonEmptySet[Char]): Characters[String] = Characters.exclude(chars.toSortedSet.toSeq)

  @deprecated("renamed to anyNot", "0.14.0")
  def anyBut (char: Char, chars: Char*): Characters[String] = Characters.exclude(char +: chars)
  
  @deprecated("use anyOf, someOf, oneOf with the range method, e.g. oneOf(range('a','z'))", "0.14.0")
  def anyIn (ranges: Iterable[Char]*): Characters[String] = Characters.include(ranges.flatten)

  /** Consumes any number of consecutive characters which satisfy the specified predicate.
    * Always succeeds unless a minimum number of required matches is specified.
    */
  def anyWhile (p: Char => Boolean): Characters[String] = Characters.anyWhile(p)


  /** Consumes one character if it matches one of the specified characters, fails otherwise.
    */
  def oneOf (char: Char, chars: Char*): PrefixedParser[String] = {
    val startChars = NonEmptySet.of(char, chars:_*)
    new PrefixCharacters(anyOf(startChars).take(1), startChars)
  }

  /** Consumes one character if it matches one of the specified characters, fails otherwise.
    */
  def oneOf (chars: NonEmptySet[Char]): PrefixedParser[String] = new PrefixCharacters(anyOf(chars).take(1), chars)

  /** Consumes one character if it is not one of the specified characters.
    */
  def oneNot (char: Char, chars: Char*): Parser[String] = Characters.exclude(char +: chars).take(1)

  /** Consumes one character if it is not one of the specified characters.
    */
  def oneNot (chars: NonEmptySet[Char]): Parser[String] = Characters.exclude(chars.toSortedSet.toSeq).take(1)

  /** Consumes one character if it satisfies the specified predicate, fails otherwise.
    */
  def oneIf (p: Char => Boolean): Parser[String] = Characters.anyWhile(p).take(1)

  /** Parses exactly one character from the input, fails only at the end of the input.
    */
  val oneChar: Parser[String] = anyChars.take(1)

  /** Consumes one or more characters if they match one of the specified characters, 
    * fails if the first character does not match.
    */
  def someOf (char: Char, chars: Char*): PrefixCharacters[String] = {
    val startChars = NonEmptySet.of(char, chars:_*)
    new PrefixCharacters(anyOf(startChars).min(1), startChars)
  }

  /** Consumes one or more characters if they match one of the specified characters, 
    * fails if the first character does not match.
    */
  def someOf (chars: NonEmptySet[Char]): PrefixCharacters[String] = new PrefixCharacters(anyOf(chars).min(1), chars)

  /** Consumes one or more characters that are not one of the specified characters, 
    * fails for empty results.
    */
  def someNot (char: Char, chars: Char*): Characters[String] = Characters.exclude(char +: chars).min(1)

  /** Consumes one or more characters that are not one of the specified characters, 
    * fails for empty results.
    */
  def someNot (chars: NonEmptySet[Char]): Characters[String] = Characters.exclude(chars.toSortedSet.toSeq).min(1)

  /** Consumes one or more characters which satisfy the specified predicate, 
    * fails for empty results.
    */
  def someWhile (p: Char => Boolean): Characters[String] = Characters.anyWhile(p).min(1)
  
  /** Consumes any number of consecutive characters until one of the specified characters
    * is encountered on the input string.
    */
  def delimitedBy (char: Char, chars: Char*): DelimitedText = new DelimitedText(TextDelimiter(oneOf(char, chars: _*)))

  /** Consumes any number of consecutive characters until one of the specified characters
    * is encountered on the input string.
    */
  def delimitedBy (chars: NonEmptySet[Char]): DelimitedText = new DelimitedText(TextDelimiter(oneOf(chars)))

  /** Consumes any number of consecutive characters until the specified string delimiter
    * is encountered on the input string.
    */
  def delimitedBy (str: String): DelimitedText =
    if (str.isEmpty) DelimitedText.Undelimited
    else delimitedBy(literal(str))

  @deprecated("compose the post condition manually, e.g. delimitedBy(':' <~ eol)", "0.14.0")
  def delimitedBy (str: String, postCondition: Parser[Any]): DelimitedText =
    if (str.isEmpty) DelimitedText.Undelimited
    else delimitedBy(literal(str) <~ lookAhead(postCondition))

  /** Consumes any number of consecutive characters until the specified delimiter parser
    * succeeds on the input. 
    * 
    * This constructor is limited to the sub-trait `PrefixedParser`
    * as only those can be optimized for an assertion that needs to be performed on each
    * character. Most parsers for non-empty text implement this trait, e.g `oneOf`, `someOf`,
    * `delimiter` or the literal parsers for a character or string.
    */
  def delimitedBy (delimiter: PrefixedParser[String]): DelimitedText = new DelimitedText(TextDelimiter(delimiter))

  /** Creates a parser for a delimiter based on the given set of delimiter characters 
    * with an API that allows to specify predicates for the characters immediately 
    * preceding or following the delimiter, a common task in markup parsing.
    */
  def delimiter (char: Char, chars: Char*): DelimiterParser = new DelimiterParser(oneOf(char, chars:_*))

  /** Creates a parser for a delimiter based on a literal string with an API that 
    * allows to specify predicates for the characters immediately 
    * preceding or following the delimiter, a common task in markup parsing.
    */
  def delimiter (delim: String): DelimiterParser = new DelimiterParser(literal(delim))

  /** Creates a parser for a delimiter with an API that allows to specify 
    * predicates for the characters immediately preceding or following 
    * the delimiter, a common task in markup parsing.
    * 
    * This specified underlying parser needs to implement the sub-trait `PrefixedParser`
    * as only those can be optimized for an assertion that needs to be performed on each
    * character. Most parsers for non-empty text implement this trait, e.g `oneOf`, `someOf`,
    * `delimiter` or the literal parsers for a character or string.
    */
  def delimiter (parser: PrefixedParser[String]): DelimiterParser = new DelimiterParser(parser)

}

/** Instance that allows to import all text parsers in isolation.
  * 
  * Usually it is more convenient to import laika.parse.api._ 
  * to get all parser builders with one import.
  */
object TextParsers extends TextParsers
