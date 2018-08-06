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

package laika.parse.text

import laika.parse._
import laika.parse.text.DelimiterResult._

import scala.annotation.tailrec

/** A parser for text that ends with a specific delimiter condition,
  * either marking the end of the text span or the start of an embedded
  * inner span.
  *
  * @author Jens Halm
  */
class DelimitedText[T] (val delimiter: Delimiter[T]) extends Parser[T] {


  private val maxChar: Char = if (delimiter.startChars.nonEmpty) delimiter.startChars.max else 0

  private lazy val optimizedDelimiters: Array[Byte] = Characters.optimizedLookup(delimiter.startChars)


  def parse (ctx: ParserContext): Parsed[T] = {

    val source = ctx.input
    val end = source.length
    val lookup = optimizedDelimiters

    @tailrec
    def parse (offset: Int): Parsed[T] = {

      def charsConsumed = offset - ctx.offset

      if (offset == end) delimiter.atEOF(charsConsumed, ctx)
      else {
        val char = source.charAt(offset)
        if (char <= maxChar && lookup(char) == 1) delimiter.atStartChar(char, charsConsumed, ctx) match {
          case Complete(result) => result
          case Continue         => parse(offset + 1)
        }
        else parse(offset + 1)
      }
    }

    parse(ctx.offset)
  }

}

object DelimitedText {

  /** A parser that reads to the end of the input, unless further conditions
    * are set on the returned `DelimierOptions`.
    *
    */
  lazy val Undelimited: DelimitedText[String] with DelimiterOptions = DelimiterOptions(ConfigurableDelimiter(Set())).acceptEOF

}

/** Represents the logic of a specific kind of text delimiter.
  *
  * @tparam T the type of result produced by this delimiter
  */
trait Delimiter[T] {

  /** The start characters that mark the (potential) end of the delimited text
    * in case the conditions implemented in `atStartChar` are met.
    */
  def startChars: Set[Char]

  /** Method invoked every time the parser encounters any of the `startChars`.
    * The result is either `Continue` in case the additional conditions for the
    * delimiter are not met at this position, or a `Complete` instance containing
    * the result.
    *
    * @param startChar the start character that was encountered on the input string (matches one of the characters
    *                  in the `startChar` set)
    * @param charsConsumed the number of characters consumed before the delimiter has been reached
    * @param context the parser context at the position the delimiter has been reached
    * @return either `Continue` in case the additional conditions for the
    *         delimiter are not met at this position, or a `Complete` instance containing
    *         the result
    */
  def atStartChar (startChar: Char, charsConsumed: Int, context: ParserContext): DelimiterResult[T]

  /** Method invoked when the end of the input is reached.
    *
    * @param charsConsumed the number of characters consumed before EOF has been reached
    * @param context the parser context at the position EOF has been reached
    * @return the result of the parser
    */
  def atEOF (charsConsumed: Int, context: ParserContext): Parsed[T]

}

/** Delimiter implementation that allows for various kinds of customization.
  */
case class ConfigurableDelimiter (endDelimiters: Set[Char],
                                  postCondition: Option[Parser[Any]] = None,
                                  acceptEOF: Boolean = false,
                                  nonEmpty: Boolean = false,
                                  keepDelimiter: Boolean = false,
                                  failOn: Set[Char] = Set()) extends Delimiter[String] {

  val startChars = endDelimiters ++ failOn

  private val emptyResult = Message.fixed(s"expected at least 1 character before end delimiter")
  private val unexpectedInput: Char => Message =
    Message.forRuntimeValue[Char] (char => s"unexpected input in delimited text: `$char`")


  def atStartChar (startChar: Char, charsConsumed: Int, context: ParserContext): DelimiterResult[String] = {

    def applyPostCondition: Option[Int] = postCondition.fold(Option(0)) { parser =>
      parser.parse(context.consume(charsConsumed + 1)) match {
        case Success(_, next) => Some(next.offset - (context.offset + charsConsumed + 1))
        case _ => None
      }
    }

    def result (delimConsumed: Int): Success[String] = {
      val capturedText = context.capture(charsConsumed)
      val totalConsumed = if (keepDelimiter) charsConsumed else charsConsumed + 1 + delimConsumed
      Success(capturedText, context.consume(totalConsumed))
    }

    if (failOn.contains(startChar)) Complete(Failure(unexpectedInput(startChar), context))
    else {
      applyPostCondition match {
        case None                                      => Continue
        case Some(_) if charsConsumed == 0 && nonEmpty => Complete(Failure(emptyResult, context))
        case Some(delimConsumed)                       => Complete(result(delimConsumed))
      }
    }
  }

  def atEOF (charsConsumed: Int, context: ParserContext): Parsed[String] = {
    if (!acceptEOF) Failure(Message.UnexpectedEOF, context)
    else if (charsConsumed == 0 && nonEmpty) Failure(emptyResult, context)
    else Success(context.capture(charsConsumed), context.consume(charsConsumed))
  }

}

/** Represents the result of parsing a delimiter.
  *
  * @tparam T the type of result produced by this delimiter
  */
trait DelimiterResult[+T]

object DelimiterResult {

  /** Signals that the parsing should continue, meaning some conditions
    * for the delimiter at this positions have not been met.
    */
  case object Continue extends DelimiterResult[Nothing]

  /** Signals that the delimiter has been successfully parsed, ending
    * the parsing of the delimited text and providing the result.
    */
  case class Complete[T] (result: Parsed[T]) extends DelimiterResult[T]

}

/** API for customizing a delimiter.
  */
trait DelimiterOptions {

  protected def delimiter: ConfigurableDelimiter

  /** Creates a delimiter that also allows reaching the end of the input.
    * By default a delimiter based parser fails in that case.
    */
  def acceptEOF: DelimitedText[String] with DelimiterOptions = DelimiterOptions(delimiter.copy(acceptEOF = true))

  /** Creates a delimiter that also allows empty result, meaning reaching the delimiter before any non-delimiter
    * characters have been parsed.
    * By default a delimiter based parser fails in that case.
    */
  def nonEmpty: DelimitedText[String] with DelimiterOptions = DelimiterOptions(delimiter.copy(nonEmpty = true))

  /** Creates a delimiter that keeps the delimiter itself on the remaining input.
    * By default all delimiter characters are consumed.
    */
  def keepDelimiter: DelimitedText[String] with DelimiterOptions = DelimiterOptions(delimiter.copy(keepDelimiter = true))

  /** Creates a delimiter that fails when parsing any of the specified characters before a delimiter is encountered.
    */
  def failOn (chars: Char*): DelimitedText[String] with DelimiterOptions = DelimiterOptions(delimiter.copy(failOn = chars.toSet))

}

object DelimiterOptions {

  def apply (delim: ConfigurableDelimiter): DelimitedText[String] with DelimiterOptions
      = new DelimitedText(delim) with DelimiterOptions {

    override val delimiter = delim

  }

}
