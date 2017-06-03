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

import DelimiterResult._

/**
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

  lazy val Undelimited: DelimitedText[String] with DelimiterOptions = DelimiterOptions(ConfigurableDelimiter(Set())).acceptEOF

}

trait Delimiter[T] {

  def startChars: Set[Char]

  def atStartChar (startChar: Char, charsConsumed: Int, context: ParserContext): DelimiterResult[T]

  def atEOF (charsConsumed: Int, context: ParserContext): Parsed[T]

}

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

    def applyPostCondition: Int = postCondition.fold(0) { parser =>
      parser.parse(context.consume(charsConsumed + 1)) match {
        case Success(_, next) => next.offset - (context.offset + charsConsumed + 1)
        case _ => -1
      }
    }

    def result (delimConsumed: Int): Success[String] = {
      val capturedText = context.capture(charsConsumed)
      val totalConsumed = if (keepDelimiter) charsConsumed else charsConsumed + 1 + delimConsumed
      Success(capturedText, context.consume(totalConsumed))
    }

    if (failOn.contains(startChar)) Complete(Failure(unexpectedInput(startChar), context))
    else {
      val delimConsumed = applyPostCondition
      if (delimConsumed < 0) Continue
      else if (charsConsumed == 0 && nonEmpty) Complete(Failure(emptyResult, context))
      else Complete(result(delimConsumed))
    }
  }

  def atEOF (charsConsumed: Int, context: ParserContext): Parsed[String] = {
    if (!acceptEOF) Failure(Message.UnexpectedEOF, context)
    else if (charsConsumed == 0 && nonEmpty) Failure(emptyResult, context)
    else Success(context.capture(charsConsumed), context.consume(charsConsumed))
  }

}

trait DelimiterResult[+T]

object DelimiterResult {

  case object Continue extends DelimiterResult[Nothing]
  case class Complete[T] (result: Parsed[T]) extends DelimiterResult[T]

}

trait DelimiterOptions {

  protected def delimiter: ConfigurableDelimiter

  def acceptEOF: DelimitedText[String] with DelimiterOptions = DelimiterOptions(delimiter.copy(acceptEOF = true))

  def nonEmpty: DelimitedText[String] with DelimiterOptions = DelimiterOptions(delimiter.copy(nonEmpty = true))

  def keepDelimiter: DelimitedText[String] with DelimiterOptions = DelimiterOptions(delimiter.copy(keepDelimiter = true))

  def failOn (chars: Char*): DelimitedText[String] with DelimiterOptions = DelimiterOptions(delimiter.copy(failOn = chars.toSet))

}

object DelimiterOptions {

  def apply (delim: ConfigurableDelimiter): DelimitedText[String] with DelimiterOptions
      = new DelimitedText(delim) with DelimiterOptions {

    override val delimiter = delim

  }

}
