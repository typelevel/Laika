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
class DelimitedText[T] (val delimiter: Delimiter[T]) extends Parser[T] {


  private val maxChar: Char = if (delimiter.startChars.nonEmpty) delimiter.startChars.max else 0

  private lazy val optimizedDelimiters: Array[Int] = {

    val lookup = new Array[Int](maxChar + 1)

    for (c <- delimiter.startChars) lookup(c) = 1

    lookup
  } // TODO - centralize logic for array creation


  def apply (ctx: ParserContext): ParseResult[T] = {

    val source = ctx.input
    val end = source.length
    val lookup = optimizedDelimiters

    @tailrec
    def parse (offset: Int): ParseResult[T] = {

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

trait Delimiter[T] {

  def startChars: Set[Char]

  def atStartChar (startChar: Char, charsConsumed: Int, context: ParserContext): DelimiterResult[T]

  def atEOF (charsConsumed: Int, context: ParserContext): ParseResult[T]

}

case class ConfigurableDelimiter (endDelimiters: Set[Char],
                                  postCondition: (Char, Int, ParserContext) => Int = { (_,_,_) => 0 },
                                  acceptEOF: Boolean = false,
                                  nonEmpty: Boolean = false,
                                  keepDelimiter: Boolean = false,
                                  failOn: Set[Char] = Set()) extends Delimiter[String] {

  val startChars = endDelimiters ++ failOn

  private val EmptyResult = Message(s"expected at least 1 character before end delimiter")

  def atStartChar (startChar: Char, charsConsumed: Int, context: ParserContext): DelimiterResult[String] = {

    def unexpectedInput (char: Char)
      = new MessageFunction(char, (_: Char) => s"unexpected input in delimited text: `$char`")

    if (failOn.contains(startChar)) Complete(Failure(unexpectedInput(startChar), context))
    else {
      val delimConsumed = postCondition(startChar, charsConsumed, context)
      if (delimConsumed < 0) Continue
      else if (charsConsumed == 0 && nonEmpty) Complete(Failure(EmptyResult, context))
      else {
        val capturedText = context.capture(charsConsumed)
        val totalConsumed = if (keepDelimiter) charsConsumed else charsConsumed + 1 + delimConsumed
        Complete(Success(capturedText, context.consume(totalConsumed)))
      }
    }
  }

  def atEOF (charsConsumed: Int, context: ParserContext): ParseResult[String] = {
    if (!acceptEOF) Failure(Message.UnexpectedEOF, context)
    else if (charsConsumed == 0 && nonEmpty) Failure(EmptyResult, context)
    else Success(context.capture(charsConsumed), context.consume(charsConsumed))
  }

}

trait DelimiterResult[+T]

case object Continue extends DelimiterResult[Nothing]
case class Complete[T] (result: ParseResult[T]) extends DelimiterResult[T]

trait DelimiterOptions {

  protected def delimiter: ConfigurableDelimiter

  def acceptEOF: DelimitedText[String] with DelimiterOptions = DelimiterOptions(delimiter.copy(acceptEOF = true))

  def nonEmpty: DelimitedText[String] with DelimiterOptions = DelimiterOptions(delimiter.copy(nonEmpty = true))

  def keepDelimiter: DelimitedText[String] with DelimiterOptions = DelimiterOptions(delimiter.copy(keepDelimiter = true))

  def failOn (chars: Char*): DelimitedText[String] with DelimiterOptions = DelimiterOptions(delimiter.copy(failOn = chars.toSet))

  def withPostCondition (parser: Parser[Any]): DelimitedText[String] with DelimiterOptions = {
    val oldPostCondition = delimiter.postCondition
    DelimiterOptions(delimiter.copy(postCondition = { (char, consumed, context) =>
      val firstResult = oldPostCondition(char, consumed, context)
      if (firstResult == -1) -1 else
        parser(context.consume(consumed + 1 + firstResult)) match {
          case Success(_, next) => Math.max(firstResult, next.offset - (context.offset + consumed + 1))
          case _ => -1
        }
    }))
  }

}

object DelimiterOptions {

  def apply (delim: ConfigurableDelimiter): DelimitedText[String] with DelimiterOptions
      = new DelimitedText(delim) with DelimiterOptions {

    override val delimiter = delim

  }

}

object DelimitedBy {

  def apply (chars: Char*): DelimitedText[String] with DelimiterOptions = DelimiterOptions(ConfigurableDelimiter(chars.toSet))

  def apply (str: String): DelimitedText[String] with DelimiterOptions = {
    val len = str.length
    if (len == 0) Undelimited
    else if (len == 1) DelimiterOptions(ConfigurableDelimiter(Set(str.head)))
    else if (len == 2) DelimiterOptions(ConfigurableDelimiter(Set(str.head),
      // TODO - check ctx is not at end
      { (_,consumed,ctx) => if (ctx.charAt(consumed + 1) == str.charAt(1)) 1 else -1 }))
    else if (len == 3) DelimiterOptions(ConfigurableDelimiter(Set(str.head),
      // TODO - check ctx is not at end
      { (_,consumed,ctx) => if (ctx.charAt(consumed + 1) == str.charAt(1)
        && ctx.charAt(consumed + 2) == str.charAt(2)) 2 else -1 }))
    else throw new NotImplementedError("post conditions > 3 characters not implemented yet")

  }

  def apply[T] (delimiter: Delimiter[T]): DelimitedText[T] = new DelimitedText(delimiter)

  lazy val Undelimited: DelimitedText[String] with DelimiterOptions = DelimiterOptions(ConfigurableDelimiter(Set())).acceptEOF

}