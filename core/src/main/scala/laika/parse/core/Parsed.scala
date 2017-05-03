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

package laika.parse.core


/** Represents the result of a `Parser`, a value of type `T` in case of success,
  * a message in case of failure as well as the `ParserContext` for the remaining
  * input.
  *
  *  @author Jens Halm
  */
sealed abstract class Parsed[+T] {

  val next: ParserContext

  val isSuccess: Boolean

  def isFailure = !isSuccess

  def get: T

  def getOrElse[B >: T] (default: => B): B = if (isFailure) default else this.get

  def orElse[U >: T] (default: => Parsed[U]): Parsed[U] = if (isFailure) default else this

  def map[U] (f: T => U): Parsed[U]

}

/** The success case of `Parsed` containing the result and the remaining input.
  */
case class Success[+T] (result: T, next: ParserContext) extends Parsed[T] {

  val isSuccess = true

  def get: T = result

  def map[U](f: T => U) = Success(f(result), next)

  override def toString = s"[${next.position}] parsed: $result"

}

/**  The failure case of `Parsed` containing an error message and the remaining input.
  *
  *  Implementation note:
  *  The message property is of type `MessageProvider`, to allow for lazy message creation.
  *  The former SDK parser combinators which this API is partially inspired by contained
  *  a lot of unnecessary string concatenations for messages that were then never read.
  *  This implementation avoids this extra cost and the result is measurable (about 15%
  *  performance gain for a typical Markdown document for example).
  *
  *  @param msgProvider  A provider that produces an error message for this failure based on its ParserContext
  *  @param next         The parser's unconsumed input at the point where the failure occurred.
  */
case class Failure (msgProvider: MessageProvider, next: ParserContext) extends Parsed[Nothing] {

  lazy val message = msgProvider.message(next)

  val isSuccess = false

  def get: Nothing = scala.sys.error("No result available, parsing failed")

  def map[U](f: Nothing => U) = this

  override def toString = s"[${next.position}] failure: $message\n\n${next.position.lineContentWithCaret}"
}


trait MessageProvider {

  def message (context: ParserContext): String

}

object MessageProvider {

  def apply (f: ParserContext => String): MessageProvider = new MessageProvider {
    def message (context: ParserContext): String = f(context)
  }

}

case class FixedMessage (msg: String) extends MessageProvider {

  def message (context: ParserContext): String = msg

}

class MessageFunction[T] (input: T, f: T => String) extends MessageProvider {

  def message (context: ParserContext): String = f(input)

}

object Message {

  val UnexpectedEOF = FixedMessage("Unexpected end of input")

  val ExpectedFailure = FixedMessage("Expected failure, but parser succeeded")

  val ExpectedEOF = FixedMessage("Expected end of input")

  val ExpectedStart = FixedMessage("Expected start of input")

  val ExpectedEOL = FixedMessage("Expected end of line")

  def apply (msg: String): MessageProvider = FixedMessage(msg)

}
