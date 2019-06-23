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

package laika.parse

/** Represents the result of a `Parser`, a value of type `T` in case of success,
  * a message in case of failure as well as the `ParserContext` for the remaining
  * input.
  *
  *  @author Jens Halm
  */
sealed abstract class Parsed[+T] {

  /** The context representing the remaining input
    * left over by the parser that produced this result.
    */
  val next: ParserContext

  /** Indicates whether this results represents a successful
    * parser invocation. The `get` method will throw on instances
    * where this method returns `false`.
    */
  val isSuccess: Boolean

  /** Indicates whether this results represents an unsuccessful
    * parser invocation. The `get` method will throw on instances
    * where this method returns `true`.
    */
  def isFailure: Boolean = !isSuccess

  /** The result as an option, empty in case of failure.
    */
  def toOption: Option[T]

  /** Returns the result value from the parser invocation if the
    * parser succeeded or otherwise the specified fallback value.
    */
  def getOrElse[B >: T] (default: => B): B = toOption.getOrElse(default)

  /** Returns this `Parsed` instance if the parser suceeded or
    * otherwise the specified fallback instance.
    */
  def orElse[U >: T] (default: => Parsed[U]): Parsed[U] = if (isFailure) default else this

  /** Builds a new `Parsed` instance by applying the specified function
    * to the result of this instance.
    */
  def map[U] (f: T => U): Parsed[U]

}

/** The success case of `Parsed` containing the result and the remaining input.
  */
case class Success[+T] (result: T, next: ParserContext) extends Parsed[T] {

  val isSuccess = true

  def toOption: Option[T] = Some(result)

  def map[U](f: T => U) = Success(f(result), next)

  override def toString = s"[${next.position}] parsed: $result"

}

/**  The failure case of `Parsed` containing an error message and the remaining input.
  *
  *  Implementation note:
  *  The message property is of type `Message`, to allow for lazy message creation.
  *  The former SDK parser combinators which this API is partially inspired by contained
  *  a lot of unnecessary string concatenations for messages which were then never read.
  *  This implementation avoids this extra cost and the result is measurable (about 15%
  *  performance gain for a typical Markdown document for example).
  *
  *  @param msgProvider  A provider that produces an error message for this failure based on its ParserContext
  *  @param next         The parser's unconsumed input at the point where the failure occurred.
  */
case class Failure (msgProvider: Message, next: ParserContext) extends Parsed[Nothing] {

  /** The message specifying the cause of the failure.
    */
  lazy val message: String = msgProvider.message(next)

  val isSuccess = false

  def toOption: Option[Nothing] = None

  def map[U](f: Nothing => U): Failure = this

  override def toString = s"[${next.position}] failure: $message\n\n${next.position.lineContentWithCaret}"
}


/** Represents a lazy failure message.
  * Implementations can use the specified `ParserContext` to construct
  * the actual message, e.g. for adding parts of the input to the message.
  */
trait Message {

  def message (context: ParserContext): String

}

/** Message companion providing several pre-built messages
  * and factory methods.
  */
object Message {


  val UnexpectedEOF: Message = fixed("Unexpected end of input")

  val ExpectedFailure: Message = fixed("Expected failure, but parser succeeded")

  val ExpectedEOF: Message = fixed("Expected end of input")

  val ExpectedStart: Message = fixed("Expected start of input")

  val ExpectedEOL: Message = fixed("Expected end of line")


  class MessageFactory[T] (f: T => String) extends (T => Message) {

    def apply (value: T): Message = new Message {

      def message (context: ParserContext): String = f(value)

    }

  }

  /** Builds a message instance for a fixed string,
    * independent of the parser context.
    */
  def fixed (msg: String): Message = new Message {

    def message (context: ParserContext): String = msg

  }

  /** Builds a message instance for the specified
    * factory function.
    */
  def forContext (f: ParserContext => String): Message = new Message {
    def message (context: ParserContext): String = f(context)
  }

  /** Builds a factory function that produces new messages
    * based on some arbitrary input type. This allows
    * to pre-capture some context for the message that
    * does not relate to the parser context.
    */
  def forRuntimeValue[T] (f: T => String): T => Message = new MessageFactory(f)

}
