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

/**
  * TODO - this is migration in progress
  *
  * @author Jens Halm
  */
trait Parsed {

}

trait MessageProvider {

  def message (context: Reader): String

}

object MessageProvider {

  def apply (f: Reader => String): MessageProvider = new MessageProvider {
    def message (context: Reader): String = f(context)
  }

}

case class FixedMessage (msg: String) extends MessageProvider {

  def message (context: Reader): String = msg

}

class MessageFunction[T] (input: T, f: T => String) extends MessageProvider {

  def message (context: Reader): String = f(input)

}

object Message {

  val UnexpectedEOF = FixedMessage("Unexpected end of input")

  val ExpectedFailure = FixedMessage("Expected failure, but parser succeeded") // TODO - should include Parser.toString

  val ExpectedEOF = FixedMessage("Expected end of input")

  val ExpectedStart = FixedMessage("Expected start of input")

  val ExpectedEOL = FixedMessage("Expected end of line")

  def apply (msg: String): MessageProvider = FixedMessage(msg)

}
