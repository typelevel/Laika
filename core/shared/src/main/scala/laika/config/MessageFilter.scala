/*
 * Copyright 2012-2023 the original author or authors.
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

package laika.config

import laika.ast.{ MessageLevel, RuntimeMessage }

/** A filter for runtime messages that meet a specified minimum message level.
  */
sealed trait MessageFilter {
  def apply(message: RuntimeMessage): Boolean
}

object MessageFilter {

  case object None extends MessageFilter {
    def apply(message: RuntimeMessage) = false
  }

  private def forLevel(level: MessageLevel) = new MessageFilter {
    def apply(message: RuntimeMessage) = message.level >= level
  }

  val Debug: MessageFilter   = forLevel(MessageLevel.Debug)
  val Info: MessageFilter    = forLevel(MessageLevel.Info)
  val Warning: MessageFilter = forLevel(MessageLevel.Warning)
  val Error: MessageFilter   = forLevel(MessageLevel.Error)
  val Fatal: MessageFilter   = forLevel(MessageLevel.Fatal)
}
