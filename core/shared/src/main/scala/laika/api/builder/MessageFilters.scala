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

package laika.api.builder

import laika.ast.MessageFilter

/** Configuration that specify which levels of message severity
  * will cause a transformation to fail and which will be rendered to the output.
  *
  * Several AST transformation steps can insert nodes with a message
  * into the tree and the way the runtime handles those is configurable.
  *
  * By default, `MessageFilters.defaults` will be used which does not render
  * any messages to the output and fails on severity `Error` or higher.
  * This is usually the best setting for scenarios that do not involve visual debugging.
  *
  * When running Laika in a preview server (like the one built into Laika's sbt plugin),
  * then `MessageFilters.forVisualDebugging` will be used internally.
  * This setting will never cause a transformation to fail,
  * but instead render all messages from level `Info` upwards.
  *
  * `MessageFilters.custom` allows the definition of your own combination of filters.
  */
sealed abstract class MessageFilters {

  /** Indicates the minimum severity level for message to cause a transformation to fail. */
  def failOn: MessageFilter

  /** Indicates the minimum severity level for message to get rendered to the output. */
  def render: MessageFilter
}

object MessageFilters {

  private final case class Impl(failOn: MessageFilter, render: MessageFilter)
      extends MessageFilters {
    override def productPrefix = "MessageFilters"
  }

  /** Does not render any messages to the output and fails on severity `Error` or higher.
    * This is usually the best setting for scenarios that do not involve visual debugging.
    */
  val defaults: MessageFilters = Impl(
    failOn = MessageFilter.Error,
    render = MessageFilter.None
  )

  /** This setting will never cause a transformation to fail,
    * but instead render all messages from level `Info` upwards.
    * This setting is used internally when using Laika's preview server.
    */
  val forVisualDebugging: MessageFilters = Impl(
    failOn = MessageFilter.None,
    render = MessageFilter.Info
  )

  /** Creates a custom combination of message filters.
    *
    * @param failOn indicates the minimum severity level for message to cause a transformation to fail
    * @param render indicates the minimum severity level for message to get rendered to the output
    */
  def custom(failOn: MessageFilter, render: MessageFilter): MessageFilters = Impl(failOn, render)
}
