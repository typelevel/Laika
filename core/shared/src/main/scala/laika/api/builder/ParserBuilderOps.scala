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

package laika.api.builder

import laika.api.config.{ ConfigEncoder, DefaultKey, Key }
import laika.ast.MessageFilter

/** API for specifying configuration options that apply to all
  * kinds of operations that contain a parsing step (Parser and Transformer).
  *
  * @author Jens Halm
  */
private[api] trait ParserBuilderOps extends CommonBuilderOps {

  /**  Turns strict mode on for the target parser, switching off any
    *  features not part of the original markup syntax.
    *  This includes the registration of markup directives (custom tags)
    *  as well as configuration sections at the start of the document.
    */
  def strict: ThisType = withConfig(config.forStrictMode)

  /**  Enables all extensions that process raw content embedded into the host
    *  markup language.
    *  These are disabled by default as Laika is designed to render to multiple
    *  output formats from a single input document. With raw content embedded
    *  the markup document is tied to a specific output format.
    */
  def withRawContent: ThisType = withConfig(config.forRawContent)

  /** Specifies the filter to apply to runtime messages that should cause a transformation to fail.
    *
    * The default is to fail transformations on messages of level `Error` or higher.
    */
  def failOnMessages(filter: MessageFilter): ThisType = withConfig(
    config.withMessageFilters(
      MessageFilters.custom(failOn = filter, render = config.messageFilters.render)
    )
  )

  /** Returns a new instance with the specified configuration value added.
    *
    * The specified value with have higher precedence than any value with the same key registered by extension bundles,
    * but lower precedence than any value with the same key specified in a configuration file for a directory
    * or a configuration header in a markup document.
    */
  def withConfigValue[T: ConfigEncoder: DefaultKey](value: T): ThisType = withConfig(
    config.withConfigValue(value)
  )

  /** Returns a new instance with the specified configuration value added.
    *
    * The specified value with have higher precedence than any value with the same key registered by extension bundles,
    * but lower precedence than any value with the same key specified in a configuration file for a directory
    * or a configuration header in a markup document.
    */
  def withConfigValue[T: ConfigEncoder](key: String, value: T): ThisType = withConfig(
    config.withConfigValue(key, value)
  )

  /** Returns a new instance with the specified configuration value added.
    *
    * The specified value with have higher precedence than any value with the same key registered by extension bundles,
    * but lower precedence than any value with the same key specified in a configuration file for a directory
    * or a configuration header in a markup document.
    */
  def withConfigValue[T: ConfigEncoder](key: Key, value: T): ThisType = withConfig(
    config.withConfigValue(key, value)
  )

}
