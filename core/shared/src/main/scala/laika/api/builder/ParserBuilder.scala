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

import laika.api.MarkupParser
import laika.api.format.MarkupFormat
import laika.config.{ MessageFilter, MessageFilters }

/** Builder API for Parser instances.
  *
  * Allows to add ExtensionBundles and the `strict` and `withRawContent` flags.
  *
  * @author Jens Halm
  */
class ParserBuilder private[laika] (format: MarkupFormat, val config: OperationConfig)
    extends ParserBuilderOps {

  type ThisType = ParserBuilder

  def withConfig(newConfig: OperationConfig): ThisType = new ParserBuilder(format, newConfig)

  /** Specifies the filter to apply to runtime messages that should cause a transformation to fail.
    *
    * The default is to fail transformations on messages of level `Error` or higher.
    */
  def failOnMessages(filter: MessageFilter): ThisType = withConfig(
    config.withMessageFilters(
      MessageFilters.custom(failOn = filter, render = config.messageFilters.render)
    )
  )

  /** Applies all configuration specified with this builder
    * and returns a new MarkupParser instance.
    */
  def build: MarkupParser = new MarkupParser(format, config)

}
