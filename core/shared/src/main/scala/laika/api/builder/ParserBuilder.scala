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
import laika.ast.DocumentType.Markup
import laika.ast.TextDocumentType
import laika.factory.MarkupFormat

/** Builder API for Parser instances.
  *
  * Allows to add ExtensionBundles and the `strict` and `withRawContent` flags.
  *
  * @author Jens Halm
  */
class ParserBuilder(format: MarkupFormat, val config: OperationConfig) extends ParserBuilderOps {

  val docType: TextDocumentType = Markup

  type ThisType = ParserBuilder

  def withConfig(newConfig: OperationConfig): ThisType = new ParserBuilder(format, newConfig)

  /** Applies all configuration specified with this builder
    * and returns a new MarkupParser instance.
    */
  def build: MarkupParser = new MarkupParser(format, config)

}
