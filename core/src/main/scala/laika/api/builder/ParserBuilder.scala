/*
 * Copyright 2012-2019 the original author or authors.
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
class ParserBuilder (format: MarkupFormat, val config: OperationConfig, rewrite: Boolean) extends ParserBuilderOps {

  val docType: TextDocumentType = Markup
  
  type ThisType = ParserBuilder

  def withConfig(newConfig: OperationConfig): ThisType = new ParserBuilder(format, newConfig, rewrite)

  /** Returns a new Parse instance that produces raw document trees without applying
   *  the default rewrite rules. These rules resolve link and image references and 
   *  rearrange the tree into a hierarchy of sections based on the (flat) sequence
   *  of header instances found in the document.
   */
  def withoutRewrite: ParserBuilder = new ParserBuilder(format, config, rewrite = false)

  /** Applies all configuration specified with this builder
    * and returns a new MarkupParser instance.
    */
  def build: MarkupParser = new MarkupParser(format, config, rewrite)

}


