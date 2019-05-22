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

package laika.api

import laika.ast.DocumentType.Markup
import laika.ast.{Document, TextDocumentType}
import laika.config.{OperationConfig, ParseConfigBuilder}
import laika.factory.MarkupFormat

/** API for performing a parse operation from various types of input to obtain
 *  a document tree without a subsequent render operation. 
 *  
 *  In cases where a render operation should follow immediately, it is more 
 *  convenient to use the [[laika.api.Transform]] API instead which 
 *  combines a parse and a render operation directly.
 *  
 *  Example for parsing Markdown from a file:
 *  
 *  {{{
 *  val doc = Parse as Markdown fromFile "hello.md"
 *  }}}
 * 
 *  @author Jens Halm
 */
class Parse private[api] (parser: MarkupFormat, val config: OperationConfig, rewrite: Boolean)
  extends ParseConfigBuilder {

  val docType: TextDocumentType = Markup
  
  type ThisType = Parse

  def withConfig(newConfig: OperationConfig): ThisType = new Parse(parser, newConfig, rewrite)

  /** Returns a new Parse instance that produces raw document trees without applying
   *  the default rewrite rules. These rules resolve link and image references and 
   *  rearrange the tree into a hierarchy of sections based on the (flat) sequence
   *  of header instances found in the document.
   */
  def withoutRewrite: Parse = new Parse(parser, config, rewrite = false)
  
  def parse (input: String): Document = ???

}

/** Serves as an entry point to the Parse API.
 * 
 *  @author Jens Halm
 */
object Parse {
  
  /** Returns a new Parse instance for the specified parser factory.
   *  This factory is usually an object provided by the library
   *  or a plugin that is capable of parsing a specific markup
   *  format like Markdown or reStructuredText. 
   * 
   *  @param parser the parser factory to use for all subsequent operations
   */
  def as (parser: MarkupFormat): Parse = new Parse(
    parser,
    OperationConfig.default.withBundlesFor(parser),
    rewrite = true
  )

}
