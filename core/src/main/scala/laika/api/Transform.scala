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
import laika.ast._
import laika.config.{OperationConfig, TransformConfigBuilder}
import laika.factory.{MarkupParser, RenderFormat}

/** API for performing a transformation operation from and to various types of input and output,
 *  combining a parse and render operation. 
 *  
 *  In cases where a parse or render operation should
 *  be performed separately, for example for manually processing the document tree model
 *  between these operations, the [[laika.api.Parse]] and [[laika.api.Render]] APIs 
 *  should be used instead.
 *  
 *  Example for transforming from Markdown to HTML using files for both input and output:
 *  
 *  {{{
 *  Transform from Markdown to HTML fromFile "hello.md" toFile "hello.html"
 *  }}}
 *  
 *  Or for transforming a document fragment from a string to the AST format
 *  for debugging purposes:
 *  
 *  {{{
 *  val input = "some *emphasized* text"
 *  
 *  Transform from Markdown to AST fromString input toString
 *  
 *  res0: java.lang.String = 
 *  Document - Blocks: 1
 *  . Paragraph - Spans: 3
 *  . . Text - 'some ' 
 *  . . Emphasized - Spans: 1
 *  . . . Text - 'emphasized'
 *  . . Text - ' text'
 *  }}}
 *  
 *  Apart from specifying input and output, the Transform API also allows to customize the operation
 *  in various ways. The `usingRule` and `creatingRule` methods allow to rewrite the document tree
 *  between the parse and render operations and the `rendering` method allows to customize the
 *  way certain types of elements are rendered.
 *  
 *  @tparam FMT the formatter API to use which varies depending on the renderer
 * 
 *  @author Jens Halm
 */
class Transform [FMT] private[Transform] (parser: MarkupParser, 
                                          val format: RenderFormat[FMT], 
                                          val config: OperationConfig) extends
  TransformConfigBuilder[FMT] {
  
  type ThisType = Transform[FMT]

  val docType: TextDocumentType = Markup

  def withConfig (newConfig: OperationConfig): ThisType = new Transform(parser, format, newConfig)
  
  def transform (input: String): String = ???
  
}

/** Serves as an entry point to the Transform API.
 * 
 *  @author Jens Halm
 */
object Transform {

  /** Step in the setup for a transform operation where the
   *  renderer must be specified.
   */
  class Builder private[Transform] (parser: MarkupParser, config: OperationConfig) {

    /** Creates and returns a new Transform instance for the specified renderer and the
     *  previously specified parser. The returned instance is stateless and reusable for
     *  multiple transformations.
     * 
     *  @param format the render format to use for the transformation
     *  @return a new Transform instance
     */
    def to [FMT] (format: RenderFormat[FMT]): Transform[FMT] = new Transform(parser, format, config)
    
  }
  
  /** Returns a new Builder instance for the specified parser factory.
   *  This factory is usually an object provided by the library
   *  or a plugin that is capable of parsing a specific markup
   *  format like Markdown or reStructuredText. The returned builder
   *  can then be used to specifiy the renderer to create the actual
   *  Transform instance.
   * 
   *  @param parser the parser factory to use
   *  @return a new Builder instance for specifying the renderer
   */
  def from (parser: MarkupParser): Builder = new Builder(parser, OperationConfig.default.withBundlesFor(parser))
  
}
