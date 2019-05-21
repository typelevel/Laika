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

import laika.ast._
import laika.config.{OperationConfig, RenderConfigBuilder}
import laika.factory.RenderFormat

/** API for performing a render operation to various types of output using an existing
 *  document tree model. 
 *  
 *  In cases where a render operation follows a parse operation 
 *  immediately, it is more convenient to use the [[laika.api.Transform]] API 
 *  instead which combines a parse and a render operation directly.
 *  
 *  Example for rendering HTML to a file:
 *  
 *  {{{
 *  val doc: Document = ...
 *  
 *  Render as HTML from doc toFile "hello.html"
 *  }}}
 *  
 *  @tparam FMT the formatter API to use which varies depending on the renderer
 * 
 *  @author Jens Halm
 */
class Render[FMT] private (val format: RenderFormat[FMT],
                           val config: OperationConfig) extends RenderConfigBuilder[FMT] {

  protected[this] lazy val theme = config.themeFor(format)

  /** The concrete implementation of the abstract Render type.
   */
  type ThisType = Render[FMT]

  def withConfig(newConfig: OperationConfig): ThisType =
    new Render[FMT](format, newConfig)

  /** Renders the specified. This may be a `RootElement` instance
    *  as well as any other type of `Element`, thus allowing to render document
    *  fragments, too.
    *
    *  @param elem the element to render
    *  @return the rendered output as a string
    */
  def render (elem: Element): String = ???

  /** Renders the specified document.
    *
    *  @param doc the document to render
    *  @return the rendered output as a string
    */
  def render (doc: Document): String = ???

}

/** Serves as an entry point to the Render API.
 * 
 *  @author Jens Halm
 */
object Render {

  /** Returns a new Render instance for the specified render format.
   *  This factory is usually an object provided by the library
   *  or a plugin that is capable of rendering a specific output. 
   * 
   *  @param format the renderer factory responsible for creating the final renderer
   */
  def as [FMT] (format: RenderFormat[FMT]): Render[FMT] =
    new Render(format, OperationConfig.default)
  
}
