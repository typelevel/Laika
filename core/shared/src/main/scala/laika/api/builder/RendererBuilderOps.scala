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

import laika.ast.{ Element, MessageFilter }
import laika.bundle.ExtensionBundle
import laika.factory.RenderFormat

/** API for specifying configuration options that apply to all
  * kinds of operations that contain a rendering step (Renderer and Transformer).
  *
  * @author Jens Halm
  */
private[api] trait RendererBuilderOps[FMT] extends CommonBuilderOps {

  protected def renderFormat: RenderFormat[FMT]

  /**  Specifies a custom render function that overrides one or more of the default
    *  renderers for the output format this instance uses.
    *
    *  This method expects a partial function that takes a formatter and the element
    *  to render. It will then be invoked for each element it is defined at.
    *
    *  Simple example for customizing the HTML output for emphasized text, adding a specific
    *  style class:
    *
    *  {{{
    *  val transformer = Transformer.from(Markdown).to(HTML).rendering {
    *    case (fmt, Emphasized(content, opt)) => fmt.element("em", opt, content, "class" -> "big")
    *  }.build
    *  }}}
    */
  def rendering(customRenderer: PartialFunction[(FMT, Element), String]): ThisType = using(
    new ExtensionBundle {
      val description: String      = "Custom render function"
      override val renderOverrides = Seq(renderFormat.Overrides(value = customRenderer))
    }
  )

  /**  Specifies the minimum required level for a runtime message to get included into the output by this renderer.
    */
  def renderMessages(filter: MessageFilter): ThisType = withConfig(
    config.withMessageFilters(render = filter)
  )

  /**  Renders without any formatting (line breaks or indentation).
    *  Useful when storing the output in a database for example.
    */
  def unformatted: ThisType = withConfig(config.renderUnformatted)

}
