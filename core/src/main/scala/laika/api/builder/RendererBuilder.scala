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

import laika.api.Renderer
import laika.factory.RenderFormat

/** Builder API for Renderer instances.
  * 
  * Allows to add ExtensionBundles, to override the renderer for specific elements
  * and other options.
  * 
  * @tparam FMT the formatter API to use which varies depending on the renderer
  * 
  * @author Jens Halm
  */
class RendererBuilder[FMT] (val renderFormat: RenderFormat[FMT],
                            val config: OperationConfig) extends RendererBuilderOps[FMT] {

  type ThisType = RendererBuilder[FMT]

  def withConfig(newConfig: OperationConfig): ThisType = new RendererBuilder[FMT](renderFormat, newConfig)

  /** Applies all configuration specified with this builder
    * and returns a new Renderer instance.
    */
  def build: Renderer = new Renderer(config) {
    override type Formatter = FMT
    override def format: RenderFormat[FMT] = renderFormat
  }

}
