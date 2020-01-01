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
import laika.ast.DocumentTreeRoot
import laika.factory.TwoPhaseRenderFormat

/** A renderer that operates with two phases, producing an interim result.
  * 
  * Examples for such renderers are EPUB (with XHTML as the interim format)
  * and PDF (with XSL-FO as the interim format).
  * 
  * This instance does not come with its own runtime. Instead its need to be passed
  * to a builder API in laika-io that knows how to execute such an operation.
  * 
  * @param interimRenderer the renderer for the 1st phase, producing the interim result
  * @param prepareTree a hook with which the interim result can be modified before it gets
  *                    passed to the post processor
  * @param postProcessor the processor taking the interim result and producing the final 
  *                      result, the implementing type may vary from format to format
  * @param description short string describing the output format for tooling and logging 
  * @tparam PP the type of the post processor 
  *            
  * @author Jens Halm
  */
case class TwoPhaseRenderer[PP] (interimRenderer: Renderer,
                                 prepareTree: DocumentTreeRoot => Either[Throwable, DocumentTreeRoot],
                                 postProcessor: PP,
                                 description: String)


/** Builder API for Renderer instances.
  *
  * Allows to add ExtensionBundles, to override the renderer for specific elements
  * and other options.
  *
  * @tparam FMT the formatter API to use which varies depending on the renderer
  * @tparam PP the type of the post processor
  *
  * @author Jens Halm
  */
class TwoPhaseRendererBuilder[FMT, PP] (val twoPhaseFormat: TwoPhaseRenderFormat[FMT, PP],
                                        val config: OperationConfig) extends RendererBuilderOps[FMT] {

  protected[this] val renderFormat = twoPhaseFormat.interimFormat

  type ThisType = TwoPhaseRendererBuilder[FMT, PP]

  def withConfig(newConfig: OperationConfig): ThisType = new TwoPhaseRendererBuilder[FMT, PP](twoPhaseFormat, newConfig)

  /** Applies all configuration specified with this builder
    * and returns a new Renderer instance.
    */
  def build: TwoPhaseRenderer[PP] = TwoPhaseRenderer(
    new RendererBuilder(renderFormat, config).build,
    twoPhaseFormat.prepareTree,
    twoPhaseFormat.postProcessor,
    twoPhaseFormat.description
  )

}
