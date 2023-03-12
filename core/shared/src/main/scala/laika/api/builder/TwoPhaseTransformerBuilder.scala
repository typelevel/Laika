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

import laika.factory.{ MarkupFormat, RenderFormat, TwoPhaseRenderFormat }

/** Builder API for Transformer instances.
  *
  * Allows to add ExtensionBundles, to register AST rewrite rules,
  * to override the renderer for specific elements and other options.
  *
  * @tparam FMT the formatter API to use which varies depending on the renderer
  * @tparam PP the type of the post processor
  *
  * @author Jens Halm
  */
class TwoPhaseTransformerBuilder[FMT, PP](
    val markupFormat: MarkupFormat,
    val twoPhaseRenderFormat: TwoPhaseRenderFormat[FMT, PP],
    val config: OperationConfig
) extends TransformerBuilderOps[FMT] {

  protected[this] val renderFormat: RenderFormat[FMT] = twoPhaseRenderFormat.interimFormat

  type ThisType = TwoPhaseTransformerBuilder[FMT, PP]

  def withConfig(newConfig: OperationConfig): ThisType =
    new TwoPhaseTransformerBuilder[FMT, PP](markupFormat, twoPhaseRenderFormat, newConfig)

}
