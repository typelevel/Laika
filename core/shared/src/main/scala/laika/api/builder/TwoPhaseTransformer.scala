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
import laika.factory.{MarkupFormat, TwoPhaseRenderFormat}

/** A transformer that operates with two phases, producing an interim result.
  *
  * Examples for such transformers are EPUB (with XHTML as the interim format)
  * and PDF (with XSL-FO as the interim format).
  * 
  * This instance does not come with its own runtime. Instead its need to be passed
  * to a builder API in laika-io that knows how to execute such an operation.
  *
  * @param markupParser the parser for the markup format, producing the document tree
  * @param renderer the two phase renderer that first turn the document tree obtained from
  *                 the parser into an interim format and then passes it to the post processor
  * @tparam PP the type of the post processor 
  * @author Jens Halm
  */
case class TwoPhaseTransformer[PP] (markupParser: MarkupParser,
                                    renderer: TwoPhaseRenderer[PP])

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
class TwoPhaseTransformerBuilder[FMT, PP] (val markupFormat: MarkupFormat,
                                           val twoPhaseRenderFormat: TwoPhaseRenderFormat[FMT, PP],
                                           val config: OperationConfig) extends TransformerBuilderOps[FMT] {
                                           
  protected[this] val renderFormat = twoPhaseRenderFormat.interimFormat

  type ThisType = TwoPhaseTransformerBuilder[FMT, PP]

  def withConfig(newConfig: OperationConfig): ThisType = 
    new TwoPhaseTransformerBuilder[FMT, PP](markupFormat, twoPhaseRenderFormat, newConfig)

  /** Applies all configuration specified with this builder
    * and returns a new Transformer instance.
    */
  def build: TwoPhaseTransformer[PP] = TwoPhaseTransformer(
    new ParserBuilder(markupFormat, config).build,
    new TwoPhaseRendererBuilder[FMT, PP](twoPhaseRenderFormat, config).build
  )

}
