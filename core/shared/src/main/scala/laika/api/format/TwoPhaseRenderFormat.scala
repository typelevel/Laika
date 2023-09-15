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

package laika.api.format

import laika.ast.DocumentTreeRoot

/** Render format based on a render phase for an interim result and a post processor.
  *
  *  Examples for such a format are PDF (with XSL-FO as the interim format)
  *  or EPUB (with XHTML as the interim format).
  *
  *  @author Jens Halm
  */
trait TwoPhaseRenderFormat[FMT, PP] extends Format {

  /** The render format for the interim result, the first phase of this renderer.
    */
  def interimFormat: RenderFormat[FMT]

  /** A hook that allows this processor to modify or enhance the given
    * document tree. A common use case is to add navigation information
    * for e-book formats like EPUB or PDF for example.
    *
    * The modified tree produced by this method will be used for
    * rendering the interim format specified with the `format` property.
    */
  def prepareTree(tree: DocumentTreeRoot): Either[Throwable, DocumentTreeRoot]

  /** Post processor that produces the final result based on the interim format.
    */
  def postProcessor: PP

}
