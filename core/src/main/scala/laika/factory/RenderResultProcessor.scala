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

package laika.factory

import laika.ast.DocumentTreeRoot
import laika.io.{BinaryOutput, RenderedTreeRoot}

/** Post processor for the result output of a renderer.
 *  Useful for scenarios where interim formats will be generated
 *  (e.g. XSL-FO for a PDF target or XHTML for an EPUB target).
 *  
 *  @author Jens Halm
 */
trait RenderResultProcessor[FMT] {


  /** The render format for the interim result.
   */
  def format: RenderFormat[FMT]

  /** A hook that allows this processor to modify or enhance the given
    * document tree. A common use case is to add navigation information
    * for e-book formats like EPUB or PDF for example.
    * 
    * The modified tree produced by this method will be used for
    * rendering the interim format specified with the `format` property.
    */
  def prepareTree (tree: DocumentTreeRoot): DocumentTreeRoot

  /** Processes the interim render result and writes it to the specified final output.
   */
  def process (result: RenderedTreeRoot, output: BinaryOutput): Unit
  
}
