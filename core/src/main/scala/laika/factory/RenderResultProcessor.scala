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

import laika.ast.DocumentTree
import laika.io.{BinaryOutput, RenderedTreeRoot}

/** Post processor for the result output of a renderer.
 *  Useful for scenarios where interim formats will be generated
 *  (e.g. XSL-FO for a PDF target or HTML for an epub target).
 *  
 *  @author Jens Halm
 */
trait RenderResultProcessor[FMT] {


  /** The factory for the renderer that produces the interim result.
   */
  def format: RenderFormat[FMT]
  
  def prepareTree (tree: DocumentTree): DocumentTree

  /** Processes the result and writes it to the specified final output.
   * 
   *  @param result the result of the render operation as a tree
   *  @param output the output to write the final result to
   */
  def process (result: RenderedTreeRoot, output: BinaryOutput): Unit
  
}
