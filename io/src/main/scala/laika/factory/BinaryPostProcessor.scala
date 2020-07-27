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

package laika.factory

import cats.effect.Sync
import laika.api.builder.OperationConfig
import laika.io.model.{BinaryOutput, RenderedTreeRoot}
import laika.io.runtime.Runtime

/** Post processor for the result output of a renderer.
 *  Useful for scenarios where interim formats will be generated
 *  (e.g. XSL-FO for a PDF target or XHTML for an EPUB target).
 *  
 *  @author Jens Halm
 */
trait BinaryPostProcessor {

  /** Processes the interim render result and writes it to the specified final output.
    * 
    * @param result the result of the renderer for the interim format
    * @param output the binary output to render to
    * @param config the operation config that had been used for the interim result 
   */
  def process[F[_]: Sync: Runtime] (result: RenderedTreeRoot[F], output: BinaryOutput[F], config: OperationConfig): F[Unit]
  
}
