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

package laika.rewrite.nav

import laika.ast.Path
import laika.config.Config

/** Translates a path of a document from an input tree to the path to use
  * when rendering to the output tree.
  * 
  * @author Jens Halm
  */
case class PathTranslator (config: Config, outputSuffix: String) {

  private val titleDocInputName = TitleDocumentConfig.inputName(config)
  private val titleDocOutputName = TitleDocumentConfig.outputName(config)

  /** Translates the specified input path to an output path by applying the given suffix
    * and adjusting the base name in case it is a title document and configured input and output names differ. 
    */
  def translate (input: Path): Path = {
    if (input.basename == titleDocInputName) input.withBasename(titleDocOutputName).withSuffix(outputSuffix)
    else input.withSuffix(outputSuffix)
  }
  
}
