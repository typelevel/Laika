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

import laika.ast.{Path, RelativePath}
import laika.config.Config

/** Translates paths of input documents to the corresponding output path. 
  * The minimum translation that usually has to happen is to replace the suffix from the input document the path
  * has been obtained from to the suffix of the output format. 
  * Further translations are allowed to happen based on user configuration.
  *
  * @author Jens Halm
  */
trait PathTranslator {

  /** Translates the specified path of an input document to the corresponding output path. 
    */
  def translate (input: Path): Path

  /** Translates the specified relative path of an input document to the corresponding output path. 
    */
  def translate (input: RelativePath): RelativePath
  
}

/** Translates paths of input documents to the corresponding output path, based on a configuration instance.
  * 
  * @author Jens Halm
  */
case class ConfigurablePathTranslator (config: Config, outputSuffix: String) extends PathTranslator {

  private val titleDocInputName = TitleDocumentConfig.inputName(config)
  private val titleDocOutputName = TitleDocumentConfig.outputName(config)

  def translate (input: Path): Path = {
    if (input.basename == titleDocInputName) input.withBasename(titleDocOutputName).withSuffix(outputSuffix)
    else input.withSuffix(outputSuffix)
  }

  def translate (input: RelativePath): RelativePath = {
    if (input.basename == titleDocInputName) input.withBasename(titleDocOutputName).withSuffix(outputSuffix)
    else input.withSuffix(outputSuffix)
  }
  
}

/** Basic path translator implementation that only replaces the suffix of the path.
  */
case class BasicPathTranslator (outputSuffix: String) extends PathTranslator {
  def translate (input: Path): Path = input.withSuffix(outputSuffix)
  def translate (input: RelativePath): RelativePath = input.withSuffix(outputSuffix)
}
