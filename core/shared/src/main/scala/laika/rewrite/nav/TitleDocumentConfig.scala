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

import laika.config.Config

/** Configuration for the names of title documents in the input and output trees.
  * 
  * @author Jens Halm
  */
object TitleDocumentConfig {

  private val defaultInputName: String = "README"
  private val defaultOutputName: String = "index"

  /** The name to denote a title document in an input tree as configured in the specified instance.
    */
  def inputName (config: Config): String =
    config.get[String]("titleDocuments.inputName").toOption.getOrElse(defaultInputName)

  /** The name to assign to a title document before rendering as configured in the specified instance.
    */
  def outputName (config: Config): String =
    config.get[String]("titleDocuments.outputName").toOption.getOrElse(defaultOutputName)

}
