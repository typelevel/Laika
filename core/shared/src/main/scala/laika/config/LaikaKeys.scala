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

package laika.config

/** Constants for configuration keys for the library's core configuration entries.
  * 
  * @author Jens Halm
  */
object LaikaKeys {

  val title = Key("title")
  
  val template = Key("template")
  
  def template (format: String): Key = Key(format, "template")
  
  val metadata = Key("metadata")
  
  val links = Key("links")
  
  val autonumbering = Key("autonumbering")
  
  val navigationOrder = Key("navigationOrder")
  
  object titleDocuments {
    val inputName = Key("titleDocuments","inputName")
    val outputName = Key("titleDocuments","outputName")
  }
  
}
