/*
 * Copyright 2013-2016 the original author or authors.
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

import com.typesafe.config.Config

/** Configuration for autonumbering of documents and sections.
 */
case class AutonumberConfig (documents: Boolean, sections: Boolean, maxDepth: Int)

case class ConfigurationException (msg: String) extends RuntimeException(msg)

object AutonumberConfig {
  
  /** Tries to obtain the autonumbering configuration
   *  from the specified configuration instance or returns
   *  the default configuration if not found.
   */
  def fromConfig (config: Config): AutonumberConfig = {
    if (config.hasPath("autonumbering")) {
      val nConf = config.getObject("autonumbering").toConfig
      val (documents, sections) = if (nConf.hasPath("scope")) nConf.getString("scope") match {
        case "documents" => (true,  false)
        case "sections"  => (false, true)
        case "all"       => (true,  true)
        case "none"      => (false, false)
        case other       => throw ConfigurationException("Unsupported value for key 'autonumbering.scope': " + other)
      } else                (false, false)
      val depth = if (nConf.hasPath("depth")) nConf.getInt("depth") else Int.MaxValue 
      AutonumberConfig(documents, sections, depth)
    }
    else defaults
  }
  
  /** The defaults for autonumbering with section
   *  and document numbering both switched off. 
   */
  def defaults: AutonumberConfig = AutonumberConfig(documents = false, sections = false, maxDepth = 0)
}
  