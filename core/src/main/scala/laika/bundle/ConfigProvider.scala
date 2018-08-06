/*
 * Copyright 2013-2018 the original author or authors.
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

package laika.bundle

import com.typesafe.config.{Config, ConfigFactory, ConfigParseOptions}
import laika.ast.Path
import laika.io.Input
import laika.parse.text.TextParsers._

/** Factory for Typesafe Config instances that add information
  * about the virtual path of the configuration within a Laika
  * document tree.
  *
  * @author Jens Halm
  */
object ConfigProvider {

  /** Creates a Typesafe Config instance based on the specified input string
    * which is expected to be in HOCON format.
    *
    * Use this factory method instead of the ones in the Typesafe library, as this
    * method adds an origin description for the virtual path (the library mostly deals with
    * files in that respect). The origin description helps resolving relative paths within
    * this configuration instance.
    */
  def fromInput (input: Input): Config =
    ConfigFactory.parseReader(input.asReader, ConfigParseOptions.defaults().setOriginDescription("path:"+input.path))

}

/** Extension methods for Typesafe Config instances.
  */
object ConfigImplicits {

  private val laikaPath = "path:" ~> anyBut(':') <~ opt(':' ~ anyIn('0' to '9').min(1)) ^^ (Path(_).parent)

  /** Extension methods for Typesafe Config instances.
    */
  implicit class ConfigOps (val config: Config) extends AnyVal {

    /** Looks up the specified key and if found, resolves
      * it as a path relative to the path of the configuration.
      *
      * Any path reference users declare in configuration files
      * or in configuration headers in text markup files can
      * be interpreted as relative paths.
      */
    def getRelativePath (key: String): Option[Path] = {
      if (config.hasPath(key)) {
        val value = config.getValue(key)
        val desc = value.origin.description
        val basePath = laikaPath.parse(desc).getOrElse(Path.Root)
        Some((basePath / Path(value.unwrapped().toString)).relativeTo(Path.Root))
      }
      else None
    }

  }

}
