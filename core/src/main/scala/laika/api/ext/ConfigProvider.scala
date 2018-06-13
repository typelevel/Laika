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

package laika.api.ext

import com.typesafe.config.{Config, ConfigFactory, ConfigParseOptions}
import laika.io.Input
import laika.tree.Paths.{Path, Root}
import laika.parse.core.text.TextParsers._

/**
  * @author Jens Halm
  */
object ConfigProvider {

//  def fromFile (name: String)(implicit codec: Codec): Config = fromInput(Input.fromFile(name)(codec))
//
//  def fromFile (file: File)(implicit codec: Codec): Config = fromInput(Input.fromFile(file)(codec))
//
//  def fromString (input: String, path: Path = Root): Config = fromInput(Input.fromString(input, path))

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

object ConfigImplicits {

  private val laikaPath = "path:" ~> anyBut(':') <~ opt(':' ~ anyIn('0' to '9').min(1)) ^^ (Path(_).parent)

  implicit class ConfigOps (val config: Config) extends AnyVal {

    def getRelativePath (key: String): Option[Path] = {
      if (config.hasPath(key)) {
        val value = config.getValue(key)
        val desc = value.origin.description
        val basePath = laikaPath.parse(desc).getOrElse(Root)
        Some((basePath / Path(value.unwrapped().toString)).relativeTo(Root))
      }
      else None
    }

  }

}
