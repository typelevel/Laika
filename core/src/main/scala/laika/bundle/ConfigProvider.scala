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

import laika.api.config.{Config, ConfigBuilder}
import laika.ast.Path
import laika.parse.markup.DocumentParser.{ParserError, ParserInput}
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
  def fromInput (input: String, path: Path): Config = ???
    // ConfigBuilder.parse(input, ConfigParseOptionsX.defaults().setOriginDescription("path:" + path))

  // temporary API
  def fromInput (input: ParserInput): Either[ParserError, Config] = Right(fromInput(input.context.input, input.path))

}
