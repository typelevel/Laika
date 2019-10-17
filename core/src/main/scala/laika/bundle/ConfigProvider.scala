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

import laika.api.config.{Config, ConfigError, ConfigParser}
import laika.ast.Path
import laika.parse.Parser
import laika.parse.combinator.Parsers
import laika.parse.hocon.HoconParsers
import laika.parse.hocon.HoconParsers.Origin
import laika.parse.markup.DocumentParser.{ParserError, ParserInput}

/** Factory for Config instances that add information
  * about the virtual path of the configuration within a Laika
  * document tree.
  *
  * @author Jens Halm
  */
trait ConfigProvider {
  
  def configHeader: Parser[UnresolvedConfig]
  
  def configDocument (input: String): UnresolvedConfig
  
}

object ConfigProvider {

  /** Creates a Config instance based on the specified input string
    * which is expected to be in HOCON format.
    *
    * This method adds a origin description to the Config instance which helps 
    * resolving relative paths within this configuration instance.
    */
  def fromInput (input: String, path: Path): Config = ConfigParser.parse(input, path).resolve.right.get // TODO - 0.12 - error handling

  // temporary API
  def fromInput (input: ParserInput): Either[ParserError, Config] = Right(fromInput(input.context.input, input.path)) // TODO - 0.12 - remove
  
  val empty: ConfigProvider = new ConfigProvider {
    def configHeader = Parsers.success(UnresolvedConfig.empty)
    def configDocument (input: String) = UnresolvedConfig.empty
  }
  
}

trait UnresolvedConfig {
  
  def resolve (origin: Origin, fallback: Config): Either[ConfigError, Config]
  
}

object UnresolvedConfig {
  
  val empty: UnresolvedConfig = new UnresolvedConfig {
    def resolve (origin: Origin, fallback: Config) = Right(fallback)
  }
  
  def default (input: String): UnresolvedConfig = new UnresolvedConfig {
    def resolve (origin: HoconParsers.Origin, fallback: Config) =
      ConfigParser.parse(input).withFallback(fallback).resolve
  }
  
}
