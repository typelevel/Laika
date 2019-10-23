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

import laika.config.{Config, ConfigError, ConfigParser, Origin}
import laika.parse.Parser
import laika.parse.combinator.Parsers

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
    def resolve (origin: Origin, fallback: Config) =
      ConfigParser.parse(input, origin).withFallback(fallback).resolve
  }
  
}
