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

import laika.config.{Config, ConfigBuilder, ConfigError, ConfigParser, Origin}
import laika.parse.Parser
import laika.parse.combinator.Parsers
import laika.parse.hocon.{IncludeResource, ObjectBuilderValue}

/** Responsible for providing the parsers for configuration files
  * and configuration headers in markup documents as part of an
  * `ExtensionBundle`.
  * 
  * Laika has a built-in implementation of this API that parses
  * configuration as HOCON, but these can be overridden by adding
  * an instance of this trait to a `ParserBundle` within an `ExtensionBundle`.
  *
  * @author Jens Halm
  */
trait ConfigProvider {

  /** The parser for configuration headers in markup documents.
    * 
    * The parser is expected to fail if it does not recognize the fence
    * before and after the configuration header. Otherwise it is expected
    * to succeed, without parsing the actual string input from the configuration
    * header. This will happen lazily when the `UnresolvedConfig` will be resolved
    * with a fallback Config provided by the runtime. This deferred resolution
    * is necessary as substitution references in configuration headers can
    * refer to values defined in configuration files or programmatically.
    */
  def configHeader: Parser[UnresolvedConfig]

  /** The parser for configuration files recognized in input directories.
    *
    * The returned `UnresolvedConfig` will be resolved lazily
    * with a fallback Config provided by the runtime. This deferred resolution
    * is necessary as substitution references in configuration headers can
    * refer to values defined in configuration files or programmatically.
    */
  def configDocument (input: String): UnresolvedConfig
  
}

object ConfigProvider {

  /** Provides parsers that always succeed by producing
    * an empty Config instance.
    */
  val empty: ConfigProvider = new ConfigProvider {
    def configHeader = Parsers.success(UnresolvedConfig.empty)
    def configDocument (input: String) = UnresolvedConfig.empty
  }
  
}

/** An unresolved Config instance that will be lazily resolved against a fallback.
  */
trait UnresolvedConfig {

  /** Extracts all unresolved requested includes the unresolved configuration contains,
    * including those nested inside other objects.
    */
  def includes: Seq[IncludeResource]

  /** Resolves the parsed configuration against the specified fallback,
    * using it for references not found in the parsed result.
    * 
    * The given origin will be used to tag each value parsed with this
    * instance.
    */
  def resolve (origin: Origin, fallback: Config, includes: Map[IncludeResource, Either[ConfigError, ObjectBuilderValue]]): Either[ConfigError, Config]
  
}

object UnresolvedConfig {

  /** Returns an empty instance that will always return just
    * the fallback unmodified.
    */
  val empty: UnresolvedConfig = new UnresolvedConfig {
    val includes = Nil
    def resolve (origin: Origin, fallback: Config, includes: Map[IncludeResource, Either[ConfigError, ObjectBuilderValue]]) = Right(ConfigBuilder.withFallback(fallback, origin).build)
  }

  /** Returns an unresolved config for the specified HOCON input
    * based on the library's built-in HOCON parser.
    */
  def default (input: String): UnresolvedConfig = new UnresolvedConfig {
    
    private val parser = ConfigParser.parse(input)
    
    lazy val includes: Seq[IncludeResource] = parser.includes
    
    def resolve (origin: Origin, fallback: Config, includes: Map[IncludeResource, Either[ConfigError, ObjectBuilderValue]]) =
      parser.resolve(origin, fallback)
  }
  
}
