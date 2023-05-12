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

package laika.bundle

import laika.config.ConfigParser
import laika.parse.Parser
import laika.parse.combinator.Parsers

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
trait ConfigProvider { self =>

  /** The parser for configuration headers in markup documents.
    *
    * The parser is expected to fail if it does not recognize the fence
    * before and after the configuration header. Otherwise it is expected
    * to succeed, without parsing the actual string input from the configuration
    * header. This will happen lazily when the `ConfigParser` will be resolved
    * with a fallback Config provided by the runtime. This deferred resolution
    * is necessary as substitution references in configuration headers can
    * refer to values defined in configuration files or programmatically.
    */
  def markupConfigHeader: Parser[ConfigParser]

  /** The parser for configuration headers in template documents.
    *
    * The parser is expected to fail if it does not recognize the fence
    * before and after the configuration header. Otherwise it is expected
    * to succeed, without parsing the actual string input from the configuration
    * header. This will happen lazily when the `ConfigParser` will be resolved
    * with a fallback Config provided by the runtime. This deferred resolution
    * is necessary as substitution references in configuration headers can
    * refer to values defined in configuration files or programmatically.
    */
  def templateConfigHeader: Parser[ConfigParser]

  /** The parser for configuration files recognized in input directories.
    *
    * The returned `ConfigParser` will be resolved lazily
    * with a fallback Config provided by the runtime. This deferred resolution
    * is necessary as substitution references in configuration headers can
    * refer to values defined in configuration files or programmatically.
    */
  def configDocument(input: String): ConfigParser

  /** Returns a new config provider for strict mode that disables all extensions for text markup
    * documents which should run only features of the original markup spec.
    * Templates and configuration files remain unaffected.
    */
  def forStrictMode: ConfigProvider = new ConfigProvider {
    def markupConfigHeader            = ConfigProvider.empty.markupConfigHeader
    def templateConfigHeader          = self.templateConfigHeader
    def configDocument(input: String) = self.configDocument(input)
  }

}

object ConfigProvider {

  /** Provides parsers that always succeed by producing
    * an empty Config instance.
    */
  val empty: ConfigProvider = new ConfigProvider {
    def markupConfigHeader            = Parsers.success(ConfigParser.empty)
    def templateConfigHeader          = Parsers.success(ConfigParser.empty)
    def configDocument(input: String) = ConfigParser.empty
  }

}
