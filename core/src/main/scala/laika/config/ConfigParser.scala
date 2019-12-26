/*
 * Copyright 2012-2019 the original author or authors.
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

import laika.parse.{Failure, Success}
import laika.parse.hocon.{ConfigResolver, HoconParsers, ObjectBuilderValue}

/** A parser for obtaining a Config instance from a HOCON string.
  * 
  * The HOCON format expected by this parsers is specified at
  * [[https://github.com/lightbend/config/blob/master/HOCON.md]]
  * 
  * @author Jens Halm
  */
class ConfigParser(input: String) {
  
  private val parsed: Either[ConfigError, ObjectBuilderValue] = 
    HoconParsers.rootObject.parse(input) match {
      case Success(builderRoot, _) => Right(builderRoot)
      case f: Failure => Left(ConfigParserError(f))
    }

  /** Parses and resolves the parsed HOCON input.
    * This includes the resolving of substitution references
    * and the merging of concatenated objects, arrays and strings.
    * 
    * Failures may be caused by both, the parser and resolver step.
    * 
    * The specified origin will be attached to every field.
    * Origins can be used to distinguish values from a specific Config
    * instance from those which were inherited from a fallback, which
    * might be relevant in scenarios where relative paths need to be
    * resolved for example.
    * 
    * The specified fallback will be used for resolving keys which are 
    * not present in the configuration created by this parser.
    * 
    * If an entire object is requested in the resulting Config instance,
    * the keys will be merged from this parser with those present in the fallback.
    * Simple values on the other hand will always override values with the same
    * key in the fallback.
    */
  def resolve(origin: Origin = Origin.root, fallback: Config = EmptyConfig): Either[ConfigError, Config] =
    parsed.flatMap(ConfigResolver
      .resolve(_, origin, fallback)
      .map(new ObjectConfig(_, origin, fallback))
    )

}

object ConfigParser {

  /** Creates a new parser for the specified HOCON input.
    */
  def parse(input: String): ConfigParser = new ConfigParser(input)

}
