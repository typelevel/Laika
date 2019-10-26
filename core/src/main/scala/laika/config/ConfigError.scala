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

import laika.ast.Path

/**
  * @author Jens Halm
  */
sealed trait ConfigError {
  def message: String
  protected def render(key: Path): String = key.components.mkString(".")
}
trait ConfigBuilderError

case class InvalidType(expected: String, actual: ConfigValue) extends ConfigError {
  val message: String = s"Invalid type - expected: $expected, actual: ${actual.productPrefix.replaceAllLiterally("Value","")}"
}
case class ConversionError(message: String) extends ConfigError
case class ValidationError(message: String) extends ConfigError
case class ConfigParserError(message: String) extends ConfigError
case class ConfigResolverError(message: String) extends ConfigError

case class NotFound(path: Path) extends ConfigError {
  val message: String = s"Not found: '${render(path)}'"
}
