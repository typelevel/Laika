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

import laika.parse.Failure

/** Base trait for all configuration errors that occurred
  * during parsing, resolving, retrieving or convering
  * configuration values.
  * 
  * @author Jens Halm
  */
sealed trait ConfigError {
  def message: String
}

/** Indicates that a value found in the configuration does not have the expected
  * type so that type conversion is not even attempted. */
case class InvalidType(expected: String, actual: ConfigValue) extends ConfigError {
  val message: String = s"Invalid type - expected: $expected, actual: ${actual.productPrefix.replaceAllLiterally("Value","")}"
}

/** An error that occurred when decoding a configuration value to a target type. */
case class DecodingError (message: String) extends ConfigError

/** A generic error for invalid values. */
case class ValidationError(message: String) extends ConfigError

/** An error that occurred when parsing HOCON input. */
case class ConfigParserError(failure: Failure) extends ConfigError {
  val message = failure.toString
}

/** Multiple errors that occurred when parsing HOCON input. */
case class ConfigParserErrors(failures: Seq[Failure]) extends ConfigError {
  val message = failures.map(_.toString).mkString("Multiple errors parsing HOCON: ", ", ", "")
}

/** An error that occurred when resolving the interim result of a parsing operation. */
case class ConfigResolverError(message: String) extends ConfigError

/** A required value that could not be found. */
case class NotFound(key: Key) extends ConfigError {
  val message: String = s"Not found: '$key'"
}

/** A ConfigError as a RuntimeException for use cases where a Throwable is required. */
case class ConfigException(error: ConfigError) extends RuntimeException(error.message)
