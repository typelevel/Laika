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

package laika.api.config

import cats.data.NonEmptyChain
import cats.syntax.all.*
import laika.ast.Path
import laika.parse.Failure

/** Base trait for all configuration errors that occurred
  * during parsing, resolving, retrieving or decoding configuration values.
  *
  * @author Jens Halm
  */
sealed trait ConfigError {
  def message: String
}

object ConfigError {

  /** Indicates that a value found in the configuration does not have the expected
    * type so that type conversion is not even attempted.
    */
  case class InvalidType(expected: String, actual: ConfigValue) extends ConfigError {

    val message: String =
      s"Invalid type - expected: $expected, actual: ${actual.productPrefix.replace("Value", "")}"

  }

  /** An error that occurred when decoding a configuration value to a target type. */
  case class DecodingFailed(error: String, key: Option[Key] = None) extends ConfigError {
    val message: String = key.fold("")(k => s"Error decoding '${k.toString}': ") + error

    def withKey(key: Key): DecodingFailed = copy(key = Some(key))
  }

  /** A generic error for invalid values. */
  case class ValidationFailed(message: String) extends ConfigError

  /** An unrecoverable error that occurred when parsing HOCON input.
    * Recoverable errors are accumulated in `InvalidFields` instead.
    */
  case class ParsingFailed(failure: Failure) extends ConfigError {
    val message = failure.toString
  }

  /** One or more invalid fields occurred in HOCON input. */
  case class InvalidFields(fields: NonEmptyChain[InvalidField]) extends ConfigError {

    val message =
      fields.map(_.toString).mkString_("Multiple invalid fields in HOCON source: ", ", ", "")

  }

  /** Represents a single invalid field in HOCON input.
    */
  case class InvalidField(name: String, failure: Failure) {

    override def toString: String = {
      val prefix = if (name == "<invalid>" || name == "<RootKey>") "" else s"'$name': "
      prefix + failure.toString
    }

  }

  /** Multiple errors that occurred when processing configuration. */
  case class MultipleErrors(failures: NonEmptyChain[ConfigError]) extends ConfigError {

    val message =
      failures.map(_.toString).mkString_("Multiple errors processing configuration: ", ", ", "")

  }

  /** Multiple errors that occurred when processing configuration for a document. */
  case class DocumentErrors(path: Path, failures: NonEmptyChain[ConfigError])
      extends ConfigError {

    val message = failures.map(_.toString).mkString_(
      s"Multiple errors processing configuration for document '$path': ",
      ", ",
      ""
    )

  }

  object DocumentErrors {

    def apply(path: Path, error: ConfigError): DocumentErrors = error match {
      case MultipleErrors(errors) => new DocumentErrors(path, errors)
      case other                  => new DocumentErrors(path, NonEmptyChain.one(other))
    }

  }

  /** Multiple errors that occurred when processing configuration for a document tree. */
  case class TreeErrors(failures: NonEmptyChain[DocumentErrors]) extends ConfigError {

    val message = failures.map(_.toString).mkString_(
      s"Multiple errors processing configuration for document tree: ",
      ", ",
      ""
    )

  }

  /** An error that occurred when resolving the interim result of a parsing operation. */
  case class ResolverFailed(message: String) extends ConfigError

  /** An error that occurred when loading a resource, before parsing could start. */
  case class ResourceLoadingFailed(message: String) extends ConfigError

  /** A required value that could not be found. */
  case class NotFound(key: Key) extends ConfigError {
    val message: String = s"Not found: '$key'"
  }

}
