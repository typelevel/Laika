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

package laika.api.config

import laika.ast.Path
import laika.parse.hocon.HoconParsers.{ConfigValue, Field, LongValue, ObjectValue, Origin, StringValue, TracedValue}

/**
  * @author Jens Halm
  */
class ConfigBuilder (fields: Seq[Field], origin: Origin, fallbacks: Seq[Config] = Nil) {

  def withValue[T](key: String, value: T)(implicit encoder: ConfigEncoder[T]) : ConfigBuilder =
    new ConfigBuilder(fields :+ Field(key, encoder(value)), origin, fallbacks) // TODO - path expansion
  
  def withValue[T](value: T)(implicit encoder: ConfigEncoder[T], defaultKey: DefaultKey[T]): ConfigBuilder =
    new ConfigBuilder(fields :+ Field(defaultKey.value, encoder(value)), origin, fallbacks) // TODO - path expansion
  
  def build: Config = new Config(ObjectValue(fields))

  def withFallback(other: Config): ConfigBuilder = new ConfigBuilder(fields, origin, fallbacks :+ other)
  
  // TODO - move to companion
  def withOrigin(path: Path): ConfigBuilder = new ConfigBuilder(fields, Origin(path))
  
}

object ConfigBuilder {

  val empty: ConfigBuilder = new ConfigBuilder(Nil, Origin.root)

  def parse(input: String): ConfigBuilder = ??? // TODO - 0.12 - move to ConfigParser
  
}

trait ConfigEncoder[-T] {
  def apply(value: T): ConfigValue
}
trait ConfigDecoder[T] { self =>
  
  def apply (value: TracedValue[ConfigValue]): Either[ConfigError, T]
  
  def flatMap[U](f: T => Either[ConfigError, U]): ConfigDecoder[U] = new ConfigDecoder[U] {
    def apply (value: TracedValue[ConfigValue]) = self.apply(value).flatMap(f)
  }
  
  def map[U](f: T => U): ConfigDecoder[U] = new ConfigDecoder[U] {
    def apply (value: TracedValue[ConfigValue]) = self.apply(value).map(f)
  }
}
trait DefaultKey[T] {
  def value: String
}

object DefaultKey {
  def apply[T](key: String): DefaultKey[T] = new DefaultKey[T] { val value: String = key }
}

trait ConfigError
trait ConfigBuilderError

case class InvalidType(expected: String, actual: String) extends ConfigError
case class ValidationError(message: String) extends ConfigError

object ConfigEncoder {
  implicit lazy val string: ConfigEncoder[String] = new ConfigEncoder[String] {
    def apply (value: String) = StringValue(value)
  }
  implicit lazy val int: ConfigEncoder[Int] = new ConfigEncoder[Int] {
    def apply (value: Int) = LongValue(value.toLong)
  }
  implicit lazy val configValue: ConfigEncoder[ConfigValue] = new ConfigEncoder[ConfigValue] {
    def apply (value: ConfigValue) = value
  }

  implicit def seq[T] (implicit elementEncoder: ConfigEncoder[T]): ConfigEncoder[Seq[T]] = ???
}

object ConfigDecoder {
  //def apply[T](f: ConfigValue => Either[ConfigError, T]): ConfigDecoder[T] = value => f(value)
  implicit lazy val string: ConfigDecoder[String] = new ConfigDecoder[String] {
    def apply (value: TracedValue[ConfigValue]) = value.value match {
      case StringValue(s) => Right(s) // TODO - convert other types
      case _ => Left(InvalidType("String", ""))
    }
  }
  implicit lazy val int: ConfigDecoder[Int] = new ConfigDecoder[Int] {
    def apply (value: TracedValue[ConfigValue]) = value.value match {
      case LongValue(n) => Right(n.toInt) // TODO - convert other types, check bounds
      case _ => Left(InvalidType("Number", ""))
    }
  }
  implicit lazy val configValue: ConfigDecoder[ConfigValue] = new ConfigDecoder[ConfigValue] {
    def apply (value: TracedValue[ConfigValue]) = Right(value.value)
  }
  implicit def tracedValue[T](implicit valueDecoder: ConfigDecoder[T]): ConfigDecoder[TracedValue[T]] = ???

  implicit lazy val path: ConfigDecoder[Path] = tracedValue[String].map { tracedValue =>
    val basePath = tracedValue.origins.headOption.fold[Path](Path.Root)(_.path)
    (basePath / Path(tracedValue.value)).relativeTo(Path.Root)
  }
  
  implicit def seq[T] (implicit elementDecoder: ConfigDecoder[T]): ConfigDecoder[Seq[T]] = ???
}
