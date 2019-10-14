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

import laika.ast.ConfigValue
import laika.parse.hocon.HoconParsers.Field

/**
  * @author Jens Halm
  */
class ConfigBuilder (values: Seq[Field]) {

  def withValue[T](key: String, value: T)(implicit encoder: ConfigEncoder[T]) : ConfigBuilder = ???
  
  def withValue[T](value: T)(implicit encoder: ConfigEncoder[T], defaultKey: DefaultKey[T]): ConfigBuilder = ???
  
  def build: Config = ???

  def withFallback(other: Config): ConfigBuilder = ???
  
}

object ConfigBuilder {

  val empty: ConfigBuilder = new ConfigBuilder(Nil)

  def parse(input: String): ConfigBuilder = ??? // TODO - 0.12 - move to ConfigParser
  
}

trait ConfigEncoder[T]
trait ConfigDecoder[T]
trait DefaultKey[T] {
  def value: String
}

trait ConfigError
trait ConfigBuilderError

object ConfigEncoder {
  implicit val forString: ConfigEncoder[String] = ???
  implicit val forInt: ConfigEncoder[Int] = ???
  implicit val forConfigValue: ConfigEncoder[ConfigValue] = ???
}

object ConfigDecoder {
  implicit val forString: ConfigDecoder[String] = ???
  implicit val forInt: ConfigDecoder[Int] = ???
  implicit val forConfigValue: ConfigDecoder[ConfigValue] = ???
}
