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

import laika.parse.hocon.HoconParsers.ObjectValue

/**
  * @author Jens Halm
  */
class Config (root: ObjectValue) {

  def get[T](key: String)(implicit decoder: ConfigDecoder[T]): Either[ConfigError, T] = ???
  
  def get[T](key: String, default: => T)(implicit decoder: ConfigDecoder[T]): Either[ConfigError, T] = ???
  
  def getOpt[T](key: String)(implicit decoder: ConfigDecoder[T]): Either[ConfigError, Option[T]] = ???
  
  def get[T](implicit decoder: ConfigDecoder[T], defaultKey: DefaultKey[T]): Either[ConfigError, T] = ???
  
  def withFallback(other: Config): Config = ??? // TODO - should only exist on ConfigBuilder
  
}

object Config {
  
  val empty: Config = new Config(ObjectValue(Nil))
  
}
