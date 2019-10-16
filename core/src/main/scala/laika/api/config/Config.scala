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
import laika.parse.hocon.HoconParsers.{ObjectValue, TracedValue}

/**
  * @author Jens Halm
  */
class Config (private[laika] val root: ObjectValue, private[laika] val fallbacks: Seq[Config] = Nil) {

  def get[T](key: String)(implicit decoder: ConfigDecoder[T]): Either[ConfigError, T] = {
    root.values
      .find(_.key == key)
      .toRight(NotFound(Path.Root / key))
      .flatMap(f => decoder(TracedValue(f.value, Set()))) // TODO - initial draft, later: split keys, use fallbacks + actual origin
  }
  
  def get[T](key: String, default: => T)(implicit decoder: ConfigDecoder[T]): Either[ConfigError, T] = 
    getOpt(key).map(_.getOrElse(default))
  
  def getOpt[T](key: String)(implicit decoder: ConfigDecoder[T]): Either[ConfigError, Option[T]] = get(key).fold(
    e => if (e.isInstanceOf[NotFound]) Right(None) else Left(e),
    r => Right(Some(r))
  )
  
  def get[T](implicit decoder: ConfigDecoder[T], defaultKey: DefaultKey[T]): Either[ConfigError, T] = get[T](defaultKey.value)
  
  def withValue[T](key: String, value: T)(implicit encoder: ConfigEncoder[T]) : ConfigBuilder = 
    ConfigBuilder.empty.withValue(key, value).withFallback(this)

  def withValue[T](value: T)(implicit encoder: ConfigEncoder[T], defaultKey: DefaultKey[T]): ConfigBuilder =
    ConfigBuilder.empty.withValue(value).withFallback(this)
  
  def withFallback(other: Config): Config = new Config(root, fallbacks :+ other) // TODO - should only exist on ConfigBuilder
  
  def resolve: Config = this // TODO - should only exist on ConfigBuilder
  
  override def hashCode: Int = (root, fallbacks).hashCode

  override def equals (obj: Any): Boolean = obj match {
    case c: Config => (c.root, c.fallbacks).equals((root, fallbacks))
    case _ => false
  }
  
}

object Config {
  
  type Result[T] = Either[ConfigError, T]
  
  val empty: Config = new Config(ObjectValue(Nil))
  
}
