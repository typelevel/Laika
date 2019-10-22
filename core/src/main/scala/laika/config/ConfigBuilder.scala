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
import laika.parse.hocon.HoconParsers._
import laika.collection.TransitionalCollectionOps._

import scala.util.Try

/**
  * @author Jens Halm
  */
class ConfigBuilder (fields: Seq[Field], origin: Origin, fallback: Config = EmptyConfig) {

  def withValue[T](key: String, value: T)(implicit encoder: ConfigEncoder[T]) : ConfigBuilder =
    new ConfigBuilder(fields :+ expandPath(Key(key), encoder(value)), origin, fallback)
  
  def withValue[T](value: T)(implicit encoder: ConfigEncoder[T], defaultKey: DefaultKey[T]): ConfigBuilder =
    withValue[T](defaultKey.value, value)
  
  def build: Config = 
    if (fields.isEmpty && origin == Origin.root) fallback 
    else new ObjectConfig(mergeObjects(ObjectValue(fields)), origin, fallback)

  private def expandPath(key: Path, value: ConfigValue): Field = {
    key.components match {
      case name :: Nil => Field(name, value, origin)
      case name :: rest => Field(name, ObjectValue(Seq(expandPath(Path(rest), value))), origin)
      case Nil => Field("", value, origin)
    }
  }

  def mergeObjects(obj: ObjectValue): ObjectValue = {

    def mergeValues(cbv1: ConfigValue, cbv2: ConfigValue): ConfigValue = (cbv1, cbv2) match {
      case (o1: ObjectValue, o2: ObjectValue) => mergeObjects(ObjectValue(o1.values ++ o2.values))
      case (_, v2) => v2
    }

    val mergedFields = obj.values.groupBy(_.key).mapValuesStrict(_.map(_.value)).toSeq.map {
      case (name, values) => Field(name, values.reduce(mergeValues), origin)
    }
    ObjectValue(mergedFields)
  }
  
}

object ConfigBuilder {

  val empty: ConfigBuilder = new ConfigBuilder(Nil, Origin.root)

  def withOrigin(origin: Origin): ConfigBuilder = new ConfigBuilder(Nil, origin)
  
  def withFallback(fallback: Config): ConfigBuilder = new ConfigBuilder(Nil, fallback.origin, fallback)
  
  def withFallback(fallback: Config, origin: Origin): ConfigBuilder = new ConfigBuilder(Nil, origin, fallback)

}

object Key {
  def apply(key: String): Path = {
    val segments = key.split("\\.").toList
    Path(segments)
  }
}
