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

package laika.config

import laika.collection.TransitionalCollectionOps._

/** A builder for creating a Config instance programmatically.
  * 
  * While it's most common in Laika that Config instances are obtained
  * by parsing HOCON, instances can also be created entirely programmatically,
  * or by a combination of HOCON and programmatic values if an existing
  * fallback is used with builder.
  * 
  * @author Jens Halm
  */
class ConfigBuilder (fields: Seq[Field], origin: Origin, fallback: Config = EmptyConfig) {

  /** Returns a new builder instance adding the specified value to the existing set of values.
    */
  def withValue[T](key: String, value: T)(implicit encoder: ConfigEncoder[T]): ConfigBuilder =
    withValue(Key.parse(key), value)

  /** Returns a new builder instance adding the specified value to the existing set of values.
    */
  def withValue[T](key: Key, value: T)(implicit encoder: ConfigEncoder[T]): ConfigBuilder =
    new ConfigBuilder(fields :+ expandPath(key, encoder(value)), origin, fallback)

  /** Returns a new builder instance adding the specified value to the existing set of values.
    */
  def withValue[T](value: T)(implicit encoder: ConfigEncoder[T], defaultKey: DefaultKey[T]): ConfigBuilder =
    withValue[T](defaultKey.value, value)

  /** Returns a new builder instance adding the specified value to the existing set of values if it is non-empty.
    */
  def withValue[T](key: String, value: Option[T])(implicit encoder: ConfigEncoder[T]): ConfigBuilder =
    value.fold(this)(withValue(Key.parse(key), _))

  /** Returns a new builder instance adding the specified value to the existing set of values if it is non-empty.
    */
  def withValue[T](key: Key, value: Option[T])(implicit encoder: ConfigEncoder[T]): ConfigBuilder =
    value.fold(this)(withValue(key, _))

  /** Creates a builder with the specified fallback which will be used
    * for resolving keys which are not present in the configuration created
    * by this builder.
    *
    * If an entire object is requested in the resulting Config instance,
    * the keys will be merged from this builder with those present in the fallback.
    * Simple values on the other hand will always override values with the same
    * key in the fallback.
    */
  def withFallback(newFallback: Config): ConfigBuilder = 
    new ConfigBuilder(fields, origin, fallback.withFallback(newFallback))

  /** Resolves all specified values and returns a new Config instance.
    */
  def build: Config = build(fallback)

  /** Resolves all specified values, using the specified fallback, and returns a new Config instance.
    */
  def build (fallback: Config): Config =
    if (fields.isEmpty && origin == Origin.root) fallback
    else new ObjectConfig(asObjectValue, origin, fallback)
  
  private[laika] def asObjectValue: ObjectValue = mergeObjects(ObjectValue(fields))
 
  private def expandPath(key: Key, value: ConfigValue): Field = {
    key.segments match {
      case name :: Nil => Field(name, value, origin)
      case name :: rest => Field(name, ObjectValue(Seq(expandPath(Key(rest), value))), origin)
      case Nil => Field("", value, origin)
    }
  }

  private def mergeObjects(obj: ObjectValue): ObjectValue = { // TODO - duplicate code with ObjectValue.merge

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

/** Companion factory for ConfigBuilder instances.
  */
object ConfigBuilder {

  val empty: ConfigBuilder = new ConfigBuilder(Nil, Origin.root)

  /** Creates a builder with the specified origin which will be attached
    * to every field specified with the new builder.
    * 
    * Origins can be used to distinguish values from a specific Config
    * instance from those which were inherited from a fallback, which
    * might be relevant in scenarios where relative paths need to be
    * resolved for example.
    */
  def withOrigin(origin: Origin): ConfigBuilder = new ConfigBuilder(Nil, origin)

  /** Creates a builder with the specified fallback which will be used
    * for resolving keys which are not present in the configuration created
    * by this builder.
    * 
    * If an entire object is requested in the resulting Config instance,
    * the keys will be merged from this builder with those present in the fallback.
    * Simple values on the other hand will always override values with the same
    * key in the fallback.
    */
  def withFallback(fallback: Config): ConfigBuilder = new ConfigBuilder(Nil, fallback.origin, fallback)

  /** Creates a builder with the specified fallback and origin.
    * 
    * The origin will be used for resolving keys which are not present in the 
    * configuration created by this builder.
    *
    * If an entire object is requested in the resulting Config instance,
    * the keys will be merged from this builder with those present in the fallback.
    * Simple values on the other hand will always override values with the same
    * key in the fallback.
    * 
    * Origins can be used to distinguish values from a specific Config
    * instance from those which were inherited from a fallback, which
    * might be relevant in scenarios where relative paths need to be
    * resolved for example.
    */
  def withFallback(fallback: Config, origin: Origin): ConfigBuilder = new ConfigBuilder(Nil, origin, fallback)

}
