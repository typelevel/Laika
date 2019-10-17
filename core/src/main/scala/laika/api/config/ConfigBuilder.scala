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
import laika.parse.hocon.HoconParsers._
import laika.collection.TransitionalCollectionOps._

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

  def withFallback(other: Config): ConfigBuilder = new ConfigBuilder(fields, origin, fallback.withFallback(other))
  
  // TODO - move to companion
  def withOrigin(path: Path): ConfigBuilder = new ConfigBuilder(fields, Origin(path))

  private def expandPath(key: Path, value: ConfigValue): Field = {
    key.components match {
      case name :: Nil => Field(name, value)
      case name :: rest => Field(name, ObjectValue(Seq(expandPath(Path(rest), value))))
      case Nil => Field("", value)
    }
  }

  def mergeObjects(obj: ObjectValue): ObjectValue = {

    def mergeValues(cbv1: ConfigValue, cbv2: ConfigValue): ConfigValue = (cbv1, cbv2) match {
      case (o1: ObjectValue, o2: ObjectValue) => mergeObjects(ObjectValue(o1.values ++ o2.values))
      case (_, v2) => v2
    }

    val mergedFields = obj.values.groupBy(_.key).mapValuesStrict(_.map(_.value)).toSeq.map {
      case (name, values) => Field(name, values.reduce(mergeValues))
    }
    ObjectValue(mergedFields)
  }
  
}

object ConfigBuilder {

  val empty: ConfigBuilder = new ConfigBuilder(Nil, Origin.root)

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

object Key {
  def apply(key: String): Path = {
    val segments = key.split("\\.").toList
    Path(segments)
  }
}

trait ConfigError
trait ConfigBuilderError

case class InvalidType(expected: String, actual: String) extends ConfigError
case class ValidationError(message: String) extends ConfigError
case class ConfigParserError(message: String) extends ConfigError
case class ConfigResolverError(message: String) extends ConfigError
case class NotFound(path: Path) extends ConfigError

object ConfigEncoder {
  
  implicit val string: ConfigEncoder[String] = new ConfigEncoder[String] {
    def apply (value: String) = StringValue(value)
  }
  
  implicit val int: ConfigEncoder[Int] = new ConfigEncoder[Int] {
    def apply (value: Int) = LongValue(value.toLong)
  }
  
  implicit val configValue: ConfigEncoder[ConfigValue] = new ConfigEncoder[ConfigValue] {
    def apply (value: ConfigValue) = value
  }
  
  implicit def seq[T] (implicit elementEncoder: ConfigEncoder[T]): ConfigEncoder[Seq[T]] = new ConfigEncoder[Seq[T]] {
    def apply (value: Seq[T]) = ArrayValue(value.map(elementEncoder.apply))
  }
}

object ConfigDecoder {
  
  implicit val string: ConfigDecoder[String] = new ConfigDecoder[String] {
    def apply (value: TracedValue[ConfigValue]) = value.value match {
      case StringValue(s) => Right(s) // TODO - convert other types
      case _ => Left(InvalidType("String", ""))
    }
  }
  
  implicit val int: ConfigDecoder[Int] = new ConfigDecoder[Int] {
    def apply (value: TracedValue[ConfigValue]) = value.value match {
      case LongValue(n) => Right(n.toInt) // TODO - convert other types, check bounds
      case _ => Left(InvalidType("Number", ""))
    }
  }
  
  implicit val configValue: ConfigDecoder[ConfigValue] = new ConfigDecoder[ConfigValue] {
    def apply (value: TracedValue[ConfigValue]) = Right(value.value)
  }
  
  implicit def tracedValue[T](implicit valueDecoder: ConfigDecoder[T]): ConfigDecoder[TracedValue[T]] = new ConfigDecoder[TracedValue[T]] {
    def apply (value: TracedValue[ConfigValue]) = valueDecoder(value).map(res => value.copy(value = res))
  }

  implicit lazy val path: ConfigDecoder[Path] = tracedValue[String].map { tracedValue =>
    val basePath = tracedValue.origins.headOption.fold[Path](Path.Root)(_.path)
    (basePath.parent / Path(tracedValue.value)).relativeTo(Path.Root)
  }
  
  implicit def seq[T] (implicit elementDecoder: ConfigDecoder[T]): ConfigDecoder[Seq[T]] = new ConfigDecoder[Seq[T]] {
    def apply (value: TracedValue[ConfigValue]) = value.value match {
      case ArrayValue(values) => 
        val elements = values.map(v => elementDecoder(TracedValue(v, value.origins)))
        val errors = elements.collect { case Left(e) => e }
        if (errors.nonEmpty) Left(ValidationError(s"One or more errors decoding array elements: ${errors.mkString(", ")}"))
        else Right(elements.collect { case Right(r) => r })
      case _ => Left(InvalidType("Array", ""))
    }
  }
}
