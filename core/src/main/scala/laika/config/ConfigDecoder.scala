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

import laika.ast.{Path, PathBase, RelativePath}

import scala.util.Try

/** A type class that can decode a ConfigValue to an instance of T.
  * 
  * @author Jens Halm
  */
trait ConfigDecoder[T] { self =>

  /** Decode the given traced value.
    * 
    * The object passed to this method is of type `Traced[ConfigValue]`,
    * which passes the `Origin` alongside the actual value. The origin
    * can be used to resolve relative paths and similar tasks.
    */ 
  def apply (value: Traced[ConfigValue]): Either[ConfigError, T]

  def flatMap[U](f: T => Either[ConfigError, U]): ConfigDecoder[U] = new ConfigDecoder[U] {
    def apply (value: Traced[ConfigValue]) = self.apply(value).flatMap(f)
  }

  def map[U](f: T => U): ConfigDecoder[U] = new ConfigDecoder[U] {
    def apply (value: Traced[ConfigValue]) = self.apply(value).map(f)
  }
}

/** Companion containing default decoder implementations for simple values and Seq's.
  */
object ConfigDecoder {

  implicit val string: ConfigDecoder[String] = new ConfigDecoder[String] {
    def apply (value: Traced[ConfigValue]) = value.value match {
      case s: SimpleConfigValue => Right(s.render)
      case invalid => Left(InvalidType("String", invalid))
    }
  }

  implicit val int: ConfigDecoder[Int] = new ConfigDecoder[Int] {
    def apply (value: Traced[ConfigValue]) = value.value match {
      case LongValue(n) => Either.cond(n.isValidInt, n.toInt, DecodingError(s"not a valid integer: $n"))
      case DoubleValue(n) => Either.cond(n.isValidInt, n.toInt, DecodingError(s"not a valid integer: $n"))
      case StringValue(s) => Try(s.toInt).toEither.left.map(_ => DecodingError(s"not an integer: $s"))
      case invalid => Left(InvalidType("Number", invalid))
    }
  }

  implicit val double: ConfigDecoder[Double] = new ConfigDecoder[Double] {
    def apply (value: Traced[ConfigValue]) = value.value match {
      case LongValue(n) => Right(n.toDouble)
      case DoubleValue(n) => Right(n)
      case StringValue(s) => Try(s.toDouble).toEither.left.map(_ => DecodingError(s"not a double: $s"))
      case invalid => Left(InvalidType("Number", invalid))
    }
  }

  implicit val configValue: ConfigDecoder[ConfigValue] = new ConfigDecoder[ConfigValue] {
    def apply (value: Traced[ConfigValue]) = Right(value.value)
  }

  implicit def tracedValue[T](implicit valueDecoder: ConfigDecoder[T]): ConfigDecoder[Traced[T]] = new ConfigDecoder[Traced[T]] {
    def apply (value: Traced[ConfigValue]) = valueDecoder(value).map(res => value.copy(value = res))
  }

  implicit lazy val path: ConfigDecoder[Path] = tracedValue[String].map { tracedValue =>
    PathBase.decode(tracedValue.value) match {
      case p: RelativePath => tracedValue.origin.path.parent / p // TODO - 0.14 - distinguish between tree and doc origins
      case p: Path => p
    }
  }

  implicit def seq[T] (implicit elementDecoder: ConfigDecoder[T]): ConfigDecoder[Seq[T]] = new ConfigDecoder[Seq[T]] {
    def apply (value: Traced[ConfigValue]) = value.value match {
      case ArrayValue(values) =>
        val elements = values.map(v => elementDecoder(Traced(v, value.origin)))
        val errors = elements.collect { case Left(e) => e }
        if (errors.nonEmpty) Left(DecodingError(s"One or more errors decoding array elements: ${errors.mkString(", ")}"))
        else Right(elements.collect { case Right(r) => r })
      case invalid => Left(InvalidType("Array", invalid))
    }
  }
}
