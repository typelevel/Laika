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

import laika.config.Config.ConfigResult
import laika.parse.hocon.{ IncludeResource, ObjectBuilderValue }

import scala.util.Try

/** API for retrieving configuration values based on a string key and a decoder.
  *
  * Config instances are used in many places in this library, each `Document`,
  * `DocumentTree` and `Directive` has a Config instance associated with it.
  *
  * One use case for configuration is controlling the behaviour of built-in features,
  * like setting the navigation order or the depth for table of contents.
  *
  * A second use case is user configuration, where custom variables can be set
  * in configuration files or headers and then referenced in templates or markup
  * with the syntax `\${ref.path}`.
  *
  * The key is a path separated by '.', which allows to reference nested objects
  * in the configuration.
  *
  * Built-in decoders are available for simple types like `String`, `Int`, `Double`, `Boolean`
  * and any `Seq` consisting of those values.
  *
  * It also comes with a decoder for `Path`, which resolves relative paths in the configuration
  * against the (virtual) path of the origin.
  *
  * This API is usually used with values obtained by parsing HOCON, as specified in
  * [[https://github.com/lightbend/config/blob/master/HOCON.md]], but the API is generic
  * and can also be used with values specified programmatically.
  *
  * Please note that Laika does not depend on the Typesafe Config library or any of its
  * commonly used Scala wrappers or forks. It has its own HOCON parser, which implements
  * the full spec while still being minimal and lightweight. It also ensures the FP
  * properties are kept intact, e.g. it has full referential transparency and does
  * not throw Exceptions like most of the alternatives.
  *
  * @author Jens Halm
  */
trait Config {

  /** The origin of this configuration, which might come from programmatic configuration,
    * a file in one of the input directories, a configuration header in a markup document
    * or from an attribute section in a directive.
    */
  def origin: Origin

  /** Verifies whether this config instance contains a value mapped to the specified key.
    */
  def hasKey(key: String): Boolean = hasKey(Key.parse(key))

  /** Verifies whether this config instance contains a value mapped to the specified key.
    */
  def hasKey(key: Key): Boolean

  /** Retrieve a required value for the specified key and decoder.
    */
  def get[T](key: Key)(implicit decoder: ConfigDecoder[T]): ConfigResult[T]

  /** Retrieve a required value for the specified key and decoder.
    */
  def get[T](key: String)(implicit decoder: ConfigDecoder[T]): ConfigResult[T] =
    get[T](Key.parse(key))

  /** Retrieve an optional value for the specified key and decoder, falling back to the
    * given default if the value is missing.
    */
  def get[T](key: Key, default: => T)(implicit decoder: ConfigDecoder[T]): ConfigResult[T] =
    getOpt(key).map(_.getOrElse(default))

  /** Retrieve an optional value for the specified key and decoder, falling back to the
    * given default if the value is missing.
    */
  def get[T](key: String, default: => T)(implicit decoder: ConfigDecoder[T]): ConfigResult[T] =
    get[T](Key.parse(key), default)

  /** Retrieve an optional value for the specified key and decoder.
    * The result is still an Either as this method might still fail even if the value is present in
    * case the decoding fails.
    */
  def getOpt[T](key: Key)(implicit decoder: ConfigDecoder[T]): ConfigResult[Option[T]] =
    get(key).fold(
      e => if (e.isInstanceOf[NotFound]) Right(None) else Left(e),
      r => Right(Some(r))
    )

  /** Retrieve an optional value for the specified implicit key and decoder.
    *
    * A defaultKey can be used for commonly used configuration objects like `AutonumberConfig`
    * that are expected to be mapped to a specific key, like `autonumbering`.
    *
    * The result is still an Either as this method might still fail even if the value is present in
    * case the decoding fails.
    */
  def getOpt[T](implicit
      decoder: ConfigDecoder[T],
      defaultKey: DefaultKey[T]
  ): ConfigResult[Option[T]] =
    getOpt(defaultKey.value)

  /** Retrieve an optional value for the specified key and decoder.
    * The result is still an Either as this method might still fail even if the value is present in
    * case the decoding fails.
    */
  def getOpt[T](key: String)(implicit decoder: ConfigDecoder[T]): ConfigResult[Option[T]] =
    getOpt[T](Key.parse(key))

  /** Retrieve a required value for the specified implicit key and decoder.
    *
    * A defaultKey can be used for commonly used configuration objects like `AutonumberConfig`
    * that are expected to be mapped to a specific key, like `autonumbering`.
    */
  def get[T](implicit decoder: ConfigDecoder[T], defaultKey: DefaultKey[T]): ConfigResult[T] =
    get[T](defaultKey.value)

  /** Creates a new configuration builder with the specified value and this instance as
    * a fallback. The returned builder can be used to add further values before calling
    * `build` to retrieve a new instance.
    */
  def withValue[T](key: String, value: T)(implicit encoder: ConfigEncoder[T]): ConfigBuilder =
    ConfigBuilder.withFallback(this).withValue(key, value)

  /** Creates a new configuration builder with the specified value and this instance as
    * a fallback. The returned builder can be used to add further values before calling
    * `build` to retrieve a new instance.
    */
  def withValue[T](key: Key, value: T)(implicit encoder: ConfigEncoder[T]): ConfigBuilder =
    ConfigBuilder.withFallback(this).withValue(key, value)

  /** Creates a new configuration builder with the specified value and this instance as
    * a fallback. The returned builder can be used to add further values before calling
    * `build` to retrieve a new instance.
    */
  def withValue[T](
      value: T
  )(implicit encoder: ConfigEncoder[T], defaultKey: DefaultKey[T]): ConfigBuilder =
    ConfigBuilder.withFallback(this).withValue(value)

  /** Returns a new configuration instance using the specified instance as a fallback
    * for keys not found in this instance.
    *
    * If this instance already has a fallback, the new fallback will be passed further down the chain.
    */
  def withFallback(other: Config): Config

  /** Returns a new configuration instance using the specified origin.
    */
  def withOrigin(origin: Origin): Config

  private[laika] def withoutFallback: Config

}

/** The default implementation of the Config API.
  */
private[laika] class ObjectConfig(
    val root: ObjectValue,
    val origin: Origin,
    val fallback: Config = EmptyConfig
) extends Config {

  private def lookup(
      keySegments: Seq[String],
      target: ArrayValue,
      targetOrigin: Origin
  ): Option[Field] = {
    Try(keySegments.head.toInt).toOption.flatMap { posKey =>
      ((target.values.drop(posKey).headOption, keySegments.tail) match {
        case (res, Nil)                    => res.map(Field("", _, targetOrigin))
        case (Some(ov: ObjectValue), rest) => lookup(rest, ov)
        case _                             => None
      }): Option[Field]
    }
  }

  private def lookup(keySegments: Seq[String], target: ObjectValue): Option[Field] = {
    (target.values.find(_.key == keySegments.head), keySegments.tail) match {
      case (res, Nil)                                          => res
      case (Some(Field(_, av: ArrayValue, fieldOrigin)), rest) => lookup(rest, av, fieldOrigin)
      case (Some(Field(_, ov: ObjectValue, _)), rest)          => lookup(rest, ov)
      case _                                                   => None
    }
  }

  private def lookup(key: Key): Option[Field] =
    if (key.segments.isEmpty) Some(Field("", root, origin)) else lookup(key.segments, root)

  def hasKey(key: Key): Boolean = lookup(key).nonEmpty || fallback.hasKey(key)

  def get[T](key: Key)(implicit decoder: ConfigDecoder[T]): ConfigResult[T] = {
    lookup(key).fold(fallback.get[T](key)) { field =>
      val res = field match {
        case Field(_, ov: ObjectValue, _) =>
          fallback.get[ConfigValue](key).toOption match {
            case Some(parentOv: ObjectValue) => ov.merge(parentOv)
            case _                           => ov
          }
        case _                            => field.value
      }
      decoder(Traced(res, field.origin)).left.map {
        case de @ DecodingError(_, child) => de.withKey(child.fold(key)(key.child))
        case other                        => other
      }
    }
  }

  def withFallback(other: Config): Config = other match {
    case EmptyConfig => this
    case _           => new ObjectConfig(root, origin, fallback.withFallback(other))
  }

  private[laika] def withoutFallback: Config = new ObjectConfig(root, origin)

  def withOrigin(newOrigin: Origin): Config = new ObjectConfig(root, newOrigin, fallback)

  override def hashCode: Int = (root, origin, fallback).hashCode

  override def equals(obj: Any): Boolean = obj match {
    case c: ObjectConfig => (c.root, c.origin, c.fallback).equals((root, origin, fallback))
    case _               => false
  }

}

/** An empty configuration instance.
  */
private[config] object EmptyConfig extends Config {

  val origin: Origin = Origin.root

  def hasKey(key: Key): Boolean = false

  def get[T](key: Key)(implicit decoder: ConfigDecoder[T]): ConfigResult[T] = Left(NotFound(key))

  def withFallback(other: Config): Config = other

  private[laika] def withoutFallback: Config = this

  def withOrigin(newOrigin: Origin): Config = this
}

object Config {

  type ConfigResult[T] = Either[ConfigError, T]

  private[laika] type IncludeMap = Map[IncludeResource, Either[ConfigError, ObjectBuilderValue]]

  val empty: Config = EmptyConfig

}
