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

import laika.config.Config.IncludeMap
import laika.parse.{ Failure, Success }
import laika.parse.hocon.{
  BuilderField,
  ConfigResolver,
  HoconParsers,
  IncludeBuilderValue,
  IncludeResource,
  ObjectBuilderValue
}

/** A parser for obtaining a Config instance from a HOCON string.
  *
  * The HOCON format expected by this parsers is specified at
  * [[https://github.com/lightbend/config/blob/master/HOCON.md]]
  *
  * @author Jens Halm
  */
trait ConfigParser {

  /** Extracts all unresolved requested includes the parsed configuration contains,
    * including those nested inside other objects.
    *
    * Since the pure core module does not support effectful IO, this hook
    * can be used by a higher level API to resolve includes and pass them
    * to the `resolve` method.
    */
  def includes: Seq[IncludeResource]

  /** The unresolved result of the parser which can be used
    * for processing inclusions that will be resolved in the
    * context of the hosting configuration.
    */
  def unresolved: Either[ConfigError, ObjectBuilderValue]

  /** Parses and resolves the parsed HOCON input.
    * This includes the resolving of substitution references
    * and the merging of concatenated objects, arrays and strings.
    *
    * Failures may be caused by both, the parser and resolver step.
    *
    * The specified origin will be attached to every field.
    * Origins can be used to distinguish values from a specific Config
    * instance from those which were inherited from a fallback, which
    * might be relevant in scenarios where relative paths need to be
    * resolved for example.
    *
    * The specified fallback will be used for resolving keys which are
    * not present in the configuration created by this parser.
    *
    * If an entire object is requested in the resulting Config instance,
    * the keys will be merged from this parser with those present in the fallback.
    * Simple values on the other hand will always override values with the same
    * key in the fallback.
    *
    * The specified includes contain all resources that could be loaded
    * based on the requested resources exposed by the `includes` property.
    * Keeping the actual loading separate allows the resource loading step
    * to be separate and in a module designed for effectful operations.
    */
  def resolve(
      origin: Origin = Origin.root,
      fallback: Config = EmptyConfig,
      includes: IncludeMap = ConfigParser.includesNotSupported
  ): Either[ConfigError, Config]

}

object ConfigParser {

  private[laika] val includesNotSupported: IncludeMap =
    Map.empty.withDefaultValue(
      Left(
        ConfigResourceError(
          "Loading of includes is not supported in pure mode, use parsers or transformers in laika-io for this purpose."
        )
      )
    )

  /** Creates a new parser for the specified HOCON input.
    */
  def parse(input: String): ConfigParser = new ConfigParser {

    lazy val unresolved: Either[ConfigError, ObjectBuilderValue] =
      HoconParsers.rootObject.parse(input) match {
        case Success(builderRoot, _) => Right(builderRoot)
        case f: Failure              => Left(ConfigParserError(f))
      }

    lazy val includes: Seq[IncludeResource] = {

      def extractIncludes(obj: ObjectBuilderValue): Seq[IncludeResource] = obj.values.flatMap {
        case BuilderField(_, IncludeBuilderValue(resource)) => Seq(resource)
        case BuilderField(_, child: ObjectBuilderValue)     => extractIncludes(child)
        case _                                              => Nil
      }

      unresolved.fold(_ => Nil, extractIncludes)
    }

    def resolve(
        origin: Origin = Origin.root,
        fallback: Config = EmptyConfig,
        includes: IncludeMap = ConfigParser.includesNotSupported
    ): Either[ConfigError, Config] =
      unresolved.flatMap(
        ConfigResolver
          .resolve(_, origin, fallback, includes)
          .map(new ObjectConfig(_, origin, fallback))
      )

  }

  /** Creates a new parser that will produce an empty result.
    */
  def empty: ConfigParser = new ConfigParser {

    val includes: Seq[IncludeResource] = Nil

    val unresolved: Either[ConfigError, ObjectBuilderValue] = Right(ObjectBuilderValue(Nil))

    def resolve(
        origin: Origin = Origin.root,
        fallback: Config = EmptyConfig,
        includes: IncludeMap = ConfigParser.includesNotSupported
    ): Either[ConfigError, Config] =
      Right(ConfigBuilder.withFallback(fallback, origin).build)

  }

}
