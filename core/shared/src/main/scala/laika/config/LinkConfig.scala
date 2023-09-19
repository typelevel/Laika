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

import laika.api.config.ConfigError.ValidationFailed
import laika.api.config.{ ConfigDecoder, ConfigEncoder, DefaultKey }
import laika.ast.{ ExternalTarget, InternalTarget, Path, Target, VirtualPath }

sealed abstract class LinkConfig {

  /** List of global link definitions, mapping an identifier to an internal or external target.
    *
    * Allows to centralize commonly used URLs and associate them with an id that can be used
    * in markup sources, avoiding the repetitive definition of those URLs in the markup.
    *
    * The use of these ids in markup does not require a directive, it can be used with
    * "native" markup syntax, e.g. `[linkText][linkId]` in Markdown where `linkId` is
    * the identifier defined here.
    */
  def targets: Seq[TargetDefinition]

  /** Defines a list of base URLs for API links which allows the use
    * of the `@:api` directive in markup.
    *
    * Different base URLs can be defined for different packages,
    * so that fully qualified class names point to the correct external sources.
    * In markup it is then sufficient to pass the fully qualified class name
    * to the directive (e.g. `@:api(com.foo.Person)`)
    */
  def apiLinks: Seq[ApiLinks]

  /** Defines a list of base URLs for links to the sources of referenced classes
    * which allows the use of the `@:source` directive in markup.
    *
    * Different base URLs can be defined for different packages,
    * so that fully qualified class names or a relative path to a markup source file
    * point to the correct external sources.
    * In markup it is then sufficient to pass the fully qualified class name
    * to the directive (e.g. `@:source(com.foo.Person)`) or, when pointing to
    * markup sources, its relative path (e.g. `@:source(setup/intro.md)`)
    */
  def sourceLinks: Seq[SourceLinks]

  def addTargets(newTargets: TargetDefinition*): LinkConfig

  def addApiLinks(newLinks: ApiLinks*): LinkConfig

  def addSourceLinks(newLinks: SourceLinks*): LinkConfig

}

object LinkConfig {

  private final case class Impl(
      targets: Seq[TargetDefinition],
      apiLinks: Seq[ApiLinks],
      sourceLinks: Seq[SourceLinks]
  ) extends LinkConfig {

    override def productPrefix: String = "LinkConfig"

    def addTargets(newTargets: TargetDefinition*): LinkConfig =
      copy(targets = targets ++ newTargets)

    def addApiLinks(newLinks: ApiLinks*): LinkConfig = copy(apiLinks = apiLinks ++ newLinks)

    def addSourceLinks(newLinks: SourceLinks*): LinkConfig =
      copy(sourceLinks = sourceLinks ++ newLinks)

  }

  val empty: LinkConfig = Impl(Nil, Nil, Nil)

  implicit val key: DefaultKey[LinkConfig] = DefaultKey(LaikaKeys.links)

  implicit val decoder: ConfigDecoder[LinkConfig] = ConfigDecoder.config.flatMap { config =>
    for {
      targets     <- config.get[Map[String, String]]("targets", Map.empty[String, String])
      apiLinks    <- config.get[Seq[ApiLinks]]("api", Nil)
      sourceLinks <- config.get[Seq[SourceLinks]]("source", Nil)
    } yield {
      val mappedTargets = targets.map { case (id, targetURL) =>
        TargetDefinition(id, Target.parse(targetURL))
      }
      Impl(mappedTargets.toSeq, apiLinks, sourceLinks)
    }
  }

  implicit val encoder: ConfigEncoder[LinkConfig] = ConfigEncoder[LinkConfig] { config =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("targets", config.targets.map(t => (t.id, t.target.render())).toMap)
      .withValue("api", config.apiLinks)
      .withValue("source", config.sourceLinks)
      .build
  }

}

/** Represents configuration options for link validation. */
sealed trait LinkValidation

object LinkValidation {

  /** Completely disables any kind of link validation */
  case object Off extends LinkValidation

  /** Only validates link target within the same document.
    * The default when using the `laika-core` transformer with a single input string.
    */
  case object Local extends LinkValidation

  /** Validates link targets within the same document and in other documents.
    * The default when using the sbt plugin or the `laika-io` transformer APIs.
    *
    * @param excluded one or more paths to link targets that should be excluded from validation -
    *                 the values apply recursively and include subdirectories
    */
  case class Global(excluded: Seq[Path] = Nil) extends LinkValidation

  private val keyValue                                         = LaikaKeys.links.child("validation")
  implicit val key: DefaultKey[LinkValidation]                 = DefaultKey(keyValue)
  implicit val globalKey: DefaultKey[LinkValidation.Global]    = DefaultKey(keyValue)
  implicit val localKey: DefaultKey[LinkValidation.Local.type] = DefaultKey(keyValue)
  implicit val offKey: DefaultKey[LinkValidation.Off.type]     = DefaultKey(keyValue)

  implicit val decoder: ConfigDecoder[LinkValidation] = ConfigDecoder.config.flatMap { config =>
    val result = for {
      scope    <- config.getOpt[String]("scope")
      excluded <- config.get[Seq[Path]]("excluded", Nil)
    } yield (scope, excluded)
    result.flatMap {
      case (Some("global"), excluded) => Right(Global(excluded))
      case (Some("local"), _)         => Right(Local)
      case (Some("off"), _)           => Right(Off)
      case (None, _)                  => Left(ValidationFailed(s"scope not specified"))
      case (Some(unknown), _) => Left(ValidationFailed(s"Unsupported value for scope: $unknown"))
    }
  }

  implicit val encoder: ConfigEncoder[LinkValidation] = ConfigEncoder[LinkValidation] { config =>
    val (scope, excluded) = config match {
      case Global(excluded) => ("global", excluded)
      case Local            => ("local", Nil)
      case Off              => ("off", Nil)
    }
    ConfigEncoder.ObjectBuilder.empty
      .withValue("scope", scope)
      .withValue("excluded", excluded)
      .build
  }

}

sealed abstract class TargetDefinition {
  def id: String
  def target: Target
}

object TargetDefinition {

  private final case class Impl(id: String, target: Target) extends TargetDefinition {
    override def productPrefix: String = "TargetDefinition"
  }

  private[config] def apply(id: String, target: Target): TargetDefinition = Impl(id, target)

  def external(id: String, uri: String): TargetDefinition = Impl(id, ExternalTarget(uri))

  def internal(id: String, path: VirtualPath): TargetDefinition = Impl(id, InternalTarget(path))
}

sealed abstract class SourceLinks {
  def baseUri: String
  def suffix: String
  def packagePrefix: String

  def withPackagePrefix(prefix: String): SourceLinks
}

object SourceLinks {

  private final case class Impl(baseUri: String, suffix: String, packagePrefix: String)
      extends SourceLinks {
    override def productPrefix: String                 = "SourceLinks"
    def withPackagePrefix(prefix: String): SourceLinks = copy(packagePrefix = prefix)
  }

  def apply(baseUri: String, suffix: String): SourceLinks = Impl(baseUri, suffix, "*")

  implicit val decoder: ConfigDecoder[SourceLinks] = ConfigDecoder.config.flatMap { config =>
    for {
      baseUri <- config.get[String]("baseUri")
      prefix  <- config.get[String]("packagePrefix", "*")
      suffix  <- config.get[String]("suffix")
    } yield {
      Impl(baseUri, suffix, prefix)
    }
  }

  implicit val encoder: ConfigEncoder[SourceLinks] = ConfigEncoder[SourceLinks] { links =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("baseUri", links.baseUri)
      .withValue("packagePrefix", links.packagePrefix)
      .withValue("suffix", links.suffix)
      .build
  }

}

sealed abstract class ApiLinks {
  def baseUri: String
  def packagePrefix: String
  def packageSummary: String

  def withPackagePrefix(prefix: String): ApiLinks
  def withPackageSummary(prefix: String): ApiLinks
}

object ApiLinks {

  private final case class Impl(
      baseUri: String,
      packagePrefix: String,
      packageSummary: String
  ) extends ApiLinks {

    override def productPrefix: String = "ApiLinks"

    def withPackagePrefix(prefix: String): ApiLinks = copy(packagePrefix = prefix)

    def withPackageSummary(summary: String): ApiLinks = copy(packageSummary = summary)
  }

  def apply(baseUri: String): ApiLinks = Impl(baseUri, "*", "index.html")

  implicit val decoder: ConfigDecoder[ApiLinks] = ConfigDecoder.config.flatMap { config =>
    for {
      baseUri <- config.get[String]("baseUri")
      prefix  <- config.get[String]("packagePrefix", "*")
      summary <- config.get[String]("packageSummary", "index.html")
    } yield {
      Impl(baseUri, prefix, summary)
    }
  }

  implicit val encoder: ConfigEncoder[ApiLinks] = ConfigEncoder[ApiLinks] { links =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("baseUri", links.baseUri)
      .withValue("packagePrefix", links.packagePrefix)
      .withValue("packageSummary", links.packageSummary)
      .build
  }

}
