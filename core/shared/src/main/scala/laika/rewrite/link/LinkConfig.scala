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

package laika.rewrite.link

import laika.ast.{ Path, Target }
import laika.config._

/** @author Jens Halm
  */
case class LinkConfig(
    targets: Seq[TargetDefinition] = Nil,
    excludeFromValidation: Seq[Path] = Nil,
    apiLinks: Seq[ApiLinks] = Nil,
    sourceLinks: Seq[SourceLinks] = Nil
)

object LinkConfig {

  val empty: LinkConfig = LinkConfig(Nil, Nil, Nil)

  implicit val key: DefaultKey[LinkConfig] = DefaultKey(LaikaKeys.links)

  implicit val decoder: ConfigDecoder[LinkConfig] = ConfigDecoder.config.flatMap { config =>
    for {
      targets     <- config.get[Map[String, String]]("targets", Map.empty[String, String])
      exclude     <- config.get[Seq[Path]]("excludeFromValidation", Nil)
      apiLinks    <- config.get[Seq[ApiLinks]]("api", Nil)
      sourceLinks <- config.get[Seq[SourceLinks]]("source", Nil)
    } yield {
      val mappedTargets = targets.map { case (id, targetURL) =>
        TargetDefinition(id, Target.parse(targetURL))
      }
      LinkConfig(mappedTargets.toSeq, exclude, apiLinks, sourceLinks)
    }
  }

  implicit val encoder: ConfigEncoder[LinkConfig] = ConfigEncoder[LinkConfig] { config =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("targets", config.targets.map(t => (t.id, t.target.render())).toMap)
      .withValue("excludeFromValidation", config.excludeFromValidation)
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
      case (None, _)                  => Left(ValidationError(s"scope not specified"))
      case (Some(unknown), _) => Left(ValidationError(s"Unsupported value for scope: $unknown"))
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

case class TargetDefinition(id: String, target: Target)

case class SourceLinks(baseUri: String, suffix: String, packagePrefix: String = "*")

object SourceLinks {

  implicit val decoder: ConfigDecoder[SourceLinks] = ConfigDecoder.config.flatMap { config =>
    for {
      baseUri <- config.get[String]("baseUri")
      prefix  <- config.get[String]("packagePrefix", "*")
      suffix  <- config.get[String]("suffix")
    } yield {
      SourceLinks(baseUri, suffix, prefix)
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

case class ApiLinks(
    baseUri: String,
    packagePrefix: String = "*",
    packageSummary: String = "index.html"
)

object ApiLinks {

  implicit val decoder: ConfigDecoder[ApiLinks] = ConfigDecoder.config.flatMap { config =>
    for {
      baseUri <- config.get[String]("baseUri")
      prefix  <- config.get[String]("packagePrefix", "*")
      summary <- config.get[String]("packageSummary", "index.html")
    } yield {
      ApiLinks(baseUri, prefix, summary)
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
