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

import laika.ast.{Path, Target}
import laika.config._

/**
  * @author Jens Halm
  */
case class LinkConfig (targets: Seq[TargetDefinition] = Nil, 
                       excludeFromValidation: Seq[Path] = Nil,
                       apiLinks: Seq[ApiLinks] = Nil,
                       sourceLinks: Seq[SourceLinks] = Nil)

object LinkConfig {
  
  val empty: LinkConfig = LinkConfig(Nil, Nil, Nil)
  
  implicit val key: DefaultKey[LinkConfig] = DefaultKey(LaikaKeys.links)
  
  implicit val decoder: ConfigDecoder[LinkConfig] = ConfigDecoder.config.flatMap { config =>
    for {
      targets  <- config.get[Map[String, String]]("targets", Map.empty[String,String])
      exclude  <- config.get[Seq[Path]]("excludeFromValidation", Nil)
      apiLinks <- config.get[Seq[ApiLinks]]("api", Nil)
      sourceLinks <- config.get[Seq[SourceLinks]]("source", Nil)
    } yield {
      val mappedTargets = targets.map {
        case (id, targetURL) => TargetDefinition(id, Target.parse(targetURL))
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

case class TargetDefinition (id: String, target: Target)

case class SourceLinks (baseUri: String, suffix: String, packagePrefix: String = "*")

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

case class ApiLinks (baseUri: String, packagePrefix: String = "*", packageSummary: String = "index.html")

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
