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

package laika.rewrite

import laika.config.{ConfigDecoder, ConfigEncoder, DefaultKey, LaikaKeys}

case class Version (displayValue: String, pathSegment: String, defaultLinkTarget: String = "index.html", label: Option[String] = None)

object Version {

  implicit val decoder: ConfigDecoder[Version] = ConfigDecoder.config.flatMap { config =>
    for {
      displayName        <- config.get[String]("displayValue")
      pathSegment        <- config.get[String]("pathSegment")
      defaultLinkTarget  <- config.get[String]("defaultLinkTarget", "index.html")
      label              <- config.getOpt[String]("label")
    } yield {
      Version(displayName, pathSegment, defaultLinkTarget, label)
    }
  }

  implicit val encoder: ConfigEncoder[Version] = ConfigEncoder[Version] { version =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("displayValue", version.displayValue)
      .withValue("pathSegment", version.pathSegment)
      .withValue("defaultLinkTarget", version.defaultLinkTarget)
      .withValue("label", version.label)
      .build
  }
  
}

case class Versions (currentVersion: Version, olderVersions: Seq[Version], newerVersions: Seq[Version] = Nil) {
  
  def allVersions: Seq[Version] = newerVersions ++: currentVersion +: olderVersions
  
}

object Versions {
  
  implicit val key: DefaultKey[Versions] = DefaultKey(LaikaKeys.versions)

  implicit val decoder: ConfigDecoder[Versions] = ConfigDecoder.config.flatMap { config =>
    for {
      currentVersion <- config.get[Version]("currentVersion")
      olderVersions  <- config.get[Seq[Version]]("olderVersions")
      newerVersions  <- config.get[Seq[Version]]("newerVersions")
    } yield {
      Versions(currentVersion, olderVersions, newerVersions)
    }
  }

  implicit val encoder: ConfigEncoder[Versions] = ConfigEncoder[Versions] { versions =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("currentVersion", versions.currentVersion)
      .withValue("olderVersions", versions.olderVersions)
      .withValue("newerVersions", versions.newerVersions)
      .build
  }
  
}
