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

import laika.ast.Path
import laika.config.{ConfigDecoder, ConfigEncoder, DefaultKey, LaikaKeys}

/** Configuration for a single version of the documentation.
  * 
  * @param displayValue the description of the version to use in any UI (e.g. version dropdowns)
  * @param pathSegment the string to use as a path segments in URLs pointing to this version
  * @param fallbackLink the link target to use when switching to this version from a page that does not exist in this version
  * @param label an optional label that will be used in the UI (e.g. `Dev` or `Stable`) 
  */
case class Version (displayValue: String, pathSegment: String, fallbackLink: String = "index.html", label: Option[String] = None)

object Version {

  implicit val decoder: ConfigDecoder[Version] = ConfigDecoder.config.flatMap { config =>
    for {
      displayName   <- config.get[String]("displayValue")
      pathSegment   <- config.get[String]("pathSegment")
      fallbackLink  <- config.get[String]("fallbackLink", "index.html")
      label         <- config.getOpt[String]("label")
    } yield {
      Version(displayName, pathSegment, fallbackLink, label)
    }
  }

  implicit val encoder: ConfigEncoder[Version] = ConfigEncoder[Version] { version =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("displayValue", version.displayValue)
      .withValue("pathSegment", version.pathSegment)
      .withValue("fallbackLink", version.fallbackLink)
      .withValue("label", version.label)
      .build
  }
  
}

/** Global configuration for versioned documentation. 
  * 
  * The order in the `Seq` properties will be used for any list views in the UI (e.g. for the version chooser dropdown).
  * 
  * @param currentVersion the version that the sources of a transformation produce
  * @param olderVersions list of older versions that have previously been rendered (may be empty)
  * @param newerVersions list of newer versions that have previously been rendered (may be empty)
  * @param excludeFromScanning paths to be skipped when scanning the output directory for existing versions (e.g. for API docs)
  * @param renderUnversioned indicates whether unversioned documents should be rendered 
  *                          (setting this to false may be useful when re-rendering older versions)
  */
case class Versions (currentVersion: Version, 
                     olderVersions: Seq[Version], 
                     newerVersions: Seq[Version] = Nil, 
                     excludeFromScanning: Seq[Path] = Nil,
                     renderUnversioned: Boolean = true) {
  
  def allVersions: Seq[Version] = newerVersions ++: currentVersion +: olderVersions
  
}

object Versions {
  
  implicit val key: DefaultKey[Versions] = DefaultKey(LaikaKeys.versions)

  implicit val decoder: ConfigDecoder[Versions] = ConfigDecoder.config.flatMap { config =>
    for {
      currentVersion      <- config.get[Version]("currentVersion")
      olderVersions       <- config.get[Seq[Version]]("olderVersions", Nil)
      newerVersions       <- config.get[Seq[Version]]("newerVersions", Nil)
      excludeFromScanning <- config.get[Seq[Path]]("excludeFromScanning", Nil)
      renderUnversioned   <- config.get[Boolean]("renderUnversioned", false)
    } yield {
      Versions(currentVersion, olderVersions, newerVersions, excludeFromScanning, renderUnversioned)
    }
  }

  implicit val encoder: ConfigEncoder[Versions] = ConfigEncoder[Versions] { versions =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("currentVersion", versions.currentVersion)
      .withValue("olderVersions", versions.olderVersions)
      .withValue("newerVersions", versions.newerVersions)
      .withValue("excludeFromScanning", versions.excludeFromScanning)
      .withValue("renderUnversioned", versions.renderUnversioned)
      .build
  }
  
}
