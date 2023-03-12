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

import cats.syntax.all._
import cats.data.NonEmptyChain
import laika.ast.Path
import laika.config.{
  ConfigDecoder,
  ConfigEncoder,
  ConfigErrors,
  DefaultKey,
  LaikaKeys,
  ValidationError
}

/** Configuration for a single version of the documentation.
  *
  * @param displayValue the description of the version to use in any UI (e.g. version dropdowns)
  * @param pathSegment the string to use as a path segments in URLs pointing to this version
  * @param fallbackLink the link target to use when switching to this version from a page that does not exist in this version
  * @param label an optional label that will be used in the UI (e.g. `Dev` or `Stable`)
  * @param canonical indicates whether this is the canonical version
  */
case class Version(
    displayValue: String,
    pathSegment: String,
    fallbackLink: String = "index.html",
    label: Option[String] = None,
    canonical: Boolean = false
)

object Version {

  implicit val decoder: ConfigDecoder[Version] = ConfigDecoder.config.flatMap { config =>
    for {
      displayName  <- config.get[String]("displayValue")
      pathSegment  <- config.get[String]("pathSegment")
      canonical    <- config.get[Boolean]("canonical", false)
      fallbackLink <- config.get[String]("fallbackLink", "index.html")
      label        <- config.getOpt[String]("label")
    } yield {
      Version(displayName, pathSegment, fallbackLink, label, canonical)
    }
  }

  implicit val encoder: ConfigEncoder[Version] = ConfigEncoder[Version] { version =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("displayValue", version.displayValue)
      .withValue("pathSegment", version.pathSegment)
      .withValue("fallbackLink", version.fallbackLink)
      .withValue("label", version.label)
      .withValue("canonical", version.canonical)
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
  * @param renderUnversioned indicates whether unversioned documents should be rendered
  *                          (setting this to false may be useful when re-rendering older versions)
  * @param scannerConfig optional configuration for scanning and indexing existing versions,
  *                       used by the Helium version switcher dropdown and by the preview server.
  */
case class Versions(
    currentVersion: Version,
    olderVersions: Seq[Version],
    newerVersions: Seq[Version] = Nil,
    renderUnversioned: Boolean = true,
    scannerConfig: Option[VersionScannerConfig] = None
) {

  lazy val allVersions: Seq[Version] = newerVersions ++: currentVersion +: olderVersions

  /** Configures the version scanner to use during transformations.
    * These settings enable scanning and indexing existing versions during a transformation,
    * used by the Helium version switcher dropdown and by the preview server.
    */
  def withVersionScanner(rootDirectory: String, exclude: Seq[Path]): Versions =
    copy(scannerConfig = Some(VersionScannerConfig(rootDirectory, exclude)))

  /** Validates this configuration instance and either returns a `Left` with a
    * list of errors encountered or a `Right` containing this instance.
    */
  def validated: Either[NonEmptyChain[String], Versions] = {

    val dupSegments    = allVersions.groupBy(_.pathSegment).filter(_._2.size > 1).keys.toList.sorted
    val dupSegmentsMsg =
      if (dupSegments.isEmpty) None
      else Some(s"Path segments used for more than one version: ${dupSegments.mkString(", ")}")

    val dupCanonical    = allVersions.filter(_.canonical).map(_.displayValue).toList.sorted
    val dupCanonicalMsg =
      if (dupCanonical.size < 2) None
      else Some(s"More than one version marked as canonical: ${dupCanonical.mkString(", ")}")

    NonEmptyChain.fromSeq(dupSegmentsMsg.toList ++ dupCanonicalMsg.toList) match {
      case Some(chain) => Left(chain)
      case None        => Right(this)
    }
  }

}

/** Optional configuration for scanning existing versions that had been generated by a different tool.
  * This setting is optional and serves two purposes:
  *
  * - If older versions of the documentation had been generated by different tools,
  *   Laika can use this configuration for indexing the available paths for "smart linking",
  *   which is a feature of the Helium version switcher drop down.
  *   The result of the scanning operation will be used to populate the file `/laika/versionInfo.json`
  *   in the output directory.
  *
  * - If you are using the preview server (either the `laikaPreview` task in the sbt plugin,
  *   or the `laika.preview.ServerBuilder` from the API) and you want to also test the version switcher
  *   drop down menu, then Laika needs to know where older versions are located so that it can serve them, too.
  *
  * The specified root directory is expected to match the structure of versioned documentation as rendered by Laika.
  * This means that the root directory is expected to have immediate sub-directories with names that correspond
  * to the `pathSegment` property of the configuration for that version.
  *
  * @param rootDirectory file system path that represents the root of existing versions.
  * @param exclude paths to be skipped when scanning the output directory for existing versions (e.g. for API docs),
  *                interpreted from the root directory of each version.
  */
case class VersionScannerConfig(rootDirectory: String, exclude: Seq[Path] = Nil)

object VersionScannerConfig {

  implicit val decoder: ConfigDecoder[VersionScannerConfig] = ConfigDecoder.config.flatMap {
    config =>
      for {
        rootDirectory <- config.get[String]("rootDirectory")
        exclude       <- config.get[Seq[Path]]("exclude", Nil)
      } yield {
        VersionScannerConfig(rootDirectory, exclude)
      }
  }

  implicit val encoder: ConfigEncoder[VersionScannerConfig] = ConfigEncoder[VersionScannerConfig] {
    config =>
      ConfigEncoder.ObjectBuilder.empty
        .withValue("rootDirectory", config.rootDirectory)
        .withValue("exclude", config.exclude)
        .build
  }

}

object Versions {

  implicit val key: DefaultKey[Versions] = DefaultKey(LaikaKeys.versions)

  implicit val decoder: ConfigDecoder[Versions] = ConfigDecoder.config.flatMap { config =>
    for {
      currentVersion    <- config.get[Version]("currentVersion")
      olderVersions     <- config.get[Seq[Version]]("olderVersions", Nil)
      newerVersions     <- config.get[Seq[Version]]("newerVersions", Nil)
      renderUnversioned <- config.get[Boolean]("renderUnversioned", false)
      versionScanner    <- config.getOpt[VersionScannerConfig]("scannerConfig")
      result            <- Versions(
        currentVersion,
        olderVersions,
        newerVersions,
        renderUnversioned,
        versionScanner
      )
        .validated.leftMap(err => ConfigErrors(err.map(ValidationError(_))))
    } yield result
  }

  implicit val encoder: ConfigEncoder[Versions] = ConfigEncoder[Versions] { versions =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("currentVersion", versions.currentVersion)
      .withValue("olderVersions", versions.olderVersions)
      .withValue("newerVersions", versions.newerVersions)
      .withValue("renderUnversioned", versions.renderUnversioned)
      .withValue("scannerConfig", versions.scannerConfig)
      .build
  }

}
