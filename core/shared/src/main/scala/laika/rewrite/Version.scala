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

import cats.syntax.all.*
import cats.data.NonEmptyChain
import laika.ast.Path
import laika.config.ConfigError.{ ConfigErrors, ValidationError }
import laika.config.{ ConfigDecoder, ConfigEncoder, DefaultKey }
import laika.config.LaikaKeys

/** Configuration for a single version of the documentation.
  */
sealed abstract class Version {

  /** The description of the version to use in any UI (for example in version dropdowns).
    */
  def displayValue: String

  /** The string to use as a path segments in URLs pointing to this version.
    */
  def pathSegment: String

  /** The link target to use when switching to this version from a page that does not exist in this version.
    *
    * Default: `/index.html`
    */
  def fallbackLink: String

  /** An optional label that will be used in the UI (e.g. `Dev` or `Stable`).
    */
  def label: Option[String]

  /** Indicates whether this is the canonical version.
    *
    * When using the Helium theme setting this flag results in canonical link references
    * getting inserted into the HTML `head` section of the generated output.
    */
  def canonical: Boolean

  def withFallbackLink(value: String): Version
  def withLabel(value: String): Version
  def setCanonical: Version
}

object Version {

  def apply(displayValue: String, pathSegment: String): Version =
    Impl(displayValue, pathSegment, "index.html", None, canonical = false)

  private final case class Impl(
      displayValue: String,
      pathSegment: String,
      fallbackLink: String,
      label: Option[String],
      canonical: Boolean
  ) extends Version {
    override def productPrefix                   = "Version"
    def withFallbackLink(value: String): Version = copy(fallbackLink = value)
    def withLabel(value: String): Version        = copy(label = Some(value))
    def setCanonical: Version                    = copy(canonical = true)
  }

  implicit val decoder: ConfigDecoder[Version] = ConfigDecoder.config.flatMap { config =>
    for {
      displayName  <- config.get[String]("displayValue")
      pathSegment  <- config.get[String]("pathSegment")
      canonical    <- config.get[Boolean]("canonical", false)
      fallbackLink <- config.get[String]("fallbackLink", "index.html")
      label        <- config.getOpt[String]("label")
    } yield {
      Impl(displayName, pathSegment, fallbackLink, label, canonical)
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
  */
sealed abstract class Versions {

  /** The version that the sources of a transformation produce. */
  def currentVersion: Version

  /** List of older versions that have previously been rendered (may be empty). */
  def olderVersions: Seq[Version]

  /** List of newer versions that have previously been rendered (may be empty). */
  def newerVersions: Seq[Version]

  /** Indicates whether unversioned documents should be rendered
    * (setting this to false may be useful when re-rendering older versions).
    */
  def renderUnversioned: Boolean

  /** Optional configuration for scanning and indexing existing versions,
    * used by the Helium version switcher dropdown and by the preview server..
    */
  def scannerConfig: Option[VersionScannerConfig]

  /** Specifies an absolute file path that points to a directory containing previously
    * rendered Laika output.
    * This enables scanning and indexing existing versions during a transformation,
    * used by the Helium version switcher dropdown and by the preview server.
    *
    * This is optional, without infos about existing versions, the menu will simply switch
    * to the landing page of the respective versions.
    *
    * See [[VersionScannerConfig]] for details.
    *
    * @param rootDirectory the directory to scan
    * @param exclude virtual paths inside that directory that should be ignored
    */
  def withVersionScanner(rootDirectory: String, exclude: Seq[Path] = Nil): Versions

  /** Validates this configuration instance and either returns a `Left` with a
    * list of errors encountered or a `Right` containing this instance.
    */
  def validated: Either[NonEmptyChain[String], Versions]

  def withNewerVersions(versions: Version*): Versions
  def withOlderVersions(versions: Version*): Versions

  def withRenderUnversioned(value: Boolean): Versions

  lazy val allVersions: Seq[Version] = newerVersions ++: currentVersion +: olderVersions
}

object Versions {

  def forCurrentVersion(version: Version): Versions =
    Impl(version, Nil, Nil, renderUnversioned = true, None)

  private final case class Impl(
      currentVersion: Version,
      olderVersions: Seq[Version],
      newerVersions: Seq[Version] = Nil,
      renderUnversioned: Boolean = true,
      scannerConfig: Option[VersionScannerConfig] = None
  ) extends Versions {

    override def productPrefix = "Versions"

    def withVersionScanner(rootDirectory: String, exclude: Seq[Path] = Nil): Versions =
      copy(scannerConfig = Some(VersionScannerConfig(rootDirectory, exclude)))

    def validated: Either[NonEmptyChain[String], Versions] = {

      val dupSegments = allVersions.groupBy(_.pathSegment).filter(_._2.size > 1).keys.toList.sorted
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

    def withNewerVersions(versions: Version*): Versions = copy(newerVersions = versions)

    def withOlderVersions(versions: Version*): Versions = copy(olderVersions = versions)

    def withRenderUnversioned(value: Boolean): Versions = copy(renderUnversioned = value)
  }

  implicit val key: DefaultKey[Versions] = DefaultKey(LaikaKeys.versions)

  implicit val decoder: ConfigDecoder[Versions] = ConfigDecoder.config.flatMap { config =>
    for {
      currentVersion    <- config.get[Version]("currentVersion")
      olderVersions     <- config.get[Seq[Version]]("olderVersions", Nil)
      newerVersions     <- config.get[Seq[Version]]("newerVersions", Nil)
      renderUnversioned <- config.get[Boolean]("renderUnversioned", false)
      versionScanner    <- config.getOpt[VersionScannerConfig]("scannerConfig")
      result            <- Impl(
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
  */
sealed abstract class VersionScannerConfig {

  /** File system path that represents the root of existing versions.
    */
  def rootDirectory: String

  /** Paths to be skipped when scanning the output directory for existing versions (for example for API docs),
    * interpreted from the root directory of each version.
    */
  def exclude: Seq[Path]

}

object VersionScannerConfig {

  def apply(rootDirectory: String, exclude: Seq[Path] = Nil): VersionScannerConfig =
    Impl(rootDirectory, exclude)

  private final case class Impl(rootDirectory: String, exclude: Seq[Path] = Nil)
      extends VersionScannerConfig {
    override def productPrefix = "VersionScannerConfig"
  }

  implicit val decoder: ConfigDecoder[VersionScannerConfig] = ConfigDecoder.config.flatMap {
    config =>
      for {
        rootDirectory <- config.get[String]("rootDirectory")
        exclude       <- config.get[Seq[Path]]("exclude", Nil)
      } yield {
        Impl(rootDirectory, exclude)
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
