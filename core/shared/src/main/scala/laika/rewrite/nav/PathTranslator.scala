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

package laika.rewrite.nav

import laika.ast.Path.Root
import laika.ast.{
  AbsoluteInternalTarget,
  ExternalTarget,
  Path,
  RelativeInternalTarget,
  RelativePath,
  ResolvedInternalTarget,
  RootCursor,
  Target
}
import laika.config.Config.ConfigResult
import laika.config.{ Config, LaikaKeys }
import laika.rewrite.{ OutputContext, Versions }

/** Translates paths of input documents to the corresponding output path.
  * The minimum translation that usually has to happen is to replace the suffix from the input document the path
  * has been obtained from to the suffix of the output format.
  * Further translations are allowed to happen based on user configuration.
  *
  * @author Jens Halm
  */
trait PathTranslator {

  /** Retrieves the attributes for the specified path in the context of the current virtual tree of documents.
    * If there is no document or tree associated with the specified path, the result will be empty.
    *
    * Mostly used by implementations of this trait, but accessible publicly for some less common scenarios,
    * e.g. in directive implementations.
    */
  def getAttributes(path: Path): Option[PathAttributes]

  /** Translates the specified path of an input document to the corresponding output path.
    */
  def translate(input: Path): Path

  /** Translates the specified relative path of an input document to the corresponding output path.
    *
    * Translator implementations resolve the relative path in relation to a reference path,
    * which implies that there is a dedicated path translator instance per output document.
    * Using `forReferencePath` a copy of this translator that uses a different path as reference
    * can be created cheaply.
    */
  def translate(input: RelativePath): RelativePath

  /** Translates the specified target pointing to an input document to a target pointing to an output document.
    * Might turn an internal target into an external one in cases where it points to a document that is
    * not rendered for the current target format, but for the site output.
    * In this case it will point to the corresponding location of the hosted site,
    * in case a `siteBaseURL` is configured.
    */
  def translate(target: Target): Target = target match {
    case rt: ResolvedInternalTarget =>
      rt.copy(absolutePath = translate(rt.absolutePath), relativePath = translate(rt.relativePath))
    case at: AbsoluteInternalTarget => at.copy(path = translate(at.path))
    case rt: RelativeInternalTarget => rt.copy(path = translate(rt.path))
    case et                         => et
  }

  /** Creates a copy of this path translator that uses the specified reference path for resolving
    * relative paths.
    * All other aspect of translation logic should behave the same as in this instance.
    */
  def forReferencePath(path: Path): PathTranslator

}

/** Builders that apply additional functionality to existing path translator instances.
  */
object PathTranslator {

  /** Creates a new translator instance that behaves exactly like the specified translator
    * except for ignoring all version configuration.
    */
  def ignoreVersions(translator: PathTranslator): PathTranslator = translator match {
    case cpt: ConfigurablePathTranslator =>
      cpt.copy(targetLookup =
        cpt.targetLookup.andThen(_.map(attr => PathAttributes(attr.isStatic, isVersioned = false)))
      )
    case other                           => other
  }

  /** Creates a new translator instance that applies the specified path translator function
    * before invoking the base translator.
    */
  def preTranslate(baseTranslator: PathTranslator)(f: Path => Path): PathTranslator =
    new PathTranslatorExtension(baseTranslator, preTranslate = f)

  /** Creates a new translator instance that applies the specified path translator function
    * after invoking the base translator.
    */
  def postTranslate(baseTranslator: PathTranslator)(f: Path => Path): PathTranslator =
    new PathTranslatorExtension(baseTranslator, postTranslate = f)

}

private[laika] class PathTranslatorExtension(
    baseTranslator: PathTranslator,
    preTranslate: Path => Path = identity,
    postTranslate: Path => Path = identity,
    refPath: Path = Root / "refPath"
) extends PathTranslator {

  private val translatedRefPath = translate(refPath)

  def getAttributes(path: Path): Option[PathAttributes] = baseTranslator.getAttributes(path)

  def translate(input: Path): Path = postTranslate(baseTranslator.translate(preTranslate(input)))

  def forReferencePath(path: Path): PathTranslator = new PathTranslatorExtension(
    baseTranslator.forReferencePath(path),
    preTranslate,
    postTranslate,
    path
  )

  def translate(input: RelativePath): RelativePath = {
    val absolute   = RelativeInternalTarget(input).relativeTo(refPath).absolutePath
    val translated = translate(absolute)
    translated.relativeTo(translatedRefPath)
  }

}

private[laika] case class ConfigurablePathTranslator(
    config: TranslatorConfig,
    outputContext: OutputContext,
    refPath: Path,
    targetLookup: Path => Option[PathAttributes]
) extends PathTranslator {

  private val currentVersion    = config.versions.map(_.currentVersion.pathSegment)
  private val translatedRefPath = translate(refPath)

  def getAttributes(path: Path): Option[PathAttributes] = targetLookup(path)

  def translate(input: Path): Path = translate(input, outputContext.formatSelector == "html")

  private def translate(input: Path, isHTMLTarget: Boolean): Path = {
    getAttributes(input).fold(input) { spec =>
      val shifted = if (spec.isVersioned && isHTMLTarget) currentVersion.fold(input) { version =>
        Root / version / input.relative
      }
      else input
      if (!spec.isStatic) {
        if (input.basename == config.titleDocInputName)
          shifted.withBasename(config.titleDocOutputName).withSuffix(outputContext.fileSuffix)
        else
          shifted.withSuffix(outputContext.fileSuffix)
      }
      else shifted
    }
  }

  def translate(input: RelativePath): RelativePath = {
    val absolute   = RelativeInternalTarget(input).relativeTo(refPath).absolutePath
    val translated = translate(absolute)
    translated.relativeTo(translatedRefPath)
  }

  override def translate(target: Target): Target = (target, config.siteBaseURL) match {
    case (ResolvedInternalTarget(absolutePath, _, formats), Some(baseURL))
        if !formats.contains(outputContext.formatSelector) =>
      ExternalTarget(
        baseURL + translate(absolutePath.withSuffix("html"), isHTMLTarget = true).relative.toString
      )
    case _ => super.translate(target)
  }

  def forReferencePath(path: Path): PathTranslator = copy(refPath = path)
}

sealed abstract class PathAttributes {
  def isStatic: Boolean
  def isVersioned: Boolean
}

object PathAttributes {

  private final case class Impl(isStatic: Boolean, isVersioned: Boolean) extends PathAttributes {
    override def productPrefix = "PathAttributes"
  }

  def apply(isStatic: Boolean, isVersioned: Boolean): PathAttributes = Impl(isStatic, isVersioned)
}

private[laika] case class TranslatorConfig(
    versions: Option[Versions],
    titleDocInputName: String,
    titleDocOutputName: String,
    siteBaseURL: Option[String]
)

private[laika] object TranslatorConfig {

  def readFrom(config: Config): ConfigResult[TranslatorConfig] = for {
    versions           <- config.getOpt[Versions]
    titleDocInputName  <- TitleDocumentConfig.inputName(config)
    titleDocOutputName <- TitleDocumentConfig.outputName(config)
    siteBaseURL        <- config.getOpt[String](LaikaKeys.siteBaseURL)
  } yield TranslatorConfig(versions, titleDocInputName, titleDocOutputName, siteBaseURL)

  val empty: TranslatorConfig =
    TranslatorConfig(
      None,
      TitleDocumentConfig.defaultInputName,
      TitleDocumentConfig.defaultOutputName,
      None
    )

}

private[laika] class TargetLookup(cursor: RootCursor) extends (Path => Option[PathAttributes]) {

  private def isVersioned(config: Config): Boolean =
    config.get[Boolean](LaikaKeys.versioned).getOrElse(false)

  private val lookup: Map[Path, PathAttributes] = {

    val treeConfigs = cursor.target.staticDocuments.map(doc => doc.path.parent).toSet[Path].map {
      path =>
        (path, cursor.treeConfig(path))
    }.toMap

    val markupDocs = cursor.target.allDocuments.map { doc =>
      (
        doc.path.withoutFragment,
        PathAttributes(isStatic = false, isVersioned = isVersioned(doc.config))
      )
    }

    val staticDocs = cursor.target.staticDocuments.map { doc =>
      (
        doc.path.withoutFragment,
        PathAttributes(isStatic = true, isVersioned = isVersioned(treeConfigs(doc.path.parent)))
      )
    }

    (markupDocs ++ staticDocs).toMap
  }

  val versionedDocuments: Seq[Path] = lookup.collect {
    case (path, attr) if attr.isVersioned && !attr.isStatic =>
      path
  }.toSeq

  def apply(path: Path): Option[PathAttributes] =
    lookup.get(path.withoutFragment)
      .orElse(
        Some(
          PathAttributes(isStatic = true, isVersioned = isVersioned(cursor.treeConfig(path.parent)))
        )
      )
  // paths which have validation disabled might not appear in the lookup, we treat them as static and
  // pick the versioned flag from its directory config.

}

/** Path translator implementation that returns all paths unmodified.
  *
  * Used in scenarios where only a single document gets rendered and there is no use case for
  * cross references or static or versioned documents.
  */
object NoOpPathTranslator extends PathTranslator {
  def getAttributes(path: Path): Option[PathAttributes] = None
  def translate(input: Path): Path                      = input
  def translate(input: RelativePath): RelativePath      = input
  def forReferencePath(path: Path): PathTranslator      = this
}
