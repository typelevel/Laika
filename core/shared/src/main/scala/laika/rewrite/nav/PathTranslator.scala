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
import laika.ast.{AbsoluteInternalTarget, ExternalTarget, Path, RelativeInternalTarget, RelativePath, ResolvedInternalTarget, RootCursor, Target}
import laika.config.{Config, LaikaKeys}
import laika.rewrite.Versions

/** Translates paths of input documents to the corresponding output path. 
  * The minimum translation that usually has to happen is to replace the suffix from the input document the path
  * has been obtained from to the suffix of the output format. 
  * Further translations are allowed to happen based on user configuration.
  *
  * @author Jens Halm
  */
trait PathTranslator {

  /** Translates the specified path of an input document to the corresponding output path. 
    */
  def translate (input: Path): Path

  /** Translates the specified relative path of an input document to the corresponding output path. 
    */
  def translate (input: RelativePath): RelativePath

  /** Translates the specified target pointing to an input document to a target pointing to an output document.
    * Might turn an internal target into an external one in cases where it points to a document that is
    * not rendered for the current target format, but for the site output. 
    * In this case it will point to the corresponding location of the hosted site, 
    * in case a `siteBaseURL` is configured. 
    */
  def translate (target: Target): Target = target match {
    case rt: ResolvedInternalTarget => 
      rt.copy(absolutePath = translate(rt.absolutePath), relativePath = translate(rt.relativePath))
    case at: AbsoluteInternalTarget => at.copy(path = translate(at.path))
    case rt: RelativeInternalTarget => rt.copy(path = translate(rt.path))
    case et => et
  }
  
}

/** Translates paths of input documents to the corresponding output path, based on a configuration instance.
  * 
  * @author Jens Halm
  */
case class ConfigurablePathTranslator (rootConfig: Config, outputSuffix: String, outputFormat: String, refPath: Path, targetLookup: Path => Option[TranslatorSpec]) extends PathTranslator {

  private val titleDocInputName = TitleDocumentConfig.inputName(rootConfig)
  private val titleDocOutputName = TitleDocumentConfig.outputName(rootConfig)
  private val siteBaseURL = rootConfig.getOpt[String](LaikaKeys.siteBaseURL).toOption.flatten
  private val currentVersion = rootConfig.getOpt[Versions].toOption.flatten.map(_.currentVersion.pathSegment)
  
  def translate (input: Path): Path = {
    targetLookup(input).fold(input) { spec =>
      val shifted = if (spec.isVersioned && outputFormat == "html") currentVersion.fold(input) { version =>
        Root / version / input.relative
      } else input
      if (!spec.isStatic) {
        if (input.basename == titleDocInputName) shifted.withBasename(titleDocOutputName).withSuffix(outputSuffix)
        else shifted.withSuffix(outputSuffix)
      }
      else shifted
    }
    
  }

  def translate (input: RelativePath): RelativePath = {
    val absolute = RelativeInternalTarget(input).relativeTo(refPath).absolutePath
    val translated = translate(absolute)
    translated.relativeTo(translate(refPath))
  }
  
  override def translate (target: Target): Target = (target, siteBaseURL) match {
    case (ResolvedInternalTarget(absolutePath, _, formats), Some(baseURL)) if !formats.contains(outputFormat) =>
      ExternalTarget(baseURL + translate(absolutePath.relative.withSuffix("html")).toString)
    case _ => super.translate(target)
  }
  
}

private[laika] case class TranslatorSpec(isStatic: Boolean, isVersioned: Boolean)

private[laika] class TargetLookup (cursor: RootCursor) extends (Path => Option[TranslatorSpec]) {

  private val lookup: Map[Path, TranslatorSpec] = {

    def isVersioned (config: Config): Boolean = config.get[Boolean](LaikaKeys.versioned).getOrElse(false)

    val treeConfigs = cursor.target.staticDocuments.map(doc => doc.path.parent).toSet[Path].map { path =>
      (path, cursor.treeConfig(path))
    }.toMap

    val markupDocs = cursor.target.allDocuments.map { doc =>
      (doc.path.withoutFragment, TranslatorSpec(isStatic = false, isVersioned = isVersioned(doc.config)))
    }

    val staticDocs = cursor.target.staticDocuments.map { doc =>
      (doc.path.withoutFragment, TranslatorSpec(isStatic = true, isVersioned = isVersioned(treeConfigs(doc.path.parent))))
    }

    (markupDocs ++ staticDocs).toMap
  }

  val versionedDocuments: Seq[Path] = lookup.collect {
    case (path, TranslatorSpec(false, true)) => path
  }.toSeq

  def apply (path: Path): Option[TranslatorSpec] = lookup.get(path.withoutFragment)

}


/** Basic path translator implementation that only replaces the suffix of the path.
  * 
  * Used in scenarios where only a single document gets rendered and there is no use case for
  * cross references or static or versioned documents.
  */
case class BasicPathTranslator (outputSuffix: String) extends PathTranslator {
  def translate (input: Path): Path = input.withSuffix(outputSuffix)
  def translate (input: RelativePath): RelativePath = input.withSuffix(outputSuffix)
}
