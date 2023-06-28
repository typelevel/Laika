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

import laika.ast.Path
import laika.config.Config.ConfigResult
import laika.config.{ Config, ConfigDecoder, ConfigEncoder, Key, LaikaKeys }

/** Configuration for a cover image for e-books (EPUB or PDF).
  *
  * The optional classifier can be used if the `@:select` directive
  * is used to produce multiple e-books with slightly different content.
  * The classifier would refer to the name of the configured choice,
  * or in case of multiple choices, to the combination of their names concatenated with `-`.
  *
  * @author Jens Halm
  */
case class CoverImage(path: Path, classifier: Option[String] = None)

object CoverImage {

  def apply(path: Path, classifier: String): CoverImage = CoverImage(path, Some(classifier))

  implicit val decoder: ConfigDecoder[CoverImage] = ConfigDecoder.config.flatMap { config =>
    for {
      path       <- config.get[Path]("path")
      classifier <- config.getOpt[String]("classifier")
    } yield {
      CoverImage(path, classifier)
    }
  }

  implicit val encoder: ConfigEncoder[CoverImage] = ConfigEncoder[CoverImage] { coverImage =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("path", coverImage.path)
      .withValue("classifier", coverImage.classifier)
      .build
  }

}

case class CoverImages(default: Option[Path], classified: Map[String, Path]) {

  def getImageFor(classifier: String): Option[Path] = classified.get(classifier).orElse(default)

  def withFallback(fallback: CoverImages): CoverImages = {
    CoverImages(default.orElse(fallback.default), fallback.classified ++ classified)
  }

}

object CoverImages {

  def forPDF(config: Config): ConfigResult[CoverImages] =
    extract(config, LaikaKeys.root.child("pdf"), LaikaKeys.root)

  def forEPUB(config: Config): ConfigResult[CoverImages] =
    extract(config, LaikaKeys.root.child("epub"), LaikaKeys.root)

  private def extract(config: Config, mainKey: Key, fallbackKey: Key): ConfigResult[CoverImages] = {

    def extract(key: Key): ConfigResult[CoverImages] = for {
      classified <- config.get[Seq[CoverImage]](key.child(LaikaKeys.coverImages.local), Nil)
      default    <- config
        .getOpt[Path](key.child(LaikaKeys.coverImage.local))
        .map(_.orElse(classified.find(_.classifier.isEmpty).map(_.path)))
    } yield {
      val classifiedMap = classified
        .collect { case CoverImage(path, Some(classifier)) => (classifier, path) }
        .toMap
      CoverImages(default, classifiedMap)
    }

    for {
      mainConf <- extract(mainKey)
      fallback <- extract(fallbackKey)
    } yield mainConf.withFallback(fallback)
  }

}
