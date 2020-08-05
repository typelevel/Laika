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

package laika.helium.generate

import cats.data.Kleisli
import cats.effect.Sync
import laika.ast.Path.Root
import laika.ast.{Block, BlockSequence, Document, Image, InternalTarget, Paragraph, Path, RootElement, SpanLink, Styles, Text, Title, TitledBlock}
import laika.config.LaikaKeys
import laika.helium.DownloadPage
import laika.io.config.SiteConfig
import laika.io.model.ParsedTree
import laika.rewrite.nav.{ChoiceConfig, ChoiceGroupsConfig, CoverImages}

/**
  * @author Jens Halm
  */
private[laika] object DownloadPageGenerator {

  def generate[F[_]: Sync](pageConfig: DownloadPage): Kleisli[F, ParsedTree[F], ParsedTree[F]] = {

    val refPath: Path = Root / "downloads"

    def downloadAST (link: Path, title: String, coverImage: Option[Path]): TitledBlock = TitledBlock(Seq(
      Text(title)
    ), coverImage.map(img => Paragraph(Image(title, InternalTarget.fromPath(img, refPath)))).toSeq ++ Seq(
      Paragraph(SpanLink(Seq(Text("Download")), InternalTarget.fromPath(link, refPath)))
    ))

    Kleisli[F, ParsedTree[F], ParsedTree[F]] { tree =>

      val epubCoverImages = CoverImages.forEPUB(tree.root.config)
      val pdfCoverImages = CoverImages.forPDF(tree.root.config)

      val artifactBaseName = tree.root.config.get[String](LaikaKeys.artifactBaseName).getOrElse("download")
      val downloadPath = SiteConfig.downloadPath(tree.root.config)

      val combinations: Seq[Seq[ChoiceConfig]] = ChoiceGroupsConfig
        .createChoiceCombinationsConfig(tree.root.config)
      val downloads: Seq[Block] =
        if (combinations.isEmpty) {
          val epubLink = downloadPath / s"$artifactBaseName.epub"
          val pdfLink = downloadPath / s"$artifactBaseName.pdf"
          Seq(
            BlockSequence(
              downloadAST(epubLink, "EPUB", epubCoverImages.default),
              downloadAST(pdfLink, "PDF", pdfCoverImages.default)
            ).withOptions(Styles("downloads"))
          )
        }
        else combinations.map { combination =>
          val baseTitle = combination.map(_.label).mkString(" - ")
          val classifier = combination.map(_.name).mkString("-")
          val epubLink = downloadPath / s"$artifactBaseName-$classifier.epub"
          val pdfLink = downloadPath / s"$artifactBaseName-$classifier.pdf"
          BlockSequence(
            downloadAST(epubLink, baseTitle + " (EPUB)", epubCoverImages.getImageFor(classifier)),
            downloadAST(pdfLink, baseTitle + " (PDF)", pdfCoverImages.getImageFor(classifier))
          ).withOptions(Styles("downloads"))
        }
      val blocks = Title(pageConfig.title) +: pageConfig.description.map(Paragraph(_)).toSeq ++: downloads
      val doc = Document(Root / "downloads", RootElement(blocks))
      Sync[F].pure(tree.copy(
        root = tree.root.copy(
          tree = tree.root.tree.copy(
            content = doc +: tree.root.tree.content,
          )
        )
      ))
    }
  }
  
}
