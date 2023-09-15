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

package laika.render

import cats.effect.{ Async, IO, Resource }
import cats.syntax.all.*
import laika.api.Renderer
import laika.api.builder.{ OperationConfig, TwoPhaseRendererBuilder }
import laika.api.config.Config
import laika.api.format.{
  BinaryPostProcessor,
  BinaryPostProcessorBuilder,
  RenderFormat,
  TagFormatter,
  TwoPhaseRenderFormat
}
import laika.ast.{ DocumentTreeRoot, TemplateRoot }
import laika.format.{ Markdown, PDF, XSLFO }
import laika.io.FileIO
import laika.io.api.BinaryTreeRenderer
import laika.io.errors.ConfigException
import laika.io.helper.RenderResult
import laika.io.implicits.*
import laika.io.model.{ BinaryOutput, RenderedTreeRoot }
import laika.render.FOFormatter.Preamble
import laika.render.fo.TestTheme
import laika.render.pdf.FOConcatenation
import laika.theme.Theme
import laika.theme.config.BookConfig
import munit.CatsEffectSuite

class PDFNavigationSpec extends CatsEffectSuite with FileIO with PDFTreeModel {

  object FOTest extends TwoPhaseRenderFormat[TagFormatter, BinaryPostProcessorBuilder] {

    val interimFormat: RenderFormat[TagFormatter] = XSLFO

    def prepareTree(root: DocumentTreeRoot): Either[Throwable, DocumentTreeRoot] = Right(
      root
        .modifyTree(_.withDefaultTemplate(TemplateRoot.fallback, "fo"))
        .modifyDocumentsRecursively { doc =>
          val preamble = Preamble(doc.title.fold(doc.name)(_.extractText))
          doc.prependContent(preamble)
        }
    )

    def postProcessor: BinaryPostProcessorBuilder = new BinaryPostProcessorBuilder {

      def build[F[_]: Async](config: Config, theme: Theme[F]): Resource[F, BinaryPostProcessor[F]] =
        Resource.pure[F, BinaryPostProcessor[F]](new BinaryPostProcessor[F] {

          override def process(
              result: RenderedTreeRoot[F],
              output: BinaryOutput[F],
              opConfig: OperationConfig
          ): F[Unit] = {

            val pdfConfig = BookConfig.decodeWithDefaults(result.config, PDF.configKey)
            output.resource.use { out =>
              for {
                config <- Async[F].fromEither(pdfConfig.left.map(ConfigException.apply))
                fo     <- Async[F].fromEither(FOConcatenation(result, config, opConfig)): F[String]
                _      <- Async[F].delay(out.write(fo.getBytes("UTF-8"))): F[Unit]
              } yield ()
            }

          }

        })

    }

  }

  private val defaultParagraphProperties =
    """font-family="serif" font-size="10pt" line-height="1.5" space-after="3mm" text-align="justify""""

  private val defaultTitleProperties =
    """color="#007c99" font-family="sans-serif" font-size="24pt" font-weight="bold" keep-with-next="always" space-after="6mm" space-before="0mm""""

  def idPrefix(num: Int): String = if (num > 4) "_tree-2" else if (num > 2) "_tree-1" else ""

  def results(range: Range): String = range.map(result).reduceLeft(_ + _)

  def wrapped(content: String): String =
    s"""<fo:wrapper font-size="10pt">
       |$content
       |</fo:wrapper>""".stripMargin

  def result(num: Int): String = {
    s"""
       |
       |<fo:block id="${idPrefix(num)}_doc-$num" page-break-before="always">
       |  <fo:marker marker-class-name="chapter"><fo:block>Title $num &amp; More</fo:block></fo:marker>
       |</fo:block>
       |<fo:block id="${idPrefix(num)}_doc-${
        num
      }_title-$num" $defaultTitleProperties>Title $num &amp; More</fo:block>
       |<fo:block $defaultParagraphProperties>Text $num</fo:block>""".stripMargin
  }

  def treeTitleResult(num: Int): String = {
    val idPrefix = if (num == 3) "_tree-2" else if (num == 2) "_tree-1" else ""

    s"""
       |
       |<fo:block id="${idPrefix}_index" page-break-before="always">
       |  <fo:marker marker-class-name="chapter"><fo:block>Title Doc $num</fo:block></fo:marker>
       |</fo:block>
       |<fo:block id="${idPrefix}_index_title-$num" $defaultTitleProperties>Title Doc $num</fo:block>
       |<fo:block $defaultParagraphProperties>Text $num</fo:block>""".stripMargin
  }

  def withDefaultTemplate(result: String, bookmarks: String = ""): String =
    RenderResult.fo.withFallbackTemplate(result, bookmarks)

  def bookmarkTreeResult(treeNum: Int, docNum: Int, titleDoc: Boolean = false): String = {
    val title    = if (!titleDoc) s"Tree $treeNum &amp; More" else s"Title Doc ${treeNum + 1}"
    val treeLink = if (!titleDoc) s"doc-$docNum" else "index"
    s"""  <fo:bookmark internal-destination="_tree-${treeNum}_$treeLink">
       |    <fo:bookmark-title>$title</fo:bookmark-title>
       |    <fo:bookmark internal-destination="_tree-${treeNum}_doc-$docNum">
       |      <fo:bookmark-title>Title $docNum &amp; More</fo:bookmark-title>
       |    </fo:bookmark>
       |    <fo:bookmark internal-destination="_tree-${treeNum}_doc-${docNum + 1}">
       |      <fo:bookmark-title>Title ${docNum + 1} &amp; More</fo:bookmark-title>
       |    </fo:bookmark>
       |  </fo:bookmark>
       |""".stripMargin
  }

  val bookmarkRootResult: String = """<fo:bookmark-tree>
                                     |  <fo:bookmark internal-destination="_doc-1">
                                     |    <fo:bookmark-title>Title 1 &amp; More</fo:bookmark-title>
                                     |  </fo:bookmark>
                                     |  <fo:bookmark internal-destination="_doc-2">
                                     |    <fo:bookmark-title>Title 2 &amp; More</fo:bookmark-title>
                                     |  </fo:bookmark>
                                     |""".stripMargin

  val closeBookmarks = "</fo:bookmark-tree>"

  lazy val renderer: Resource[IO, BinaryTreeRenderer[IO]] = {
    val builder = Renderer.of(FOTest)
    builder.withConfig(builder.config.withBundlesFor(Markdown))
      .parallel[IO]
      .withTheme(TestTheme.heliumTestProps.build)
      .build
  }

  type Builder = TwoPhaseRendererBuilder[TagFormatter, BinaryPostProcessor[IO]]

  def result(navigationDepth: Int = 23, useTitleDocuments: Boolean = false): IO[String] =
    renderer.use { r =>
      val tree = createTree(navigationDepth, useTitleDocuments).withDefaultTemplate(
        TestTheme.foTemplate,
        "fo"
      )
      val root = DocumentTreeRoot(tree).addStyles(Map("fo" -> TestTheme.foStyles))
      withByteArrayTextOutput { out =>
        r
          .from(root)
          .toStream(IO.pure(out))
          .render
          .void
      }
    }

  test("render a tree with all structure elements disabled") {

    result(navigationDepth = 0).assertEquals(withDefaultTemplate(wrapped(results(1 to 6))))
  }

  test("render a tree with navigation elements enabled") {

    result().assertEquals(
      withDefaultTemplate(
        wrapped(results(1 to 6)),
        bookmarkRootResult +
          bookmarkTreeResult(1, 3) + bookmarkTreeResult(2, 5) +
          closeBookmarks
      )
    )
  }

  test(
    "render a tree with navigation elements enabled, handling a title document in both subtrees"
  ) {

    result(useTitleDocuments = true).assertEquals(
      withDefaultTemplate(
        wrapped(
          results(1 to 2) +
            treeTitleResult(2) + results(3 to 4) +
            treeTitleResult(3) + results(5 to 6)
        ),
        bookmarkRootResult +
          bookmarkTreeResult(1, 3, titleDoc = true) + bookmarkTreeResult(2, 5, titleDoc = true) +
          closeBookmarks
      )
    )
  }

}
