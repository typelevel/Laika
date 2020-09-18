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

import cats.effect.{IO, Resource, Sync}
import cats.implicits._
import laika.api.Renderer
import laika.api.builder.{OperationConfig, TwoPhaseRendererBuilder}
import laika.ast.{DocumentTreeRoot, TemplateRoot}
import laika.config.{Config, ConfigException}
import laika.factory.{BinaryPostProcessor, BinaryPostProcessorBuilder, RenderFormat, TwoPhaseRenderFormat}
import laika.format.{Markdown, PDF, XSLFO}
import laika.io.api.BinaryTreeRenderer
import laika.io.helper.RenderResult
import laika.io.implicits._
import laika.io.model.{BinaryOutput, RenderedTreeRoot}
import laika.io.runtime.Runtime
import laika.io.{FileIO, IOWordSpec}
import laika.render.FOFormatter.Preamble
import laika.render.fo.TestTheme
import laika.render.pdf.FOConcatenation
import laika.theme.Theme


class PDFNavigationSpec extends IOWordSpec with FileIO {

  
  object FOTest extends TwoPhaseRenderFormat[FOFormatter, BinaryPostProcessorBuilder] {
    
    val interimFormat: RenderFormat[FOFormatter] = XSLFO
    
    def prepareTree (root: DocumentTreeRoot): Either[Throwable, DocumentTreeRoot] = Right(root
      .copy(tree = root.tree.withDefaultTemplate(TemplateRoot.fallback, "fo"))
      .mapDocuments { doc =>
        val preamble = Preamble(doc.title.fold(doc.name)(_.extractText))
        doc.copy(content = doc.content.withContent(preamble +: doc.content.content))
      })

    def postProcessor: BinaryPostProcessorBuilder = new BinaryPostProcessorBuilder {

      def build[F[_] : Sync](config: Config, theme: Theme[F]): Resource[F, BinaryPostProcessor] = Resource.pure[F, BinaryPostProcessor](new BinaryPostProcessor {

        override def process[G[_] : Sync : Runtime](result: RenderedTreeRoot[G], output: BinaryOutput[G], opConfig: OperationConfig): G[Unit] = {

          val pdfConfig = PDF.BookConfig.decodeWithDefaults(result.config)
          output.resource.use { out =>
            for {
              config <- Sync[G].fromEither(pdfConfig.left.map(ConfigException))
              fo <- Sync[G].fromEither(FOConcatenation(result, config, opConfig)): G[String]
              _ <- Sync[G].delay(out.write(fo.getBytes("UTF-8"))): G[Unit]
            } yield ()
          }

        }
      })
    }
    
  }
  
  
  trait ResultModel {
    
    private val defaultParagraphProperties = """font-family="serif" font-size="10pt" line-height="1.5" space-after="3mm" text-align="justify""""
    private val defaultTitleProperties = """color="#007c99" font-family="sans-serif" font-size="24pt" font-weight="bold" keep-with-next="always" space-after="6mm" space-before="0mm""""
    
    def idPrefix (num: Int): String = if (num > 4) "_tree2" else if (num > 2) "_tree1" else ""
    
    def results (range: Range): String = range.map(result).reduceLeft(_ + _)
    
    def result (num: Int): String = {
      s"""
        |
        |<fo:block id="${idPrefix(num)}_doc$num" page-break-before="always">
        |  <fo:marker marker-class-name="chapter"><fo:block>Title $num &amp; More</fo:block></fo:marker>
        |</fo:block>
        |<fo:block id="${idPrefix(num)}_doc${num}_title-$num" $defaultTitleProperties>Title $num &amp; More</fo:block>
        |<fo:block $defaultParagraphProperties>Text $num</fo:block>""".stripMargin
    }

    def treeTitleResult (num: Int): String = {
      val idPrefix = if (num == 3) "_tree2" else if (num == 2) "_tree1" else ""

      s"""
         |
         |<fo:block id="${idPrefix}_index" page-break-before="always">
         |  <fo:marker marker-class-name="chapter"><fo:block>Title Doc $num</fo:block></fo:marker>
         |</fo:block>
         |<fo:block id="${idPrefix}_index_title-$num" $defaultTitleProperties>Title Doc $num</fo:block>
         |<fo:block $defaultParagraphProperties>Text $num</fo:block>""".stripMargin
    }

    def withDefaultTemplate(result: String, bookmarks: String = ""): String = RenderResult.fo.withFallbackTemplate(result, bookmarks)
    
    def bookmarkTreeResult(treeNum: Int, docNum: Int, titleDoc: Boolean = false): String = {
      val title = if (!titleDoc) s"Tree ${treeNum+1} &amp; More" else s"Title Doc ${treeNum+1}"
      val treeLink = if (!titleDoc) s"doc$docNum" else "index" 
      s"""  <fo:bookmark internal-destination="_tree${treeNum}_$treeLink">
         |    <fo:bookmark-title>$title</fo:bookmark-title>
         |    <fo:bookmark internal-destination="_tree${treeNum}_doc$docNum">
         |      <fo:bookmark-title>Title $docNum &amp; More</fo:bookmark-title>
         |    </fo:bookmark>
         |    <fo:bookmark internal-destination="_tree${treeNum}_doc${docNum + 1}">
         |      <fo:bookmark-title>Title ${docNum + 1} &amp; More</fo:bookmark-title>
         |    </fo:bookmark>
         |  </fo:bookmark>
         |""".stripMargin
    }
      
    val bookmarkRootResult: String = """<fo:bookmark-tree>
      |  <fo:bookmark internal-destination="_doc1">
      |    <fo:bookmark-title>Title 1 &amp; More</fo:bookmark-title>
      |  </fo:bookmark>
      |  <fo:bookmark internal-destination="_doc2">
      |    <fo:bookmark-title>Title 2 &amp; More</fo:bookmark-title>
      |  </fo:bookmark>
      |""".stripMargin  
      
    val closeBookmarks = "</fo:bookmark-tree>"
  }
  
  
  trait Setup extends TreeModel with ResultModel {
    
    lazy val renderer: Resource[IO, BinaryTreeRenderer[IO]] = {
      val builder = Renderer.of(FOTest)
      builder.withConfig(builder.config.withBundlesFor(Markdown))
        .io(blocker)
        .parallel[IO]
        .withTheme(TestTheme.heliumTestProps.build)
        .build
    }
    
    type Builder = TwoPhaseRendererBuilder[FOFormatter, BinaryPostProcessor]
    
    def result: IO[String] = renderer.use { r =>
      withByteArrayTextOutput { out =>
      r
        .from(DocumentTreeRoot(tree.withDefaultTemplate(TestTheme.foTemplate, "fo"), styles = Map("fo" -> TestTheme.foStyles)))
        .toStream(IO.pure(out))
        .render
        .void
      } 
    }
    
  }
  
  
  "The PDF navigation utility" should {

    "render a tree with all structure elements disabled" in new Setup {

      override val navigationDepth = 0

      result.assertEquals(withDefaultTemplate(results(1 to 6)))
    }

    "render a tree with navigation elements enabled" in new Setup {

      result.assertEquals(withDefaultTemplate(
        results(1 to 6),
        bookmarkRootResult + 
        bookmarkTreeResult(1, 3) + bookmarkTreeResult(2, 5) + 
        closeBookmarks
      ))
    }

    "render a tree with navigation elements enabled, handling a title document in both subtrees" in new Setup {

      override val useTitleDocuments = true

      result.assertEquals(withDefaultTemplate(
        results(1 to 2) +
        treeTitleResult(2) + results(3 to 4) +
        treeTitleResult(3) + results(5 to 6),
        bookmarkRootResult + 
        bookmarkTreeResult(1, 3, titleDoc = true) + bookmarkTreeResult(2, 5, titleDoc = true) + 
        closeBookmarks
      ))
    }
  }
  
}
