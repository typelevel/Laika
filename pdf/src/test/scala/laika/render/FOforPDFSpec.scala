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

import cats.effect.{Async, IO}
import cats.implicits._
import laika.api.Renderer
import laika.api.builder.TwoPhaseRendererBuilder
import laika.ast.{DocumentTreeRoot, TemplateRoot}
import laika.config.ConfigException
import laika.factory.{BinaryPostProcessor, RenderFormat, TwoPhaseRenderFormat}
import laika.format.{PDF, XSLFO}
import laika.io.binary.ParallelRenderer
import laika.io.helper.RenderResult
import laika.io.implicits._
import laika.io.model.{BinaryOutput, RenderedTreeRoot}
import laika.io.runtime.Runtime
import laika.io.{FileIO, IOSpec}
import laika.render.pdf.{FOConcatenation, PDFNavigation}


class FOforPDFSpec extends IOSpec with FileIO {

  
  object FOTest extends TwoPhaseRenderFormat[FOFormatter, BinaryPostProcessor] {
    
    val interimFormat: RenderFormat[FOFormatter] = XSLFO
    
    def prepareTree (root: DocumentTreeRoot): Either[Throwable, DocumentTreeRoot] = {
      val pdfConfig = PDF.BookConfig.decodeWithDefaults(root.config)
      val rootWithTemplate = root.copy(
        tree = root.tree.withDefaultTemplate(TemplateRoot.fallback, "fo")
      )
      pdfConfig.map(PDFNavigation.prepareTree(rootWithTemplate, _)).left.map(ConfigException)
    }

    object postProcessor extends BinaryPostProcessor {

      override def process[F[_]: Async: Runtime] (result: RenderedTreeRoot[F], output: BinaryOutput[F]): F[Unit] = {

        val pdfConfig = PDF.BookConfig.decodeWithDefaults(result.config)
        output.resource.use { out =>
          for {
            config <- Async[F].fromEither(pdfConfig.left.map(ConfigException))
            fo <- Async[F].fromEither(FOConcatenation(result, config).left.map(ConfigException)): F[String]
            _  <- Async[F].delay(out.write(fo.getBytes("UTF-8"))): F[Unit]
          } yield ()
        }

      }
    }
    
  }
  
  
  trait ResultModel {
    
    private val defaultParagraphProperties = """font-family="serif" font-size="10pt" line-height="1.5" space-after="3mm" text-align="justify""""
    private val defaultTitleProperties = """font-family="sans-serif" font-size="18pt" font-weight="bold" keep-with-next="always" space-after="6mm" space-before="12mm""""
    
    def results (num: Int): String = (1 to num).map(result).reduce(_ + _)
    
    def idPrefix (num: Int): String = if (num > 4) "_tree2" else if (num > 2) "_tree1" else ""
    
    def result (num: Int): String = {
      s"""<fo:marker marker-class-name="chapter"><fo:block>Title $num &amp; More</fo:block></fo:marker>
        |<fo:block id="${idPrefix(num)}_doc${num}_title-$num" $defaultTitleProperties>Title $num &amp; More</fo:block>
        |<fo:block $defaultParagraphProperties>Text $num</fo:block>""".stripMargin
    }

    def resultWithDocTitle (num: Int): String = {
      s"""<fo:block id="${idPrefix(num)}_doc$num">
        |  <fo:marker marker-class-name="chapter"><fo:block>Title $num &amp; More</fo:block></fo:marker>
        |  <fo:block id="${idPrefix(num)}_doc${num}_title-$num" $defaultTitleProperties>Title $num &amp; More</fo:block>
        |</fo:block>
        |<fo:block $defaultParagraphProperties>Text $num</fo:block>""".stripMargin
    }
    
    def treeTitleResult (num: Int): String = {
      val idPrefix = if (num == 3) "_tree2" else if (num == 2) "_tree1" else ""

      s"""<fo:block id="${idPrefix}_index">
         |  <fo:marker marker-class-name="chapter"><fo:block>Title Doc $num</fo:block></fo:marker>
         |  <fo:block id="${idPrefix}_index_title-$num" $defaultTitleProperties>Title Doc $num</fo:block>
         |</fo:block>
         |<fo:block $defaultParagraphProperties>Text $num</fo:block>""".stripMargin
    }

    def withDefaultTemplate(result: String, bookmarks: String = ""): String = RenderResult.fo.withDefaultTemplate(result, bookmarks)
    
    def bookmarkTreeResult(treeNum: Int, docNum: Int, titleDoc: Boolean = false): String = {
      val title = if (!titleDoc) s"Tree ${treeNum+1} &amp; More" else s"Title Doc ${treeNum+1}"
      val treeLink = if (!titleDoc) s"doc$docNum" else "index" 
      s"""    <fo:bookmark internal-destination="_tree${treeNum}_$treeLink">
         |      <fo:bookmark-title>$title</fo:bookmark-title>
         |      <fo:bookmark internal-destination="_tree${treeNum}_doc$docNum">
         |        <fo:bookmark-title>Title $docNum &amp; More</fo:bookmark-title>
         |      </fo:bookmark>
         |      <fo:bookmark internal-destination="_tree${treeNum}_doc${docNum + 1}">
         |        <fo:bookmark-title>Title ${docNum + 1} &amp; More</fo:bookmark-title>
         |      </fo:bookmark>
         |    </fo:bookmark>
         |""".stripMargin
    }
      
    val bookmarkRootResult: String = """<fo:bookmark-tree>
      |    <fo:bookmark internal-destination="_doc1">
      |      <fo:bookmark-title>Title 1 &amp; More</fo:bookmark-title>
      |    </fo:bookmark>
      |    <fo:bookmark internal-destination="_doc2">
      |      <fo:bookmark-title>Title 2 &amp; More</fo:bookmark-title>
      |    </fo:bookmark>
      |""".stripMargin  
      
    val closeBookmarks = "\n  </fo:bookmark-tree>"
  }
  
  
  trait Setup extends TreeModel with ResultModel {
    
    lazy val renderer: ParallelRenderer[IO] = Renderer.of(FOTest).io(blocker).parallel[IO].build
    
    type Builder = TwoPhaseRendererBuilder[FOFormatter, BinaryPostProcessor]
    
    def result: IO[String] = withByteArrayTextOutput { out =>
      renderer
        .from(DocumentTreeRoot(tree))
        .toStream(IO.pure(out))
        .render
        .void
    }
    
  }
  
  
  "The FOforPDF utility" should {

    "render a tree with all structure elements disabled" in new Setup {

      override val navigationDepth = 0

      result.assertEquals(withDefaultTemplate(results(6)))
    }

    "render a tree with navigation elements enabled" in new Setup {

      result.assertEquals(withDefaultTemplate(
        resultWithDocTitle(1) + resultWithDocTitle(2) +
        resultWithDocTitle(3) + resultWithDocTitle(4) +
        resultWithDocTitle(5) + resultWithDocTitle(6),
        bookmarkRootResult + bookmarkTreeResult(1, 3) + bookmarkTreeResult(2, 5).dropRight(1) + closeBookmarks
      ))
    }

    "render a tree with navigation elements enabled, handling a title document in both subtrees" in new Setup {

      override val useTitleDocuments = true

      result.assertEquals(withDefaultTemplate(
        resultWithDocTitle(1) + resultWithDocTitle(2) +
        treeTitleResult(2) + resultWithDocTitle(3) + resultWithDocTitle(4) +
        treeTitleResult(3) + resultWithDocTitle(5) + resultWithDocTitle(6),
        bookmarkRootResult + bookmarkTreeResult(1, 3, titleDoc = true) + bookmarkTreeResult(2, 5, titleDoc = true).dropRight(1) + closeBookmarks
      ))
    }
  }
  
}
