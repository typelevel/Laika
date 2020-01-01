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
import laika.render.pdf.{FOConcatenation, PDFConfigBuilder, PDFNavigation}


class FOforPDFSpec extends IOSpec with FileIO {

  
  case class FOTest (config: Option[PDF.Config]) extends TwoPhaseRenderFormat[FOFormatter, BinaryPostProcessor] {
    
    val interimFormat: RenderFormat[FOFormatter] = XSLFO
    
    def prepareTree (root: DocumentTreeRoot): Either[Throwable, DocumentTreeRoot] = {
      val pdfConfig = config.map(Right(_)).getOrElse(PDFConfigBuilder.fromTreeConfig(root.config))
      val rootWithTemplate = root.copy(tree = root.tree.withDefaultTemplate(TemplateRoot.fallback, "fo"))
      pdfConfig.map(PDFNavigation.prepareTree(rootWithTemplate, _)).left.map(ConfigException)
    }

    object postProcessor extends BinaryPostProcessor {

      override def process[F[_]: Async: Runtime] (result: RenderedTreeRoot[F], output: BinaryOutput[F]): F[Unit] = {

        val pdfConfig = config.map(Right(_)).getOrElse(PDFConfigBuilder.fromTreeConfig(result.config))
        
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
    
    def results (num: Int): String = (1 to num) map result reduce (_ + _)
    
    def idPrefix (num: Int): String = if (num > 4) "_tree2" else if (num > 2) "_tree1" else ""
    
    def result (num: Int): String = {
      s"""<fo:marker marker-class-name="chapter"><fo:block>Title $num &amp; More</fo:block></fo:marker>
        |<fo:block id="${idPrefix(num)}_doc${num}_title-$num" font-family="sans-serif" font-size="16pt" font-weight="bold" keep-with-next="always" space-after="7mm" space-before="12mm">Title $num &amp; More</fo:block>
        |<fo:block font-family="serif" font-size="10pt" space-after="3mm">Text $num</fo:block>""".stripMargin
    }

    def resultsWithDocTitle (num: Int): String = (1 to num) map (resultWithDocTitle) reduce (_ + _)
    
    def resultWithDocTitle (num: Int): String = {
      s"""<fo:block id="${idPrefix(num)}_doc${num}_">
        |  <fo:marker marker-class-name="chapter"><fo:block>Title $num &amp; More</fo:block></fo:marker>
        |  <fo:block id="${idPrefix(num)}_doc${num}_title-$num" font-family="sans-serif" font-size="16pt" font-weight="bold" keep-with-next="always" space-after="7mm" space-before="12mm">Title $num &amp; More</fo:block>
        |</fo:block>
        |<fo:block font-family="serif" font-size="10pt" space-after="3mm">Text $num</fo:block>""".stripMargin
    }
    
    def treeTitleResult (num: Int): String = {
      val idPrefix = if (num == 3) "_tree2" else if (num == 2) "_tree1" else ""

      s"""<fo:block id="${idPrefix}_title_">
         |  <fo:marker marker-class-name="chapter"><fo:block>Title Doc $num</fo:block></fo:marker>
         |  <fo:block id="${idPrefix}_title_title-$num" font-family="sans-serif" font-size="16pt" font-weight="bold" keep-with-next="always" space-after="7mm" space-before="12mm">Title Doc $num</fo:block>
         |</fo:block>
         |<fo:block font-family="serif" font-size="10pt" space-after="3mm">Text $num</fo:block>""".stripMargin
    }

    def treeLinkResult (num: Int): String = {
      val idPrefix = if (num == 3) "_tree2" else if (num == 2) "_tree1" else ""
      s"""<fo:block id="${idPrefix}_title_"/>"""
    }
    
    def tocTitle: String = """<fo:marker marker-class-name="chapter"><fo:block>Contents</fo:block></fo:marker>
      |<fo:block font-family="sans-serif" font-size="16pt" keep-with-next="always" space-after="7mm" space-before="12mm">Contents</fo:block>
      |"""stripMargin
    
    def tocDocResult (num: Int): String = {
      val margin = if (num > 2) """ margin-left="4mm"""" else ""
      val fontsize = if (num > 2) "11pt" else "12pt"
      val spaceBefore = if (num > 2) "2mm" else "5mm"
      s"""<fo:block font-family="serif" font-size="$fontsize"$margin space-after="0mm" space-before="$spaceBefore" text-align-last="justify"><fo:basic-link color="#3956ac" internal-destination="${idPrefix(num)}_doc${num}_">Title $num &amp; More<fo:leader leader-pattern="dots"></fo:leader><fo:page-number-citation ref-id="${idPrefix(num)}_doc${num}_" /></fo:basic-link></fo:block>""" + "\n"
    }
    
    def tocTreeResult (num: Int, titleDoc: Boolean = false): String = {
      val title = if (!titleDoc) s"Tree ${num+1} &amp; More" else s"Title Doc ${num+1}"
      s"""<fo:block font-family="serif" font-size="12pt" space-after="0mm" space-before="5mm" text-align-last="justify"><fo:basic-link color="#3956ac" internal-destination="_tree${num}_title_">$title<fo:leader leader-pattern="dots"></fo:leader><fo:page-number-citation ref-id="_tree${num}_title_" /></fo:basic-link></fo:block>""" + "\n"
    }

    def withDefaultTemplate(result: String, bookmarks: String = ""): String = RenderResult.fo.withDefaultTemplate(result, bookmarks)
    
    def bookmarkTreeResult(treeNum: Int, docNum: Int, titleDoc: Boolean = false): String = {
      val title = if (!titleDoc) s"Tree ${treeNum+1} &amp; More" else s"Title Doc ${treeNum+1}"
      s"""    <fo:bookmark internal-destination="_tree${treeNum}_title_">
         |      <fo:bookmark-title>$title</fo:bookmark-title>
         |      <fo:bookmark internal-destination="_tree${treeNum}_doc${docNum}_">
         |        <fo:bookmark-title>Title $docNum &amp; More</fo:bookmark-title>
         |      </fo:bookmark>
         |      <fo:bookmark internal-destination="_tree${treeNum}_doc${docNum + 1}_">
         |        <fo:bookmark-title>Title ${docNum + 1} &amp; More</fo:bookmark-title>
         |      </fo:bookmark>
         |    </fo:bookmark>
         |""".stripMargin
    }
      
    val bookmarkRootResult = """<fo:bookmark-tree>
      |    <fo:bookmark internal-destination="_doc1_">
      |      <fo:bookmark-title>Title 1 &amp; More</fo:bookmark-title>
      |    </fo:bookmark>
      |    <fo:bookmark internal-destination="_doc2_">
      |      <fo:bookmark-title>Title 2 &amp; More</fo:bookmark-title>
      |    </fo:bookmark>
      |""".stripMargin  
      
    val closeBookmarks = "\n  </fo:bookmark-tree>"
  }
  
  
  trait Setup extends TreeModel with ResultModel {
    
    def config: Option[PDF.Config]
    
    lazy val renderer: ParallelRenderer[IO] = Renderer.of(FOTest(config)).io(blocker).parallel[IO].build
    
    def result: IO[String] = withByteArrayTextOutput { out =>
      renderer.from(DocumentTreeRoot(tree)).toStream(IO.pure(out)).render.void
    }
    
  }
  
  
  "The FOforPDF utility" should {

    "render a tree with all structure elements disabled" in new Setup {

      val config = Some(PDF.Config(bookmarkDepth = 0, tocDepth = 0))

      result.assertEquals(withDefaultTemplate(results(6)))
    }

    "render a tree with all structure elements disabled by a tree configuration file" in new Setup {

      val config = None

      override val usePDFFileConfig = true

      result.assertEquals(withDefaultTemplate(results(6)))
    }

    "render a tree with a table of content" in new Setup {

      val config = Some(PDF.Config(bookmarkDepth = 0, tocDepth = Int.MaxValue, tocTitle = Some("Contents")))

      result.assertEquals(withDefaultTemplate(treeLinkResult(1) + tocTitle + tocDocResult(1) + tocDocResult(2)
        + tocTreeResult(1) + tocDocResult(3) + tocDocResult(4)
        + tocTreeResult(2) + tocDocResult(5) + tocDocResult(6).dropRight(1) + resultWithDocTitle(1) + resultWithDocTitle(2)
        + treeLinkResult(2) + resultWithDocTitle(3) + resultWithDocTitle(4)
        + treeLinkResult(3) + resultWithDocTitle(5) + resultWithDocTitle(6)))
    }

    "render a tree with bookmarks" in new Setup {

      val config = Some(PDF.Config(bookmarkDepth = Int.MaxValue, tocDepth = 0))

      result.assertEquals(withDefaultTemplate(treeLinkResult(1) + resultWithDocTitle(1) + resultWithDocTitle(2)
        + treeLinkResult(2) + resultWithDocTitle(3) + resultWithDocTitle(4)
        + treeLinkResult(3) + resultWithDocTitle(5) + resultWithDocTitle(6),
        bookmarkRootResult + bookmarkTreeResult(1, 3) + bookmarkTreeResult(2, 5).dropRight(1) + closeBookmarks))
    }

    "render a tree with all structure elements enabled" in new Setup {

      val config = Some(PDF.Config.default)

      result.assertEquals(withDefaultTemplate(
        treeLinkResult(1) + tocDocResult(1) + tocDocResult(2)
          + tocTreeResult(1) + tocDocResult(3) + tocDocResult(4)
          + tocTreeResult(2) + tocDocResult(5) + tocDocResult(6).dropRight(1)
          + resultWithDocTitle(1) + resultWithDocTitle(2)
          + treeLinkResult(2) + resultWithDocTitle(3) + resultWithDocTitle(4)
          + treeLinkResult(3) + resultWithDocTitle(5) + resultWithDocTitle(6),
        bookmarkRootResult + bookmarkTreeResult(1, 3) + bookmarkTreeResult(2, 5).dropRight(1) + closeBookmarks
      ))
    }

    "render a tree with all structure elements enabled, handling a title document in both subtrees" in new Setup {

      override val useTitleDocuments = true

      val config = Some(PDF.Config.default)

      result.assertEquals(withDefaultTemplate(
        treeLinkResult(1) + tocDocResult(1) + tocDocResult(2)
          + tocTreeResult(1, titleDoc = true) + tocDocResult(3) + tocDocResult(4)
          + tocTreeResult(2, titleDoc = true) + tocDocResult(5) + tocDocResult(6).dropRight(1)
          + resultWithDocTitle(1) + resultWithDocTitle(2)
          + treeTitleResult(2) + resultWithDocTitle(3) + resultWithDocTitle(4)
          + treeTitleResult(3) + resultWithDocTitle(5) + resultWithDocTitle(6),
        bookmarkRootResult + bookmarkTreeResult(1, 3, titleDoc = true) + bookmarkTreeResult(2, 5, titleDoc = true).dropRight(1) + closeBookmarks
      ))
    }
  }
  
}
