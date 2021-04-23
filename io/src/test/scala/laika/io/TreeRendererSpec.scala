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

package laika.io

import java.io.File

import cats.syntax.all._
import cats.data.{Chain, NonEmptyChain}
import cats.effect.{IO, Resource, Sync}
import laika.api.Renderer
import laika.ast.Path.Root
import laika.ast._
import laika.ast.sample.{BuilderKey, ParagraphCompanionShortcuts, SampleConfig, SampleTrees, TestSourceBuilders}
import laika.bundle.{BundleOrigin, BundleProvider}
import laika.config.{Config, ConfigBuilder, LaikaKeys}
import laika.format._
import laika.helium.generate.FOStyles
import laika.io.api.{BinaryTreeRenderer, TreeRenderer}
import laika.io.helper.{InputBuilder, RenderResult, TestThemeBuilder}
import laika.io.implicits._
import laika.io.model.{InputTree, RenderContent, RenderedDocument, RenderedTree, RenderedTreeRoot, StringTreeOutput}
import laika.io.runtime.RendererRuntime.{DuplicatePath, RendererErrors}
import laika.parse.GeneratedSource
import laika.parse.markup.DocumentParser.{InvalidDocument, InvalidDocuments}
import laika.render._
import laika.render.fo.TestTheme
import laika.render.fo.TestTheme.staticHTMLPaths
import laika.rewrite.{DefaultTemplatePath, Version, Versions}
import laika.rewrite.ReferenceResolver.CursorKeys
import laika.rewrite.nav.TargetFormats

import scala.io.Codec

class TreeRendererSpec extends IOWordSpec 
  with ParagraphCompanionShortcuts
  with FileIO
  with TestSourceBuilders { self =>

  val expected: String = """RootElement - Blocks: 2
      |. Paragraph - Spans: 1
      |. . Text - 'aaö'
      |. Paragraph - Spans: 1
      |. . Text - 'bbb'""".stripMargin

  trait DocBuilder extends InputBuilder {

    def titleWithId (text: String): Title = {
      val id = text
        .replaceAll("[^a-zA-Z0-9-]+","-")
        .replaceFirst("^-","")
        .replaceFirst("-$","")
        .toLowerCase
      Title(text).withOptions(Id(id) + Style.title)
    }
    
    def staticDoc(num: Int, path: Path = Root, formats: Option[String] = None) =
      ByteInput("Static" + num, path / s"static$num.txt", formats.fold[TargetFormats](TargetFormats.All)(TargetFormats.Selected(_)))

    def renderedDoc(num: Int): String =
      """RootElement - Blocks: 1
        |. Paragraph - Spans: 1
        |. . Text - 'Text """.stripMargin + num + "'"

    def twoDocs(rootDoc: RootElement, subDoc: RootElement): DocumentTree = DocumentTree(Root, List(
      Document(Root / "doc", rootDoc),
      DocumentTree(Root / "tree", List(Document(Root / "tree" / "subdoc", subDoc)))
    ))

    def renderedRoot(content: Seq[RenderContent],
                     title: Option[SpanSequence],
                     titleDocument: Option[RenderedDocument] = None,
                     coverDocument: Option[RenderedDocument] = None,
                     staticDocuments: Seq[Path] = Nil): RenderedTreeRoot[IO] = RenderedTreeRoot(
      RenderedTree(Root, title, content, titleDocument),
      TemplateRoot.fallback,
      Config.empty,
      coverDocument = coverDocument,
      staticDocuments = staticDocuments.map(ByteInput.apply(_))
    )
    
    def renderedTree(path: Path, content: Seq[RenderContent]): RenderedTree = RenderedTree(path, None, content)

    def renderedDoc(path: Path): RenderedDocument = renderedDoc(path, expected, "Title")
    def renderedDoc(path: Path, expected: String): RenderedDocument = renderedDoc(path, expected, "Title")

    def renderedDoc(path: Path, expected: String, title: String): RenderedDocument =
      RenderedDocument(path, if (hasTitle) Some(SpanSequence(title)) else None, Nil, expected, Config.empty)
    
    def hasTitle: Boolean = true
    
    def betweenBrackets (span: TemplateSpan): TemplateRoot = 
      TemplateRoot(TemplateString("["), span, TemplateString("]"))
  }
  
  trait TreeRendererSetup[FMT] extends DocBuilder {

    def styles: StyleDeclarationSet = StyleDeclarationSet.empty
    
    def treeRoot: DocumentTreeRoot = DocumentTreeRoot(input, styles = Map("fo" -> styles).withDefaultValue(StyleDeclarationSet.empty))
    
    def input: DocumentTree
    
    def renderer: Resource[IO, TreeRenderer[IO]]
    
    def renderedTree: IO[RenderedTreeRoot[IO]] = renderer
      .use (_
        .from(treeRoot)
        .toOutput(StringTreeOutput)
        .render
      )

    def addPosition (tree: DocumentTree, pos: Seq[Int] = Nil): DocumentTree = {
      val nextNum = Iterator.from(1)
      tree.copy(content = tree.content.map {
        case d: Document => d.copy(position = TreePosition(pos :+ nextNum.next()))
        case t: DocumentTree =>
          val num = pos :+ nextNum.next()
          addPosition(t.copy(position = TreePosition(num)), num)
      })
    }

    val fontConfigTree: DocumentTree = DocumentTree(Root / "laika", List(
      DocumentTree(Root / "laika" / "fonts", Nil, config = ConfigBuilder.empty.withValue(LaikaKeys.targetFormats, Seq("epub","epub.xhtml","pdf")).build)
    ))
    
    def rootElem: RootElement = RootElement(p("aaö"), p("bbb"))
    
    def rootTree: DocumentTree = rootTree(rootElem)
    def rootTree (content: RootElement): DocumentTree = DocumentTree(Root, List(Document(Root / "doc", content)))
    
  }
  
  trait ASTRenderer extends TreeRendererSetup[TextFormatter] {
    val staticPaths = Seq( // TODO - why do they differ from TreeTransformerSpec?
      Root / "laika" / "fonts" / "Lato-Regular.ttf",
      Root / "laika" / "fonts" / "Lato-Italic.ttf",
      Root / "laika" / "fonts" / "Lato-Bold.ttf",
      Root / "laika" / "fonts" / "Lato-BoldItalic.ttf",
      Root / "laika" / "fonts" / "FiraCode-Medium.otf",
      Root / "laika" / "fonts" / "icofont.ttf",
      Root / "helium" / "fonts"/ "icofont.woff",
      Root / "helium" / "fonts"/ "icofont.woff2",
    )
    lazy val renderer: Resource[IO, TreeRenderer[IO]] = Renderer.of(AST).parallel[IO].build
    override def hasTitle = false
  }

  trait HTMLRenderer extends TreeRendererSetup[HTMLFormatter] {
    val staticPaths = staticHTMLPaths.filterNot(_.suffix.contains("epub.css"))
    override val rootElem: RootElement = RootElement(titleWithId("Title"), p("bbb"))
    lazy val renderer: Resource[IO, TreeRenderer[IO]] = Renderer.of(HTML).parallel[IO].build
  }

  trait EPUB_XHTMLRenderer extends TreeRendererSetup[HTMLFormatter] {
    val staticPaths = staticHTMLPaths.filterNot(path => path.name == "laika-helium.css" || path.name == "icofont.min.css" || path.name == "landing.page.css" || path.suffix.contains("js"))
    override val rootElem: RootElement = RootElement(titleWithId("Title"), p("bbb"))
    lazy val renderer: Resource[IO, TreeRenderer[IO]] = Renderer.of(EPUB.XHTML).parallel[IO].build
  }

  trait FORenderer extends TreeRendererSetup[FOFormatter] {
    val staticPaths = Seq(
      Root / "laika" / "fonts" / "Lato-Regular.ttf",
      Root / "laika" / "fonts" / "Lato-Italic.ttf",
      Root / "laika" / "fonts" / "Lato-Bold.ttf",
      Root / "laika" / "fonts" / "Lato-BoldItalic.ttf",
      Root / "laika" / "fonts" / "FiraCode-Medium.otf",
      Root / "laika" / "fonts" / "icofont.ttf",
      Root / "helium" / "fonts"/ "icofont.woff",
      Root / "helium" / "fonts"/ "icofont.woff2",
    )
    override def styles: StyleDeclarationSet = TestTheme.foStyles
    val customStyle: StyleDeclaration = StyleDeclaration(StylePredicate.ElementType("Paragraph"), "font-size" -> "11pt")
    val customThemeStyles: Set[StyleDeclaration] = TestTheme.foStyles.styles + customStyle.increaseOrderBy(1)
    override val rootElem: RootElement = RootElement(titleWithId("Title"), p("bbb"))
    val subElem: RootElement = RootElement(titleWithId("Sub Title"), p("ccc"))
    val defaultParagraphStyles = """font-family="serif" font-size="10pt" line-height="1.5" space-after="3mm" text-align="justify""""
    val overriddenParagraphStyles = """font-family="serif" font-size="11pt" line-height="1.5" space-after="3mm" text-align="justify""""

    def title(id: String, text: String) =
      s"""<fo:block id="$id" color="#007c99" font-family="sans-serif" font-size="24pt" font-weight="bold" keep-with-next="always" space-after="6mm" space-before="0mm">$text</fo:block>"""

    def renderer: Resource[IO, TreeRenderer[IO]] = Renderer
      .of(XSLFO)
      .parallel[IO]
      .withTheme(TestTheme.heliumTestProps.build)
      .build
  }
    
    

  "The tree renderer" should {
    
    "render an empty tree" in {
      new ASTRenderer {
        val input = DocumentTree(Root, Nil)
        renderedTree.assertEquals(renderedRoot(Nil, None, staticDocuments = staticPaths))
      }
    }
  
    "render a tree with a single document" in {
      new ASTRenderer {
        val input = rootTree
        renderedTree.assertEquals(renderedRoot(List(renderedDoc(Root / "doc.txt")), None, staticDocuments = staticPaths))
      }
    }
  
    "render a tree with a single document to HTML using the default template" in {
      new HTMLRenderer {
        val input = rootTree
        val expected = RenderResult.html.withDefaultTemplate("Title", """<h1 id="title" class="title">Title</h1>
          |<p>bbb</p>""".stripMargin)
        renderedTree.assertEquals(renderedRoot(List(renderedDoc(Root / "doc.html", expected)), None, staticDocuments = staticPaths))
      }
    }
  
    "fail with duplicate paths" in {
      new HTMLRenderer {
        val input = DocumentTree(Root, List(
          Document(Root / "doc1", rootElem),
          Document(Root / "doc2", rootElem),
          Document(Root / "doc2", rootElem),
          Document(Root / "sub" / "doc", rootElem),
          Document(Root / "sub" / "doc", rootElem)
        ))
        renderer
          .use (_
            .from(treeRoot)
            .toOutput(StringTreeOutput)
            .render
          )
          .attempt
          .assertEquals(Left(
            RendererErrors(Seq(DuplicatePath(Root / "doc2"), DuplicatePath(Root / "sub" / "doc")))
          ))
      }
    }

    "collect errors from multiple documents" in {
      
      def invalidLink (key: BuilderKey): Seq[Block] = 
        Seq(InvalidBlock(s"unresolved link reference: link${key.num}", generatedSource(s"[link${key.num}]")))

      new HTMLRenderer {
        val input = SampleTrees.sixDocuments
          .docContent(invalidLink _)
          .build.tree

        val invalidDocuments = input.allDocuments.map { doc =>
          val msg = s"unresolved link reference: link${doc.path.name.last}"
          val invalidSpan = InvalidBlock(msg, generatedSource(s"[link${doc.path.name.last}]"))
          InvalidDocument(doc.path, invalidSpan)
        }
        renderer
          .use (_
            .from(treeRoot)
            .toOutput(StringTreeOutput)
            .render
          )
          .attempt
          .assertEquals(Left(
            InvalidDocuments(NonEmptyChain.fromChainUnsafe(Chain.fromSeq(invalidDocuments)))
          ))
      }
    }
  
    "render a tree with a single document to HTML using a custom template in the root directory" in {
      new HTMLRenderer {
        val template = TemplateDocument(DefaultTemplatePath.forHTML, betweenBrackets(
          TemplateContextReference(CursorKeys.documentContent, required = true, GeneratedSource)
        ))
        val input = rootTree.copy(templates = Seq(template))
        val expected = """[<h1 id="title" class="title">Title</h1>
          |<p>bbb</p>]""".stripMargin
        renderedTree.assertEquals(renderedRoot(List(renderedDoc(Root / "doc.html", expected)), None, staticDocuments = staticPaths))
      }
    }

    "render a tree with a single document to HTML using a render override in a theme" in {
      new HTMLRenderer {
        val input = rootTree
        override lazy val renderer = Renderer.of(HTML).parallel[IO]
          .withTheme(TestThemeBuilder.forBundle(BundleProvider.forOverrides(HTML.Overrides {
            case (fmt, Text(txt, _)) => fmt.text(txt + "!")
          }, origin = BundleOrigin.Theme))
          ).build
        val expected = """<h1 id="title" class="title">Title!</h1>
                         |<p>bbb!</p>""".stripMargin
        renderedTree.assertEquals(renderedRoot(List(renderedDoc(Root / "doc.html", expected)), None))
      }
    }

    "render a tree with a single document to HTML with a render override that shadows an override in a theme" in {
      new HTMLRenderer {
        val input = rootTree
        override lazy val renderer = Renderer
          .of(HTML)
          .using(BundleProvider.forOverrides(HTML.Overrides {
            case (fmt, Text(txt, _)) => fmt.text(txt + "?")
          }))
          .parallel[IO]
          .withTheme(TestThemeBuilder.forBundle(BundleProvider.forOverrides(HTML.Overrides {
            case (fmt, Text(txt, _)) => fmt.text(txt + "!")
          }, origin = BundleOrigin.Theme))
          )
          .build
        val expected = """<h1 id="title" class="title">Title?</h1>
                         |<p>bbb?</p>""".stripMargin
        renderedTree.assertEquals(renderedRoot(List(renderedDoc(Root / "doc.html", expected)), None))
      }
    }
  
    "render a tree with a single document to HTML using a custom template in an extension bundle" in {
      new HTMLRenderer {
        val template = betweenBrackets(
          TemplateContextReference(CursorKeys.documentContent, required = true, GeneratedSource)
        )
        val inputs = new TestThemeBuilder.Inputs {
          def build[F[_]: Sync] = InputTree[F]
            .addTemplate(TemplateDocument(DefaultTemplatePath.forHTML, template))
        }
        override lazy val renderer = Renderer.of(HTML)
          .parallel[IO]
          .withTheme(TestThemeBuilder.forInputs(inputs))
          .build
        val input = rootTree
        val expected = """[<h1 id="title" class="title">Title</h1>
                         |<p>bbb</p>]""".stripMargin
        renderedTree.assertEquals(renderedRoot(List(renderedDoc(Root / "doc.html", expected)), None))
      }
    }
  
    "render a tree with a cover and title document to HTML" in {
      new HTMLRenderer {
        val input = DocumentTree(Root, List(Document(Root / "doc", rootElem), fontConfigTree), Some(Document(Root / "README", rootElem)))
        override def treeRoot = DocumentTreeRoot(input, coverDocument = Some(Document(Root / "cover", rootElem)))
        val expected = RenderResult.html.withDefaultTemplate("Title", """<h1 id="title" class="title">Title</h1>
                                                                        |<p>bbb</p>""".stripMargin)
        renderedTree.assertEquals(renderedRoot(
          List(renderedDoc(Root / "doc.html", expected)),
          None,
          Some(renderedDoc(Root / "index.html", expected)),
          Some(renderedDoc(Root / "cover.html", expected)),
          staticDocuments = Seq(
            Root / "helium" / "laika-helium.js",
            Root / "helium" / "landing.page.css",
            Root / "helium" / "icofont.min.css",
            Root / "helium" / "fonts"/ "icofont.woff",
            Root / "helium" / "fonts"/ "icofont.woff2",
            Root / "helium" / "laika-helium.css"
          )
        ))
      }
    }
  
    "render a tree with a single document to EPUB.XHTML using the default template" in {
      new EPUB_XHTMLRenderer {
        val input = rootTree
        val expected = RenderResult.epub.withDefaultTemplate("Title", """<h1 id="title" class="title">Title</h1>
                                                                        |<p>bbb</p>""".stripMargin)
        val path = (Root / "doc").withSuffix("epub.xhtml")
        renderedTree.assertEquals(renderedRoot(List(renderedDoc(path, expected)), None, staticDocuments = staticPaths))
      }
    }
  
    "render a tree with a single document to EPUB.XHTML using a custom template in the root directory" in {
      new EPUB_XHTMLRenderer {
        val template = TemplateDocument(DefaultTemplatePath.forEPUB, betweenBrackets(
          TemplateContextReference(CursorKeys.documentContent, required = true, GeneratedSource)
        ))
        val input = rootTree.copy(templates = Seq(template))
        val expected = """[<h1 id="title" class="title">Title</h1>
                         |<p>bbb</p>]""".stripMargin
        val path = (Root / "doc").withSuffix("epub.xhtml")
        renderedTree.assertEquals(renderedRoot(List(renderedDoc(path, expected)), None, staticDocuments = staticPaths))
      }
    }
  
    "render a tree with a single document to EPUB.XHTML using a custom template in a theme" in {
      new EPUB_XHTMLRenderer {
        val template = betweenBrackets(
          TemplateContextReference(CursorKeys.documentContent, required = true, GeneratedSource)
        )
        val inputs = new TestThemeBuilder.Inputs {
          def build[F[_]: Sync] = InputTree[F]
            .addTemplate(TemplateDocument(DefaultTemplatePath.forEPUB, template))
        }
        override lazy val renderer = 
          Renderer
            .of(EPUB.XHTML)
            .parallel[IO]
            .withTheme(TestThemeBuilder.forInputs(inputs))
            .build
        val input = rootTree
        val expected = """[<h1 id="title" class="title">Title</h1>
                         |<p>bbb</p>]""".stripMargin
        val path = (Root / "doc").withSuffix("epub.xhtml")
        renderedTree.assertEquals(renderedRoot(List(renderedDoc(path, expected)), None, staticDocuments = Nil))
      }
    }
  
    "render a tree with a single document to XSL-FO using the default template and default CSS" in {
      new FORenderer {
        val input = rootTree
        val expected = RenderResult.fo.withFallbackTemplate(s"""${title("_doc_title", "Title")}
          |<fo:block $defaultParagraphStyles>bbb</fo:block>""".stripMargin)
        val path = Root / "doc.fo"
        renderedTree.assertEquals(renderedRoot(List(renderedDoc(path, expected)), None, staticDocuments = staticPaths))
      }
    }
  
    "render a tree with a single document to XSL-FO using a custom template" in {
      new FORenderer {
        val template = TemplateDocument(DefaultTemplatePath.forFO, betweenBrackets(
          TemplateContextReference(CursorKeys.documentContent, required = true, GeneratedSource)
        ))
        val input = rootTree.copy(templates = Seq(template))
        val expected = s"""[${title("_doc_title", "Title")}
          |<fo:block $defaultParagraphStyles>bbb</fo:block>]""".stripMargin
        val path = Root / "doc.fo"
        renderedTree.assertEquals(renderedRoot(List(renderedDoc(path, expected)), None, staticDocuments = staticPaths))
      }
    }
  
    "render a tree with two documents to XSL-FO using a custom style sheet in a theme" in {
      new FORenderer with DocBuilder {
        val inputs = new TestThemeBuilder.Inputs {
          def build[F[_]: Sync] = InputTree[F]
            .addStyles(customThemeStyles, FOStyles.defaultPath)
            .addTemplate(TemplateDocument(DefaultTemplatePath.forFO, TestTheme.foTemplate))
        }
        override val renderer =
          Renderer
            .of(XSLFO)
            .parallel[IO]
            .withTheme(TestThemeBuilder.forInputs(inputs))
            .build
        val input = twoDocs(rootElem, subElem)
        val expectedRoot = RenderResult.fo.withFallbackTemplate(s"""${title("_doc_title", "Title")}
          |<fo:block $overriddenParagraphStyles>bbb</fo:block>""".stripMargin)
        val expectedSub = RenderResult.fo.withFallbackTemplate(s"""${title("_tree_subdoc_sub-title", "Sub Title")}
          |<fo:block $overriddenParagraphStyles>ccc</fo:block>""".stripMargin)
        renderedTree.assertEquals(renderedRoot(List(
          renderedDoc(Root / "doc.fo", expectedRoot),
          renderedTree(Root / "tree", List(
            renderedDoc(Root / "tree" / "subdoc.fo", expectedSub, "Sub Title")
          ))
        ), None))
      }
    }
  
    "render a tree with two documents to XSL-FO using a custom style sheet in the tree root" in {
      new FORenderer with DocBuilder {
        val input = twoDocs(rootElem, subElem)
        val foStyles: Map[String, StyleDeclarationSet] =
          Map("fo" -> (styles ++ StyleDeclarationSet(FOStyles.defaultPath, customStyle)))
        override def treeRoot = DocumentTreeRoot(input, styles = foStyles)
        val expectedRoot = RenderResult.fo.withFallbackTemplate(s"""${title("_doc_title", "Title")}
          |<fo:block $overriddenParagraphStyles>bbb</fo:block>""".stripMargin)
        val expectedSub = RenderResult.fo.withFallbackTemplate(s"""${title("_tree_subdoc_sub-title", "Sub Title")}
          |<fo:block $overriddenParagraphStyles>ccc</fo:block>""".stripMargin)
        renderedTree.assertEquals(renderedRoot(List(
          renderedDoc(Root / "doc.fo", expectedRoot),
          renderedTree(Root / "tree", List(
            renderedDoc(Root / "tree" / "subdoc.fo", expectedSub, "Sub Title")
          ))
        ), None, staticDocuments = staticPaths))
      }
    }
  
    "render a tree with a single static document" in new ASTRenderer with DocBuilder {
      val input = DocumentTree(Root, Seq(fontConfigTree))
      override def treeRoot = DocumentTreeRoot(input, staticDocuments = Seq(StaticDocument(staticDoc(1).path)))
      renderer
        .use (_
          .from(treeRoot)
          .copying(Seq(ByteInput("...", Root / "static1.txt")))
          .toOutput(StringTreeOutput)
          .render
        )
        .assertEquals(renderedRoot(Nil, None, staticDocuments = Seq(Root / "static1.txt") ++ TestTheme.staticASTPaths))
    }

    "render a tree with a single static document from a theme" in new ASTRenderer with DocBuilder {
      val input = DocumentTree(Root, Nil)
      override def treeRoot = DocumentTreeRoot(input, staticDocuments = Seq(StaticDocument(staticDoc(1).path)))
      val inputs = new TestThemeBuilder.Inputs {
        def build[F[_]: Sync] = InputTree[F].addString("...", Root / "static1.txt")
      }
      val theme = TestThemeBuilder.forInputs(inputs)
      Renderer
        .of(AST)
        .parallel[IO]
        .withTheme(theme)
        .build
        .use (_
          .from(treeRoot)
          .toOutput(StringTreeOutput)
          .render
        )
        .assertEquals(renderedRoot(Nil, None, staticDocuments = Seq(Root / "static1.txt")))
    }
  
    "render a tree with all available file types" in new ASTRenderer with DocBuilder {
      
      val input = addPosition(
        SampleTrees.sixDocuments
          .doc6.config(SampleConfig.targetFormats("zzz"))
          .build
          .tree
      )
        
      val finalInput = input.copy(content = input.content :+ fontConfigTree)
      
      val staticDocs = Seq(
        staticDoc(1, Root),
        staticDoc(2, Root),
        staticDoc(3, Root / "dir1"),
        staticDoc(4, Root / "dir1"),
        staticDoc(5, Root / "dir2"),
        staticDoc(6, Root / "dir2"),
        staticDoc(7, Root / "dir2", Some("zzz"))
      )
      override def treeRoot = DocumentTreeRoot(finalInput, staticDocuments = staticDocs.map(doc => StaticDocument(doc.path, doc.formats)))
      
      val expectedStatic = staticDocs.dropRight(1).map(_.path)
      val expectedRendered = renderedRoot(List(
        renderedDoc(Root / "doc-1.txt", renderedDoc(1)),
        renderedDoc(Root / "doc-2.txt", renderedDoc(2)),
        renderedTree(Root / "tree-1", List(
          renderedDoc(Root / "tree-1" / "doc-3.txt", renderedDoc(3)),
          renderedDoc(Root / "tree-1" / "doc-4.txt", renderedDoc(4))
        )),
        renderedTree(Root / "tree-2", List(
          renderedDoc(Root / "tree-2" / "doc-5.txt", renderedDoc(5))
        ))
      ), None, staticDocuments = expectedStatic ++ TestTheme.staticASTPaths)

      renderer
        .use (_
          .from(treeRoot)
          .copying(staticDocs)
          .toOutput(StringTreeOutput)
          .render
        )
        .assertEquals(expectedRendered)
    }

    "render a tree while excluding all unversioned documents, based on configuration" in new HTMLRenderer with DocBuilder {

      val versions = Versions(Version("0.4.x", "0.4"), Seq(), Seq(), renderUnversioned = false)
      val input = SampleTrees.sixDocuments
        .root.config(_.withValue(versions))
        .tree1.config(SampleConfig.versioned(true))
        .tree2.config(SampleConfig.versioned(true))
        .build
        .tree
      val finalInput = input.copy(content = input.content :+ fontConfigTree)

      val staticDocs = Seq(
        staticDoc(1, Root),
        staticDoc(2, Root),
        staticDoc(3, Root / "tree-1"),
        staticDoc(4, Root / "tree-1"),
        staticDoc(5, Root / "tree-2"),
        staticDoc(6, Root / "tree-2")
      )
      override def treeRoot = DocumentTreeRoot(finalInput, staticDocuments = staticDocs.map(doc => StaticDocument(doc.path, doc.formats)))
      override def hasTitle: Boolean = false

      def docHTML(num: Int): String = s"<p>Text $num</p>"
      
      val expectedStatic = staticDocs.drop(2).map(in => Root / "0.4" / in.path.relative)
      val expectedRendered = renderedRoot(List(
        renderedTree(Root / "0.4", List(
          renderedTree(Root / "0.4" / "tree-1", List(
            renderedDoc(Root / "0.4" / "tree-1" / "doc-3.html", docHTML(3)),
            renderedDoc(Root / "0.4" / "tree-1" / "doc-4.html", docHTML(4))
          )),
          renderedTree(Root / "0.4" / "tree-2", List(
            renderedDoc(Root / "0.4" / "tree-2" / "doc-5.html", docHTML(5)),
            renderedDoc(Root / "0.4" / "tree-2" / "doc-6.html", docHTML(6))
          ))
        ))
      ), None, staticDocuments = expectedStatic :+ Root / "laika" / "versionInfo.json")

      renderer
        .use (_
          .from(treeRoot)
          .copying(staticDocs)
          .toOutput(StringTreeOutput)
          .render
        )
        .assertEquals(expectedRendered)
    }
    
    trait TwoPhaseRenderer extends DocBuilder {
      val rootElem: RootElement = RootElement(titleWithId("Title"), p("bbb"))
      val subElem: RootElement = RootElement(titleWithId("Sub Title"), p("ccc"))
  
      val input = twoDocs(rootElem, subElem)
  
      val expectedResult: String = """RootElement - Blocks: 2
        |. Title(Id(title) + Styles(title)) - Spans: 1
        |. . Text - 'Title'
        |. Paragraph - Spans: 1
        |. . Text - 'bbb'
        |RootElement - Blocks: 2
        |. Title(Id(sub-title) + Styles(title)) - Spans: 1
        |. . Text - 'Sub Title'
        |. Paragraph - Spans: 1
        |. . Text - 'ccc'
        |""".stripMargin
      
      val renderer: Resource[IO, BinaryTreeRenderer[IO]] = Renderer
        .of(TestRenderResultProcessor)
        .parallel[IO]
        .build
    }
  
    "render a tree with two documents using a RenderResultProcessor writing to an output stream" in new TwoPhaseRenderer {
      renderer.use { r =>
        withByteArrayTextOutput { out =>
          r.from(DocumentTreeRoot(input)).toStream(IO.pure(out)).render
        }
      }.assertEquals(expectedResult)
      
    }
    
    // TODO - reactivate codec tests
    //
    //    "render a document to a java.io.OutputStream, specifying the encoding explicitly" in {
    //      withByteArrayTextOutput("ISO-8859-1") { out =>
    //        renderer.from(rootElem).toStream(IO.pure(out))(Codec.ISO8859).render.void
    //      }.assertEquals(expected)
    //    }
    //
    //    "render a document to a java.io.OutputStream, specifying the encoding implicitly" in {
    //      implicit val codec: Codec = Codec.ISO8859
    //      withByteArrayTextOutput("ISO-8859-1") { out =>
    //        renderer.from(rootElem).toStream(IO.pure(out)).render.void
    //      }.assertEquals(expected)
    //    }
  
    "render a tree with two documents using a RenderResultProcessor writing to a file" in new TwoPhaseRenderer {
      val res = renderer.use { r =>
        for {
          f   <- newTempFile
          _   <- r.from(DocumentTreeRoot(input)).toFile(f).render
          res <- readFile(f)
        } yield res
      }
      res.assertEquals(expectedResult)
    }
  
    trait FileSystemTest extends DocBuilder {
      import cats.implicits._
      
      val input = SampleTrees.sixDocuments.build
      
      def readFiles (base: String): IO[List[String]] = {
        List(
          readFile(base+"/doc-1.txt"),
          readFile(base+"/doc-2.txt"),
          readFile(base+"/tree-1/doc-3.txt"),
          readFile(base+"/tree-1/doc-4.txt"),
          readFile(base+"/tree-2/doc-5.txt"),
          readFile(base+"/tree-2/doc-6.txt")
        ).sequence
      }
    }
  
    "render to a directory using the toDirectory method" in {
      new FileSystemTest {
        val renderer = Renderer.of(AST)
          .parallel[IO]
          .build

        val expectedFileContents: List[String] = (1 to 6).map(renderedDoc).toList
        
        val res = for {
          f   <- newTempDirectory
          _   <- renderer.use(_.from(input).toDirectory(f).render)
          res <- readFiles(f.getPath)
        } yield res
        
        res.assertEquals(expectedFileContents)  
      }
    }

    "render to a directory with existing versioned renderer output" in {
      new FileSystemTest {

        val htmlRenderer = Renderer.of(HTML)
          .parallel[IO]
          .build

        val versions = Versions(
          Version("0.4.x", "0.4"), 
          Seq(Version("0.3.x", "0.3"), Version("0.2.x", "0.2"), Version("0.1.x", "0.1", "toc.html")), 
          Seq(Version("0.5.x", "0.5"))
        )

        val versionedInput = SampleTrees.sixDocuments
          .root.config(_.withValue(versions))
          .tree1.config(SampleConfig.versioned(true))
          .tree2.config(SampleConfig.versioned(true))
          .build
        
        def mkDirs (dir: File): IO[Unit] = IO {
          Seq("0.1/tree-1", "0.1/tree-2", "0.2/tree-1", "0.2/tree-2", "0.3/tree-1", "0.3/tree-2").foreach { name =>
            new File(dir, name).mkdirs()
          }
        }

        val paths = versionedInput.tree.allDocuments.map(_.path.withSuffix("html").toString)
        val versionedPaths = paths.map("0.1" + _) ++ paths.drop(1).map("0.2" + _) ++ paths.dropRight(1).map("0.3" + _)
        def writeExistingVersionedFiles (dir: File): IO[Unit] = versionedPaths.toList.map { path =>
          val file = new File(dir, path)
          writeFile(file, "<html></html>")
        }.sequence.void

        val expectedFileContents: List[String] = (1 to 6).map(num => s"<p>Text $num</p>").toList
        
        val expectedVersionInfo = 
          """{
            |  "versions": [
            |    { "displayValue": "0.5.x", "pathSegment": "0.5", "fallbackLink": "/index.html" },
            |    { "displayValue": "0.4.x", "pathSegment": "0.4", "fallbackLink": "/index.html" },
            |    { "displayValue": "0.3.x", "pathSegment": "0.3", "fallbackLink": "/index.html" },
            |    { "displayValue": "0.2.x", "pathSegment": "0.2", "fallbackLink": "/index.html" },
            |    { "displayValue": "0.1.x", "pathSegment": "0.1", "fallbackLink": "/toc.html" }
            |  ],
            |  "linkTargets": [
            |    { "path": "/doc-1.html", "versions": ["0.1","0.3"] },
            |    { "path": "/doc-2.html", "versions": ["0.1","0.2","0.3"] },
            |    { "path": "/tree-1/doc-3.html", "versions": ["0.1","0.2","0.3","0.4"] },
            |    { "path": "/tree-1/doc-4.html", "versions": ["0.1","0.2","0.3","0.4"] },
            |    { "path": "/tree-2/doc-5.html", "versions": ["0.1","0.2","0.3","0.4"] },
            |    { "path": "/tree-2/doc-6.html", "versions": ["0.1","0.2","0.4"] }
            |  ]
            |}""".stripMargin

        def readVersionedFiles (base: String): IO[List[String]] = {
          List(
            readFile(base+"/doc-1.html"),
            readFile(base+"/doc-2.html"),
            readFile(base+"/0.4/tree-1/doc-3.html"),
            readFile(base+"/0.4/tree-1/doc-4.html"),
            readFile(base+"/0.4/tree-2/doc-5.html"),
            readFile(base+"/0.4/tree-2/doc-6.html")
          ).sequence
        }
        
        def readVersionInfo (base: String): IO[String] = readFile(base+"/laika/versionInfo.json")
        
        val res = for {
          f   <- newTempDirectory
          _   <- mkDirs(f)
          _   <- writeExistingVersionedFiles(f)
          _   <- htmlRenderer.use(_.from(versionedInput).toDirectory(f).render)
          res <- readVersionedFiles(f.getPath)
          vi  <- readVersionInfo(f.getPath)
        } yield (res, vi)

        res.assertEquals((expectedFileContents, expectedVersionInfo))
      }
    }
  
    "render to a directory using a document with non-ASCII characters" in new ASTRenderer {
      val expected = """RootElement - Blocks: 1
                       |. Paragraph - Spans: 1
                       |. . Text - 'Doc äöü'""".stripMargin
      val input = rootTree(RootElement(p("Doc äöü")))
      
      val res = for {
        f   <- newTempDirectory
        _   <- renderer.use(_.from(treeRoot).toDirectory(f)(Codec.ISO8859).render)
        res <- readFile(new File(f, "doc.txt"), Codec.ISO8859)
      } yield res
      
      res.assertEquals(expected)
    }
    
  }
}
