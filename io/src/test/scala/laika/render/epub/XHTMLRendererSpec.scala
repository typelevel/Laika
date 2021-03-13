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

package laika.render.epub

import cats.effect.IO
import laika.api.Renderer
import laika.ast.Path.Root
import laika.ast._
import laika.ast.sample.ParagraphCompanionShortcuts
import laika.config.{ConfigBuilder, LaikaKeys}
import laika.format.EPUB
import laika.io.{FileIO, IOWordSpec}
import laika.io.implicits._
import laika.io.model.{RenderedDocument, RenderedTree, StringTreeOutput}
import laika.rewrite.nav.{ConfigurablePathTranslator, TargetFormats, TranslatorSpec}

/**
  * @author Jens Halm
  */
class XHTMLRendererSpec extends IOWordSpec with ParagraphCompanionShortcuts with FileIO {

  private val defaultRenderer = Renderer.of(EPUB.XHTML).build
  
  trait DocBuilder {

    def markupDoc (num: Int, path: Path = Root): Document = Document(path / ("doc"+num), RootElement(p("Doc"+num)))

  }

  trait StringRenderer {

    private def collectDocuments (result: RenderedTree): Seq[RenderedDocument] =
      (result.content.collect { case doc: RenderedDocument => Seq(doc) } ++
        result.content.collect { case tree: RenderedTree => collectDocuments(tree) }).flatten
    
    def renderedDocs (root: DocumentTreeRoot): IO[Seq[RenderedDocument]] =
      Renderer
        .of(EPUB.XHTML)
        .parallel[IO]
        .build
        .use(_
          .from(root)
          .toOutput(StringTreeOutput)
          .render
        )
        .map(root => collectDocuments(root.tree))
    
    def renderedXhtml (num: Int, style1: String, style2: String): String = s"""<?xml version="1.0" encoding="UTF-8"?>
     |<!DOCTYPE html>
     |<html xmlns="http://www.w3.org/1999/xhtml" xmlns:epub="http://www.idpf.org/2007/ops">
     |  <head>
     |    <meta charset="utf-8" />
     |    <meta name="generator" content="laika" />
     |    <title></title>
     |    <link rel="stylesheet" type="text/css" href="$style1" />
     |    <link rel="stylesheet" type="text/css" href="$style2" />
     |  </head>
     |  <body epub:type="bodymatter">
     |    <div class="content">
     |      <p>Doc$num</p>
     |    </div>
     |  </body>
     |</html>""".stripMargin

  }

  "The XHTML Renderer for EPUB" should {

    "render a paragraph containing a citation link with an epub:type attribute" in {
      val elem = p(Text("some "), CitationLink("ref", "label"), Text(" span"))
      defaultRenderer.render(elem) should be("""<p>some <a class="citation" href="#ref" epub:type="noteref">[label]</a> span</p>""")
    }

    "render a paragraph containing a footnote link with an epub:type attribute" in {
      val elem = p(Text("some "), FootnoteLink("id", "label"), Text(" span"))
      defaultRenderer.render(elem) should be("""<p>some <a class="footnote" href="#id" epub:type="noteref">[label]</a> span</p>""")
    }

    "render a footnote with an epub:type attribute" in {
      val elem = Footnote("label", List(p("a"), p("b")), Id("id"))
      val html =
        """<aside id="id" class="footnote" epub:type="footnote">
          |  <p>a</p>
          |  <p>b</p>
          |</aside>""".stripMargin
      defaultRenderer.render(elem) should be(html)
    }

    "render a citation with an epub:type attribute" in {
      val elem = Citation("ref", List(p("a"), p("b")), Id("ref"))
      val html =
        """<aside id="ref" class="citation" epub:type="footnote">
          |  <p>a</p>
          |  <p>b</p>
          |</aside>""".stripMargin
      defaultRenderer.render(elem) should be(html)
    }

    "render a choice group without selections" in {
      val elem = Selection("config", Seq(
        Choice("name-a","label-a", List(p("common"), p("11\n22"))),
        Choice("name-b","label-b", List(p("common"), p("33\n44")))
      ))
      val html = s"""<p><strong>label-a</strong></p>
        |<p>common</p>
        |<p>11
        |22</p>
        |<p><strong>label-b</strong></p>
        |<p>common</p>
        |<p>33
        |44</p>""".stripMargin
      defaultRenderer.render(elem) should be (html)
    }

    "translate to external URL when an internal link is not defined for EPUB as a target" in {
      val target = ResolvedInternalTarget(Path.parse("/foo#ref"), RelativePath.parse("foo#ref"), TargetFormats.Selected("html"))
      val elem = p(Text("some "), SpanLink(target)("link"), Text(" span"))
      val config = ConfigBuilder.empty.withValue(LaikaKeys.siteBaseURL, "http://external/").build
      val lookup: Path => Option[TranslatorSpec] = path => 
        if (path == Root / "doc") Some(TranslatorSpec(isStatic = false, isVersioned = false)) else None
      val translator = ConfigurablePathTranslator(config, "epub.xhtml", "epub", Root / "doc", lookup)
      defaultRenderer.render(elem, Root / "doc", translator, StyleDeclarationSet.empty) should be ("""<p>some <a href="http://external/foo.html#ref">link</a> span</p>""")
    }
    
  }

}
