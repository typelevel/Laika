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

import laika.api.Renderer
import laika.ast.Path.Root
import laika.ast._
import laika.ast.sample.ParagraphCompanionShortcuts
import laika.config.{ConfigBuilder, LaikaKeys}
import laika.format.EPUB
import laika.rewrite.OutputContext
import laika.rewrite.nav.{ConfigurablePathTranslator, PathAttributes, TargetFormats, TranslatorConfig}
import munit.CatsEffectSuite

/**
  * @author Jens Halm
  */
class XHTMLRendererSpec extends CatsEffectSuite with ParagraphCompanionShortcuts {

  private val defaultRenderer = Renderer.of(EPUB.XHTML).build
  
  trait DocBuilder {

    def markupDoc (num: Int, path: Path = Root): Document = Document(path / ("doc"+num), RootElement(p("Doc"+num)))

  }

  test("paragraph containing a citation link with an epub:type attribute") {
    val elem = p(Text("some "), CitationLink("ref", "label"), Text(" span"))
    val html = """<p>some <a class="citation" href="#ref" epub:type="noteref">[label]</a> span</p>"""
    assertEquals(defaultRenderer.render(elem), html)
  }

  test("paragraph containing a footnote link with an epub:type attribute") {
    val elem = p(Text("some "), FootnoteLink("id", "label"), Text(" span"))
    val html = """<p>some <a class="footnote" href="#id" epub:type="noteref">[label]</a> span</p>"""
    assertEquals(defaultRenderer.render(elem), html)
  }

  test("footnote with an epub:type attribute") {
    val elem = Footnote("label", List(p("a"), p("b")), Id("id"))
    val html =
      """<aside id="id" class="footnote" epub:type="footnote">
        |  <p>a</p>
        |  <p>b</p>
        |</aside>""".stripMargin
    assertEquals(defaultRenderer.render(elem), html)
  }

  test("citation with an epub:type attribute") {
    val elem = Citation("ref", List(p("a"), p("b")), Id("ref"))
    val html =
      """<aside id="ref" class="citation" epub:type="footnote">
        |  <p>a</p>
        |  <p>b</p>
        |</aside>""".stripMargin
    assertEquals(defaultRenderer.render(elem), html)
  }

  test("choice group without selections") {
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
    assertEquals(defaultRenderer.render(elem), html)
  }

  test("translate to external URL when an internal link is not defined for EPUB as a target") {
    val target = ResolvedInternalTarget(Path.parse("/foo#ref"), RelativePath.parse("foo#ref"), TargetFormats.Selected("html"))
    val elem = p(Text("some "), SpanLink(target)("link"), Text(" span"))
    val translator = {
      val config = ConfigBuilder.empty.withValue(LaikaKeys.siteBaseURL, "http://external/").build
      val tConfig = TranslatorConfig.readFrom(config).getOrElse(TranslatorConfig.empty)
      val lookup: Path => Option[PathAttributes] = path => 
        if (path == Root / "doc") Some(PathAttributes(isStatic = false, isVersioned = false)) else None
      ConfigurablePathTranslator(tConfig, OutputContext(EPUB), Root / "doc", lookup)
    }
    val html = """<p>some <a href="http://external/foo.html#ref">link</a> span</p>"""
    assertEquals(defaultRenderer.render(elem, Root / "doc", translator, StyleDeclarationSet.empty), html)
  }
    
}
