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

package laika.helium

import cats.effect.{ IO, Resource }
import laika.api.Transformer
import laika.api.builder.TransformerBuilder
import laika.api.format.TagFormatter
import laika.ast.{ Icon, IconGlyph, Path }
import laika.ast.Path.Root
import laika.config.{ ChoiceConfig, SelectionConfig, Selections, SyntaxHighlighting }
import laika.format.{ HTML, Markdown }
import laika.helium.config.{ AnchorPlacement, HeliumIcon }
import laika.io.api.TreeTransformer
import laika.io.helper.{ InputBuilder, ResultExtractor, StringOps }
import laika.io.implicits.*
import laika.markdown.github.GitHubFlavor
import laika.theme.*
import munit.CatsEffectSuite

class HeliumRenderOverridesSpec extends CatsEffectSuite with InputBuilder with ResultExtractor
    with StringOps {

  type ConfigureTransformer = TransformerBuilder[TagFormatter] => TransformerBuilder[TagFormatter]

  private val heliumBase = Helium.defaults.site.landingPage()

  def transformer(
      theme: ThemeProvider,
      configure: ConfigureTransformer
  ): Resource[IO, TreeTransformer[IO]] = {
    val builder = Transformer.from(Markdown).to(HTML).using(GitHubFlavor, SyntaxHighlighting)
    configure(builder)
      .parallel[IO]
      .withTheme(theme)
      .build
  }

  def transformAndExtract(
      inputs: Seq[(Path, String)],
      helium: Helium,
      start: String,
      end: String,
      configure: ConfigureTransformer
  ): IO[String] =
    transformer(helium.build, configure).use { t =>
      for {
        resultTree <- t.fromInput(build(inputs)).toMemory.transform
        res        <- IO.fromEither(
          resultTree.extractTidiedSubstring(Root / "doc.html", start, end)
            .toRight(new RuntimeException("Missing document under test"))
        )
      } yield res
    }

  def transformAndExtract(
      input: String,
      helium: Helium = heliumBase,
      configure: ConfigureTransformer = identity
  ): IO[String] = {
    transformAndExtract(
      Seq(Root / "doc.md" -> input),
      helium,
      "<main class=\"content\">",
      "<hr class=\"footer-rule\"/>",
      configure
    )
  }

  def entity(icon: Icon): String = icon match {
    case fig: IconGlyph => fig.codePointAsEntity
    case _              => ""
  }

  test("selections as tabs") {
    val input                           =
      """
        |@:select(config)
        |
        |@:choice(sbt)
        |
        |1st choice
        |
        |@:choice(library)
        |
        |2nd choice
        |
        |@:@
      """.stripMargin
    val expected                        =
      """<div class="tab-container" data-tab-group="config">
        |<ul class="tab-group">
        |<li class="tab active" data-choice-name="sbt"><a href="#">sbt Plugin</a></li>
        |<li class="tab" data-choice-name="library"><a href="#">Library API</a></li>
        |</ul>
        |<div class="tab-content active" data-choice-name="sbt">
        |<p>1st choice</p>
        |</div>
        |<div class="tab-content" data-choice-name="library">
        |<p>2nd choice</p>
        |</div>
        |</div>""".stripMargin
    val configure: ConfigureTransformer = _.withConfigValue(
      Selections(
        SelectionConfig(
          "config",
          ChoiceConfig("sbt", "sbt Plugin"),
          ChoiceConfig("library", "Library API")
        ).withSeparateEbooks
      )
    )
    transformAndExtract(input, configure = configure).assertEquals(expected)
  }

  test("callouts") {
    val input    =
      """
        |@:callout(warning)
        |
        |You really should not do this.
        |
        |@:@
      """.stripMargin
    val expected =
      s"""<div class="callout warning">
         |<i class="icofont-laika warning">${entity(HeliumIcon.warning)}</i>
         |<p>You really should not do this.</p>
         |</div>""".stripMargin
    transformAndExtract(input).assertEquals(expected)
  }

  private val headlineInput =
    """
      |Title
      |=====
      |
      |## Some Headline
    """.stripMargin

  // orig: <span class="anchor icofont-sm">

  test("anchors for headers - left placement (default)") {
    val expected =
      s"""<h1 id="title" class="title">Title</h1>
         |<h2 id="some-headline" class="section"><a class="anchor-link left" href="#some-headline"><i class="icofont-laika link">${
          entity(HeliumIcon.link)
        }</i></a>Some Headline</h2>""".stripMargin
    transformAndExtract(headlineInput).assertEquals(expected)
  }

  test("anchors for headers - right placement") {
    val expected =
      s"""<h1 id="title" class="title">Title</h1>
         |<h2 id="some-headline" class="section">Some Headline<a class="anchor-link right" href="#some-headline"><i class="icofont-laika link">${
          entity(HeliumIcon.link)
        }</i></a></h2>""".stripMargin
    val layout   = Helium.defaults.siteSettings.layout
    val helium   = heliumBase.site.layout(
      layout.contentWidth,
      layout.navigationWidth,
      layout.topBarHeight,
      layout.defaultBlockSpacing,
      layout.defaultLineHeight,
      AnchorPlacement.Right
    )
    transformAndExtract(headlineInput, helium).assertEquals(expected)
  }

  test("anchors for headers - disabled") {
    val expected =
      """<h1 id="title" class="title">Title</h1>
        |<h2 id="some-headline" class="section">Some Headline</h2>""".stripMargin
    val layout   = Helium.defaults.siteSettings.layout
    val helium   = heliumBase.site.layout(
      layout.contentWidth,
      layout.navigationWidth,
      layout.topBarHeight,
      layout.defaultBlockSpacing,
      layout.defaultLineHeight,
      AnchorPlacement.None
    )
    transformAndExtract(headlineInput, helium).assertEquals(expected)
  }

  test("mermaid block without nested <code> elements") {
    val markup   =
      """
        |Title
        |=====
        |
        |```mermaid
        |graph TD 
        |A[Client] --> B[Load Balancer] 
        |B --> C[Server01] 
        |B --> D[Server02]
        |```
      """.stripMargin
    val expected = """<h1 id="title" class="title">Title</h1>
                     |<pre class="mermaid">graph TD
                     |A[Client] --&gt; B[Load Balancer]
                     |B --&gt; C[Server01]
                     |B --&gt; D[Server02]</pre>""".stripMargin
    transformAndExtract(markup, heliumBase).assertEquals(expected)
  }

}
