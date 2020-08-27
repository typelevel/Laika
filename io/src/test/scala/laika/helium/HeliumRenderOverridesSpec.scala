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

import cats.effect.IO
import laika.api.Transformer
import laika.api.builder.TransformerBuilder
import laika.ast.Path
import laika.ast.Path.Root
import laika.format.{HTML, Markdown}
import laika.helium.config.{AnchorPlacement, HeliumIcon}
import laika.io.helper.{InputBuilder, ResultExtractor, StringOps}
import laika.io.implicits._
import laika.io.model.StringTreeOutput
import laika.io.{FileIO, IOFunSuite}
import laika.render.HTMLFormatter
import laika.rewrite.nav.{ChoiceConfig, SelectionConfig, Selections}
import laika.theme._

class HeliumRenderOverridesSpec extends IOFunSuite with InputBuilder with ResultExtractor with StringOps {

  type ConfigureTransformer = TransformerBuilder[HTMLFormatter] => TransformerBuilder[HTMLFormatter]
  
  def transformer (theme: ThemeProvider, configure: ConfigureTransformer) = {
    val builder = Transformer.from(Markdown).to(HTML)
    configure(builder)  
      .io(FileIO.blocker)
      .parallel[IO]
      .withTheme(theme)
      .build
  }
  
  def transformAndExtract(inputs: Seq[(Path, String)], helium: Helium, start: String, end: String, 
                          configure: ConfigureTransformer): IO[String] = 
    transformer(helium.build, configure).use { t =>
      for {
        resultTree <- t.fromInput(build(inputs)).toOutput(StringTreeOutput).transform
        res        <- IO.fromEither(resultTree.extractTidiedSubstring(Root / "doc.html", start, end)
          .toRight(new RuntimeException("Missing document under test")))
      } yield res
    }
  
  def transformAndExtract(input: String, helium: Helium = Helium.defaults, configure: ConfigureTransformer = identity): IO[String] = {
    transformAndExtract(Seq(Root / "doc.md" -> input), helium, "<main class=\"content\">", "</main>", configure)
  }
    
  test("selections as tabs") {
    val input =
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
    val expected = 
      """<div class="tab-container" data-tab-group="config">
        |<ul class="tab-group">
        |<li class="tab" data-choice-name="sbt">sbt Plugin</li>
        |<li class="tab" data-choice-name="library">Library API</li>
        |</ul>
        |<div class="tab-content" data-choice-name="sbt">
        |<p>1st choice</p>
        |</div>
        |<div class="tab-content" data-choice-name="library">
        |<p>2nd choice</p>
        |</div>
        |</div>""".stripMargin
    val configure: ConfigureTransformer = _.withConfigValue(Selections(
      SelectionConfig("config",
        ChoiceConfig("sbt", "sbt Plugin"),
        ChoiceConfig("library", "Library API")
      ).withSeparateEbooks
    ))
    transformAndExtract(input, configure = configure).assertEquals(expected)
  }

  // orig: <span class="icon icofont-xlg">
  
  test("callouts") {
    val input =
      """
        |@:callout(warning)
        |
        |You really should not do this.
        |
        |@:@
      """.stripMargin
    val expected = 
      s"""<div class="callout warning">
         |<i class="icofont-laika">${HeliumIcon.warning.codePointAsEntity}</i>
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
         |<h2 id="some-headline" class="section"><a class="anchor-link" href="#some-headline"><i class="icofont-laika">${HeliumIcon.link.codePointAsEntity}</i></a>Some Headline</h2>""".stripMargin
    transformAndExtract(headlineInput).assertEquals(expected)
  }

  test("anchors for headers - right placement") {
    val expected =
      s"""<h1 id="title" class="title">Title</h1>
         |<h2 id="some-headline" class="section">Some Headline<a class="anchor-link" href="#some-headline"><i class="icofont-laika">${HeliumIcon.link.codePointAsEntity}</i></a></h2>""".stripMargin
    val layout = Helium.defaults.siteSettings.webLayout
    val helium = Helium.defaults.site.layout(layout.contentWidth, layout.navigationWidth, 
      layout.defaultBlockSpacing, layout.defaultLineHeight, AnchorPlacement.Right)
    transformAndExtract(headlineInput, helium).assertEquals(expected)
  }

  test("anchors for headers - disabled") {
    val expected =
      """<h1 id="title" class="title">Title</h1>
        |<h2 id="some-headline" class="section">Some Headline</h2>""".stripMargin
    val layout = Helium.defaults.siteSettings.webLayout
    val helium = Helium.defaults.site.layout(layout.contentWidth, layout.navigationWidth,
      layout.defaultBlockSpacing, layout.defaultLineHeight, AnchorPlacement.None)
    transformAndExtract(headlineInput, helium).assertEquals(expected)
  }

}
