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

import java.util.Locale
import cats.effect.{IO, Resource}
import laika.api.Transformer
import laika.ast.{Image, Path}
import laika.ast.Path.Root
import laika.format.{HTML, Markdown}
import laika.helium.config._
import laika.io.api.TreeTransformer
import laika.io.helper.{InputBuilder, ResultExtractor, StringOps}
import laika.io.implicits._
import laika.io.model.StringTreeOutput
import laika.rewrite.link.LinkConfig
import laika.theme._
import munit.CatsEffectSuite

class HeliumLandingPageSpec extends CatsEffectSuite with InputBuilder with ResultExtractor with StringOps {

  def transformer (theme: ThemeProvider): Resource[IO, TreeTransformer[IO]] = Transformer
    .from(Markdown)
    .to(HTML)
    .withConfigValue(LinkConfig(excludeFromValidation = Seq(Root)))
    .parallel[IO]
    .withTheme(theme)
    .build
  
  val inputs = Seq(
    Root / "doc-1.md" -> "text",
    Root / "doc-2.md" -> "text",
    Root / "home.png" -> ""
  )
  
  def transformAndExtract(inputs: Seq[(Path, String)], helium: Helium, start: String, end: String): IO[String] = transformer(helium.build).use { t =>
    for {
      resultTree <- t.fromInput(build(inputs)).toOutput(StringTreeOutput).transform
      res        <- IO.fromEither(resultTree.extractTidiedSubstring(Root / "index.html", start, end)
        .toRight(new RuntimeException("Missing document under test")))
    } yield res
  }
  
  private val teaserHTML = """<div class="teasers">
                             |<div class="teaser">
                             |<h2>Teaser 1</h2>
                             |<p>Description 1</p>
                             |</div>
                             |<div class="teaser">
                             |<h2>Teaser 2</h2>
                             |<p>Description 2</p>
                             |</div>
                             |<div class="teaser">
                             |<h2>Teaser 3</h2>
                             |<p>Description 3</p>
                             |</div>
                             |</div>""".stripMargin
    
  test("no landing page configured") {
    transformAndExtract(inputs, Helium.defaults, "", "")
      .interceptMessage[RuntimeException]("Missing document under test")
  }
  
  test("full landing page configured") {
    val expected = s"""<head>
                     |<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
                     |<meta charset="utf-8">
                     |<meta name="viewport" content="width=device-width, initial-scale=1.0">
                     |<meta name="generator" content="Laika 0.18.0 + Helium Theme" />
                     |<title></title>
                     |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
                     |<link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/tonsky/FiraCode@1.207/distr/fira_code.css">
                     |<link rel="stylesheet" type="text/css" href="helium/icofont.min.css" />
                     |<link rel="stylesheet" type="text/css" href="helium/laika-helium.css" />
                     |<link rel="stylesheet" type="text/css" href="helium/landing.page.css" />
                     |<script src="helium/laika-helium.js"></script>
                     |<script> /* for avoiding page load transitions */ </script>
                     |</head>
                     |<body>
                     |<div id="header">
                     |<div id="header-left">
                     |<img src="home.png" alt="Project Logo">
                     |<h1>My Project</h1>
                     |<h2>Awesome Hyperbole Overkill</h2>
                     |</div>
                     |<div id="header-right">
                     |<p>Latest Stable Release</p>
                     |<p class="large">2.3.5</p>
                     |<p>Latest Milestone Release</p>
                     |<p class="large">2.4.0-M2</p>
                     |<p>License</p>
                     |<p class="large">MIT</p>
                     |<div id="docs">
                     |<p>Documentation</p>
                     |<ul>
                     |<li><a class="text-link" href="doc-1.html">Doc 1</a></li>
                     |<li><a class="text-link" href="doc-2.html">Doc 2</a></li>
                     |</ul>
                     |</div>
                     |<p class="medium"><a class="icon-link" href="doc-2.html"><i class="icofont-laika" title="Demo">&#xeeea;</i></a></p>
                     |<p class="medium"><a class="button-link" href="http://somewhere.com/">Somewhere</a></p>
                     |</div>
                     |</div>
                     |$teaserHTML
                     |</body>""".stripMargin
    val imagePath = Root / "home.png"
    val helium = Helium.defaults.site.landingPage(
      logo = Some(Image.internal(imagePath, alt = Some("Project Logo"))),
      title = Some("My Project"),
      subtitle = Some("Awesome Hyperbole Overkill"),
      latestReleases = Seq(
        ReleaseInfo("Latest Stable Release", "2.3.5"),
        ReleaseInfo("Latest Milestone Release", "2.4.0-M2")
      ),
      license = Some("MIT"),
      documentationLinks = Seq(
        TextLink.internal(Root / "doc-1.md", "Doc 1"),
        TextLink.internal(Root / "doc-2.md", "Doc 2")
      ),
      projectLinks = Seq(
        IconLink.internal(Root / "doc-2.md", HeliumIcon.demo),
        ButtonLink.external("http://somewhere.com/", "Somewhere")
      ),
      teasers = Seq(
        Teaser("Teaser 1", "Description 1"),
        Teaser("Teaser 2", "Description 2"),
        Teaser("Teaser 3", "Description 3")
      )
    )
    transformAndExtract(inputs, helium, s"""<html lang="${Locale.getDefault.toLanguageTag}">""", "</html>")
      .assertEquals(expected)
  }

  test("partial landing page configured with custom content and fragment") {
    val expected = s"""<head>
                     |<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
                     |<meta charset="utf-8">
                     |<meta name="viewport" content="width=device-width, initial-scale=1.0">
                     |<meta name="generator" content="Laika 0.18.0 + Helium Theme" />
                     |<title></title>
                     |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
                     |<link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/tonsky/FiraCode@1.207/distr/fira_code.css">
                     |<link rel="stylesheet" type="text/css" href="helium/icofont.min.css" />
                     |<link rel="stylesheet" type="text/css" href="helium/laika-helium.css" />
                     |<link rel="stylesheet" type="text/css" href="helium/landing.page.css" />
                     |<script src="helium/laika-helium.js"></script>
                     |<script> /* for avoiding page load transitions */ </script>
                     |</head>
                     |<body>
                     |<div id="header">
                     |<div id="header-left">
                     |<img src="home.png" alt="Project Logo">
                     |<h2>Awesome Hyperbole Overkill</h2>
                     |</div>
                     |<div id="header-right">
                     |<p>Latest Release</p>
                     |<p class="large">2.3.5</p>
                     |<p class="medium"><a class="icon-link" href="doc-2.html"><i class="icofont-laika" title="Demo">&#xeeea;</i></a></p>
                     |<p class="medium"><a class="button-link" href="http://somewhere.com/">Somewhere</a></p>
                     |</div>
                     |</div>
                     |<p class="header">Some <em>Header</em></p>
                     |$teaserHTML
                     |<p>Some <em>markup</em> here.</p>
                     |</body>""".stripMargin
    val imagePath = Root / "home.png"
    val helium = Helium.defaults.site.landingPage(
      logo = Some(Image.internal(imagePath, alt = Some("Project Logo"))),
      subtitle = Some("Awesome Hyperbole Overkill"),
      latestReleases = Seq(
        ReleaseInfo("Latest Release", "2.3.5")
      ),
      projectLinks = Seq(
        IconLink.internal(Root / "doc-2.md", HeliumIcon.demo),
        ButtonLink.external("http://somewhere.com/", "Somewhere")
      ),
      teasers = Seq(
        Teaser("Teaser 1", "Description 1"),
        Teaser("Teaser 2", "Description 2"),
        Teaser("Teaser 3", "Description 3")
      )
    )
    val content =
      """@:fragment(header)
        |Some *Header*
        |@:@
        |
        |Some *markup* here.""".stripMargin
    val inputsWithExtraDoc = inputs :+ (Root / "landing-page.md", content)
    transformAndExtract(inputsWithExtraDoc, helium, s"""<html lang="${Locale.getDefault.toLanguageTag}">""", "</html>")
      .assertEquals(expected)
  }

}
