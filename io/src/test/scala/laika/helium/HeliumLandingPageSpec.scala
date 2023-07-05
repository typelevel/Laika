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
import cats.effect.{ IO, Resource }
import laika.api.Transformer
import laika.ast.{ Image, Path }
import laika.ast.Path.Root
import laika.format.{ HTML, Markdown }
import laika.helium.config._
import laika.io.api.TreeTransformer
import laika.io.helper.{ InputBuilder, ResultExtractor, StringOps }
import laika.io.implicits._
import laika.io.model.StringTreeOutput
import laika.rewrite.{ Version, Versions }
import laika.rewrite.link.LinkConfig
import laika.theme._
import laika.theme.config.Color
import munit.CatsEffectSuite

class HeliumLandingPageSpec extends CatsEffectSuite with InputBuilder with ResultExtractor
    with StringOps {

  private val versions = Versions(
    Version("0.42.x", "0.42"),
    Seq(
      Version("0.41.x", "0.41"),
      Version("0.40.x", "0.40", fallbackLink = "toc.html")
    ),
    Seq(
      Version("0.43.x", "0.43")
    )
  )

  def transformer(theme: ThemeProvider): Resource[IO, TreeTransformer[IO]] = Transformer
    .from(Markdown)
    .to(HTML)
    .withConfigValue(LinkConfig(excludeFromValidation = Seq(Root)))
    .parallel[IO]
    .withTheme(theme)
    .build

  val inputs = Seq(
    Root / "doc-1.md"                          -> "text",
    Root / "doc-2.md"                          -> "text",
    Root / "home.png"                          -> "",
    Root / "styles" / "landing-extra.page.css" -> ""
  )

  def transformAndExtract(
      inputs: Seq[(Path, String)],
      helium: Helium,
      start: String,
      end: String
  ): IO[String] = transformer(helium.build).use { t =>
    for {
      resultTree <- t.fromInput(build(inputs)).toOutput(StringTreeOutput).transform
      res        <- IO.fromEither(
        resultTree.extractTidiedSubstring(Root / "index.html", start, end)
          .toRight(new RuntimeException("Missing document under test"))
      )
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
    transformAndExtract(
      inputs,
      Helium.defaults.site.topNavigationBar(homeLink =
        IconLink.external("http://foo.com", HeliumIcon.home)
      ),
      "",
      ""
    )
      .interceptMessage[RuntimeException]("Missing document under test")
  }

  test("full landing page configured") {
    val expected  =
      s"""<head>
         |<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
         |<meta charset="utf-8">
         |<meta name="viewport" content="width=device-width, initial-scale=1.0">
         |<meta name="generator" content="Laika ${LaikaVersion.value} + Helium Theme" />
         |<title></title>
         |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
         |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Fira+Mono:500">
         |<link rel="stylesheet" type="text/css" href="helium/icofont.min.css" />
         |<link rel="stylesheet" type="text/css" href="helium/laika-helium.css" />
         |<link rel="stylesheet" type="text/css" href="helium/landing.page.css" />
         |<link rel="stylesheet" type="text/css" href="styles/landing-extra.page.css" />
         |<script src="helium/laika-helium.js"></script>
         |<script src="helium/laika-versions.js"></script>
         |<script>initVersions("", "", "", null);</script>
         |<script> /* for avoiding page load transitions */ </script>
         |</head>
         |<body>
         |<div id="header" class="light-inverted dark-inverted">
         |<div id="header-left">
         |<img src="home.png" alt="Project Logo">
         |<h1>My Project</h1>
         |<h2>Awesome Hyperbole Overkill</h2>
         |<div class="row links">
         |<div class="menu-container version-menu">
         |<a class="text-link menu-toggle" href="#">Getting Started</a>
         |<nav class="menu-content">
         |<ul class="nav-list">
         |</ul>
         |</nav>
         |</div>
         |<span class="row links"><a class="icon-link svg-link" href="https://github.com/abcdefg/"><span class="github" title="Source Code"><svg class="svg-icon" width="100%" height="100%" viewBox="0 0 100 100" version="1.1" xmlns="http://www.w3.org/2000/svg" xml:space="preserve" style="fill-rule:evenodd;clip-rule:evenodd;stroke-linejoin:round;stroke-miterlimit:2;">
         |<g class="svg-shape">
         |<path d="M49.995,1c-27.609,-0 -49.995,22.386 -49.995,50.002c-0,22.09 14.325,40.83 34.194,47.444c2.501,0.458 3.413,-1.086 3.413,-2.412c0,-1.185 -0.043,-4.331 -0.067,-8.503c-13.908,3.021 -16.843,-6.704 -16.843,-6.704c-2.274,-5.773 -5.552,-7.311 -5.552,-7.311c-4.54,-3.103 0.344,-3.042 0.344,-3.042c5.018,0.356 7.658,5.154 7.658,5.154c4.46,7.64 11.704,5.433 14.552,4.156c0.454,-3.232 1.744,-5.436 3.174,-6.685c-11.102,-1.262 -22.775,-5.553 -22.775,-24.713c-0,-5.457 1.949,-9.92 5.147,-13.416c-0.516,-1.265 -2.231,-6.348 0.488,-13.233c0,0 4.199,-1.344 13.751,5.126c3.988,-1.108 8.266,-1.663 12.518,-1.682c4.245,0.019 8.523,0.574 12.517,1.682c9.546,-6.47 13.736,-5.126 13.736,-5.126c2.728,6.885 1.013,11.968 0.497,13.233c3.204,3.496 5.141,7.959 5.141,13.416c0,19.209 -11.691,23.436 -22.83,24.673c1.795,1.544 3.394,4.595 3.394,9.26c0,6.682 -0.061,12.076 -0.061,13.715c0,1.338 0.899,2.894 3.438,2.406c19.853,-6.627 34.166,-25.354 34.166,-47.438c-0,-27.616 -22.389,-50.002 -50.005,-50.002"/>
         |</g>
         |</svg></span></a><a class="icon-link glyph-link" href="https://gitter.im/abcdefg/"><i class="icofont-laika chat" title="Chat">&#xeed5;</i></a><a class="icon-link glyph-link" href="https://twitter.com/abcdefg/"><i class="icofont-laika twitter" title="Twitter">&#xed7a;</i></a></span>
         |</div>
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
         |<p class="medium"><a class="text-link" href="doc-1.html">Text Link</a></p>
         |<p class="medium"><a class="button-link" href="http://somewhere.com/">Somewhere</a></p>
         |<p class="medium"><span class="row links"><a class="icon-link glyph-link" href="doc-2.html"><i class="icofont-laika demo" title="Demo">&#xeeea;</i></a><a class="icon-link glyph-link" href="doc-3.md"><i class="icofont-laika info">&#xef4e;</i></a></span></p>
         |</div>
         |</div>
         |$teaserHTML
         |</body>""".stripMargin
    val imagePath = Root / "home.png"
    val helium    = Helium.defaults
      .site.versions(versions)
      .site.landingPage(
        logo = Some(Image.internal(imagePath, alt = Some("Project Logo"))),
        title = Some("My Project"),
        subtitle = Some("Awesome Hyperbole Overkill"),
        latestReleases = Seq(
          ReleaseInfo("Latest Stable Release", "2.3.5"),
          ReleaseInfo("Latest Milestone Release", "2.4.0-M2")
        ),
        license = Some("MIT"),
        titleLinks = Seq(
          VersionMenu.create(unversionedLabel = "Getting Started"),
          LinkGroup.create(
            IconLink.external("https://github.com/abcdefg/", HeliumIcon.github),
            IconLink.external("https://gitter.im/abcdefg/", HeliumIcon.chat),
            IconLink.external("https://twitter.com/abcdefg/", HeliumIcon.twitter)
          )
        ),
        documentationLinks = Seq(
          TextLink.internal(Root / "doc-1.md", "Doc 1"),
          TextLink.internal(Root / "doc-2.md", "Doc 2")
        ),
        projectLinks = Seq(
          TextLink.internal(Root / "doc-1.md", "Text Link"),
          ButtonLink.external("http://somewhere.com/", "Somewhere"),
          LinkGroup.create(
            IconLink.internal(Root / "doc-2.md", HeliumIcon.demo),
            IconLink.internal(Root / "doc-3.md", HeliumIcon.info)
          )
        ),
        teasers = Seq(
          Teaser("Teaser 1", "Description 1"),
          Teaser("Teaser 2", "Description 2"),
          Teaser("Teaser 3", "Description 3")
        ),
        styles = Seq(Root / "styles" / "landing-extra.page.css")
      )
    transformAndExtract(
      inputs,
      helium,
      s"""<html lang="${Locale.getDefault.toLanguageTag}">""",
      "</html>"
    )
      .assertEquals(expected)
  }

  test(
    "partial landing page configured with custom content and fragment and light background gradient"
  ) {
    val expected           =
      s"""<head>
         |<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
         |<meta charset="utf-8">
         |<meta name="viewport" content="width=device-width, initial-scale=1.0">
         |<meta name="generator" content="Laika ${LaikaVersion.value} + Helium Theme" />
         |<title></title>
         |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
         |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Fira+Mono:500">
         |<link rel="stylesheet" type="text/css" href="helium/icofont.min.css" />
         |<link rel="stylesheet" type="text/css" href="helium/laika-helium.css" />
         |<link rel="stylesheet" type="text/css" href="helium/landing.page.css" />
         |<script src="helium/laika-helium.js"></script>
         |<script> /* for avoiding page load transitions */ </script>
         |</head>
         |<body>
         |<div id="header" class="light-default dark-inverted">
         |<div id="header-left">
         |<img src="home.png" alt="Project Logo">
         |<h2>Awesome Hyperbole Overkill</h2>
         |</div>
         |<div id="header-right">
         |<p>Latest Release</p>
         |<p class="large">2.3.5</p>
         |<p class="medium"><a class="icon-link glyph-link" href="doc-2.html"><i class="icofont-laika demo" title="Demo">&#xeeea;</i></a></p>
         |<p class="medium"><a class="button-link" href="http://somewhere.com/">Somewhere</a></p>
         |</div>
         |</div>
         |<p class="header">Some <em>Header</em></p>
         |$teaserHTML
         |<p>Some <em>markup</em> here.</p>
         |</body>""".stripMargin
    val imagePath          = Root / "home.png"
    val helium             = Helium.defaults
      .site.landingPage(
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
      .site.themeColors(
        primary = Color.hex("007c99"),
        secondary = Color.hex("931813"),
        primaryMedium = Color.hex("a7d4de"),
        primaryLight = Color.hex("ebf6f7"),
        text = Color.hex("5f5f5f"),
        background = Color.hex("ffffff"),
        bgGradient = (Color.hex("a7d4de"), Color.hex("ebf6f7")) // light bg from medium to light
      )
    val content            =
      """@:fragment(header)
        |Some *Header*
        |@:@
        |
        |Some *markup* here.""".stripMargin
    val inputsWithExtraDoc = inputs :+ ((Root / "landing-page.md", content))
    transformAndExtract(
      inputsWithExtraDoc,
      helium,
      s"""<html lang="${Locale.getDefault.toLanguageTag}">""",
      "</html>"
    )
      .assertEquals(expected)
  }

}
