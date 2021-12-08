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
import laika.api.builder.TransformerBuilder
import laika.ast.Path
import laika.ast.Path.Root
import laika.format.{HTML, Markdown}
import laika.io.api.TreeTransformer
import laika.io.helper.{InputBuilder, ResultExtractor, StringOps}
import laika.io.implicits._
import laika.io.model.StringTreeOutput
import laika.render.HTMLFormatter
import laika.rewrite.link.LinkConfig
import laika.rewrite.nav.{ChoiceConfig, CoverImage, SelectionConfig, Selections}
import laika.theme._
import munit.CatsEffectSuite

class HeliumDownloadPageSpec extends CatsEffectSuite with InputBuilder with ResultExtractor with StringOps {

  type ConfigureTransformer = TransformerBuilder[HTMLFormatter] => TransformerBuilder[HTMLFormatter]
  
  def transformer (theme: ThemeProvider, configure: ConfigureTransformer): Resource[IO, TreeTransformer[IO]] = {
    val builder = Transformer.from(Markdown).to(HTML)
      .withConfigValue(LinkConfig(excludeFromValidation = Seq(Root)))
    configure(builder)  
      .parallel[IO]
      .withTheme(theme)
      .build
  }
  
  val singleDoc = Seq(
    Root / "doc.md" -> "text"
  )
  
  def transformAndExtract(inputs: Seq[(Path, String)], helium: Helium, start: String, end: String, 
                          configure: ConfigureTransformer = identity): IO[String] = 
    transformer(helium.build, configure).use { t =>
      for {
        resultTree <- t.fromInput(build(inputs)).toOutput(StringTreeOutput).transform
        res        <- IO.fromEither(resultTree.extractTidiedSubstring(Root / "downloads.html", start, end)
          .toRight(new RuntimeException("Missing document under test")))
      } yield res
    }
    
  test("no download page configured") {
    transformAndExtract(singleDoc, Helium.defaults, "", "")
      .interceptMessage[RuntimeException]("Missing document under test")
  }
  
  private val heliumWithDownloadPage = Helium.defaults.site.downloadPage(
    title = "Downloads",
    description = Some("EPUB & PDF"),
    downloadPath = Root / "documents",
  ).site.landingPage()
  
  test("download page included - full HTML") {
    val expected = """<head>
                     |<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
                     |<meta charset="utf-8">
                     |<meta name="viewport" content="width=device-width, initial-scale=1.0">
                     |<meta name="generator" content="Laika 0.18.1 + Helium Theme" />
                     |<title>Downloads</title>
                     |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
                     |<link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/tonsky/FiraCode@1.207/distr/fira_code.css">
                     |<link rel="stylesheet" type="text/css" href="helium/icofont.min.css" />
                     |<link rel="stylesheet" type="text/css" href="helium/laika-helium.css" />
                     |<script src="helium/laika-helium.js"></script>
                     |<script> /* for avoiding page load transitions */ </script>
                     |</head>
                     |<body>
                     |<header id="top-bar">
                     |<div class="row">
                     |<a id="nav-icon">
                     |<i class="icofont-laika" title="Navigation">&#xefa2;</i>
                     |</a>
                     |</div>
                     |<a class="icon-link" href="index.html"><i class="icofont-laika" title="Home">&#xef47;</i></a>
                     |<span class="row links"></span>
                     |</header>
                     |<nav id="sidebar">
                     |<div class="row">
                     |</div>
                     |<ul class="nav-list">
                     |<li class="level1 active"><a href="#">Downloads</a></li>
                     |<li class="level1"><a href="doc.html">doc.md</a></li>
                     |</ul>
                     |</nav>
                     |<div id="container">
                     |<nav id="page-nav">
                     |<p class="header"><a href="#">Downloads</a></p>
                     |<ul class="nav-list">
                     |</ul>
                     |<p class="footer"></p>
                     |</nav>
                     |<main class="content">
                     |<h1 class="title">Downloads</h1>
                     |<p>EPUB &amp; PDF</p>
                     |<div class="downloads">
                     |<div>
                     |<p class="title">EPUB</p>
                     |<p><a href="downloads/download.epub">Download</a></p>
                     |</div>
                     |<div>
                     |<p class="title">PDF</p>
                     |<p><a href="downloads/download.pdf">Download</a></p>
                     |</div>
                     |</div>
                     |</main>
                     |</div>
                     |</body>""".stripMargin
    transformAndExtract(singleDoc, heliumWithDownloadPage, s"""<html lang="${Locale.getDefault.toLanguageTag}">""", "</html>")
      .assertEquals(expected)
  }

  test("configure artifact base name") {
    val inputs = Seq(
      Root / "doc.md" -> "text",
      Root / "directory.conf" -> "laika.artifactBaseName = documentation" // TODO - might be added to Helium Config API
    )
    val expected = """<h1 class="title">Downloads</h1>
                     |<p>EPUB &amp; PDF</p>
                     |<div class="downloads">
                     |<div>
                     |<p class="title">EPUB</p>
                     |<p><a href="downloads/documentation.epub">Download</a></p>
                     |</div>
                     |<div>
                     |<p class="title">PDF</p>
                     |<p><a href="downloads/documentation.pdf">Download</a></p>
                     |</div>
                     |</div>""".stripMargin
    transformAndExtract(inputs, heliumWithDownloadPage, "<main class=\"content\">", "</main>")
      .assertEquals(expected)
  }

  test("include only EPUB") {
    val heliumWithEPUBDownloads = Helium.defaults.site.downloadPage(
      title = "Downloads",
      description = Some("EPUB & PDF"),
      downloadPath = Root / "documents",
      includePDF = false,
    )
    val expected = """<h1 class="title">Downloads</h1>
                     |<p>EPUB &amp; PDF</p>
                     |<div class="downloads">
                     |<div>
                     |<p class="title">EPUB</p>
                     |<p><a href="downloads/download.epub">Download</a></p>
                     |</div>
                     |</div>""".stripMargin
    transformAndExtract(singleDoc, heliumWithEPUBDownloads, "<main class=\"content\">", "</main>")
      .assertEquals(expected)
  }

  test("include cover image") {
    val heliumWithDownloadsAndCovers = Helium.defaults
      .site.downloadPage(
        title = "Downloads",
        description = Some("EPUB & PDF"),
        downloadPath = Root / "documents"
      )
      .pdf.coverImages(CoverImage(Root / "cover.png"))
      .epub.coverImages(CoverImage(Root / "cover.png"))
    val expected = """<h1 class="title">Downloads</h1>
                     |<p>EPUB &amp; PDF</p>
                     |<div class="downloads">
                     |<div>
                     |<p class="title">EPUB</p>
                     |<p><img src="cover.png" alt="EPUB"></p>
                     |<p><a href="downloads/download.epub">Download</a></p>
                     |</div>
                     |<div>
                     |<p class="title">PDF</p>
                     |<p><img src="cover.png" alt="PDF"></p>
                     |<p><a href="downloads/download.pdf">Download</a></p>
                     |</div>
                     |</div>""".stripMargin
    transformAndExtract(singleDoc, heliumWithDownloadsAndCovers, "<main class=\"content\">", "</main>")
      .assertEquals(expected)
  }

  test("include cover image with configured selections (choices)") {
    val heliumWithDownloadsAndCovers = Helium.defaults
      .site.downloadPage(
        title = "Downloads",
        description = Some("EPUB & PDF"),
        downloadPath = Root / "documents"
      )
      .pdf.coverImages(CoverImage(Root / "cover-sbt.png", Some("sbt")), CoverImage(Root / "cover-library.png", Some("library")))
      .epub.coverImages(CoverImage(Root / "cover-sbt.png", Some("sbt")), CoverImage(Root / "cover-library.png", Some("library")))
    val expected = """<h1 class="title">Downloads</h1>
                     |<p>EPUB &amp; PDF</p>
                     |<div class="downloads">
                     |<div>
                     |<p class="title">sbt Plugin (EPUB)</p>
                     |<p><img src="cover-sbt.png" alt="sbt Plugin (EPUB)"></p>
                     |<p><a href="downloads/download-sbt.epub">Download</a></p>
                     |</div>
                     |<div>
                     |<p class="title">sbt Plugin (PDF)</p>
                     |<p><img src="cover-sbt.png" alt="sbt Plugin (PDF)"></p>
                     |<p><a href="downloads/download-sbt.pdf">Download</a></p>
                     |</div>
                     |</div>
                     |<div class="downloads">
                     |<div>
                     |<p class="title">Library API (EPUB)</p>
                     |<p><img src="cover-library.png" alt="Library API (EPUB)"></p>
                     |<p><a href="downloads/download-library.epub">Download</a></p>
                     |</div>
                     |<div>
                     |<p class="title">Library API (PDF)</p>
                     |<p><img src="cover-library.png" alt="Library API (PDF)"></p>
                     |<p><a href="downloads/download-library.pdf">Download</a></p>
                     |</div>
                     |</div>""".stripMargin
    val configure: ConfigureTransformer = _.withConfigValue(Selections(
      SelectionConfig("config",
        ChoiceConfig("sbt", "sbt Plugin"),
        ChoiceConfig("library", "Library API")
      ).withSeparateEbooks
    ))
    transformAndExtract(singleDoc, heliumWithDownloadsAndCovers, "<main class=\"content\">", "</main>", configure)
      .assertEquals(expected)
  }

}
