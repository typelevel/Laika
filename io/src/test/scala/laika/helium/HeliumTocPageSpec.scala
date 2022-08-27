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
import laika.ast.Path
import laika.ast.Path.Root
import laika.format.{HTML, Markdown}
import laika.io.api.TreeTransformer
import laika.io.helper.{InputBuilder, ResultExtractor, StringOps}
import laika.io.implicits._
import laika.io.model.StringTreeOutput
import laika.theme._
import munit.CatsEffectSuite

class HeliumTocPageSpec extends CatsEffectSuite with InputBuilder with ResultExtractor with StringOps {

  def transformer (theme: ThemeProvider): Resource[IO, TreeTransformer[IO]] = Transformer
    .from(Markdown)
    .to(HTML)
    .parallel[IO]
    .withTheme(theme)
    .build
  
  val twoDocs = Seq(
    Root / "doc-1.md" -> "text",
    Root / "doc-2.md" -> "text",
    Root / "dir-1" / "doc-3.md" -> "text",
    Root / "dir-1" / "sub-dir" / "doc-3.md" -> "text"
  )
  
  def transformAndExtract(inputs: Seq[(Path, String)], helium: Helium, start: String, end: String): IO[String] = transformer(helium.build).use { t =>
    for {
      resultTree <- t.fromInput(build(inputs)).toOutput(StringTreeOutput).transform
      res        <- IO.fromEither(resultTree.extractTidiedSubstring(Root / "table-of-content.html", start, end)
        .toRight(new RuntimeException("Missing document under test")))
    } yield res
  }
    
  test("no table of content page configured") {
    transformAndExtract(twoDocs, Helium.defaults.site.landingPage(), "", "")
      .interceptMessage[RuntimeException]("Missing document under test")
  }
  
  test("table of content included") {
    val expected = """<head>
                     |<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
                     |<meta charset="utf-8">
                     |<meta name="viewport" content="width=device-width, initial-scale=1.0">
                     |<meta name="generator" content="Laika 0.18.1 + Helium Theme" />
                     |<title>Contents</title>
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
                     |<div class="row links">
                     |</div>
                     |</header>
                     |<nav id="sidebar">
                     |<div class="row">
                     |</div>
                     |<ul class="nav-list">
                     |<li class="level1 active nav-leaf-entry"><a href="#">Contents</a></li>
                     |<li class="level1 nav-leaf-entry"><a href="doc-1.html">doc-1.md</a></li>
                     |<li class="level1 nav-leaf-entry"><a href="doc-2.html">doc-2.md</a></li>
                     |<li class="level1 nav-section-header">dir-1</li>
                     |<li class="level2 nav-leaf-entry"><a href="dir-1/doc-3.html">doc-3.md</a></li>
                     |</ul>
                     |</nav>
                     |<div id="container">
                     |<nav id="page-nav">
                     |<p class="header"><a href="#">Contents</a></p>
                     |<ul class="nav-list">
                     |</ul>
                     |<p class="footer"></p>
                     |</nav>
                     |<main class="content">
                     |<h1 class="title">Contents</h1>
                     |<ul class="toc nav-list">
                     |<li class="level1 toc nav-leaf-entry"><a href="doc-1.html">doc-1.md</a></li>
                     |<li class="level1 toc nav-leaf-entry"><a href="doc-2.html">doc-2.md</a></li>
                     |<li class="level1 toc nav-section-header">dir-1</li>
                     |<li class="level2 toc nav-leaf-entry"><a href="dir-1/doc-3.html">doc-3.md</a></li>
                     |</ul>
                     |</main>
                     |</div>
                     |</body>""".stripMargin
    val helium = Helium.defaults.site.landingPage().site.tableOfContent("Contents", 2)
    transformAndExtract(twoDocs, helium, s"""<html lang="${Locale.getDefault.toLanguageTag}">""", "</html>").assertEquals(expected)
  }

}
