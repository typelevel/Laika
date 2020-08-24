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

import cats.effect.{IO, Resource}
import laika.api.Transformer
import laika.ast.{Image, InternalTarget, Path}
import laika.ast.Path.Root
import laika.format.{HTML, Markdown}
import laika.helium.config.{ButtonLink, HeliumIcon, IconLink, ThemeTarget}
import laika.io.helper.{InputBuilder, ResultExtractor, StringOps}
import laika.io.implicits._
import laika.io.model.StringTreeOutput
import laika.io.{FileIO, IOFunSuite}
import laika.theme._

class HeliumHTMLNavSpec extends IOFunSuite with InputBuilder with ResultExtractor with StringOps {

  def transformer (theme: Resource[IO, Theme[IO]]) = Transformer
    .from(Markdown)
    .to(HTML)
    .io(FileIO.blocker)
    .parallel[IO]
    .withTheme(theme)
    .build
  
  def inputWithTitle(num: Int): String =
    s"""
      |Doc $num
      |=====
      |
      |## Section 1
      |
      |Text
      |
      |### Section 1.1
      |
      |Text
      |
      |## Section 2
      |
      |Text
      |
      |### Section 2.1
    """.stripMargin
  
  val threeDocs = Seq(
    Root / "doc-1.md" -> inputWithTitle(1),
    Root / "doc-2.md" -> inputWithTitle(2),
    Root / "doc-3.md" -> inputWithTitle(3),
  )
  
  def transformAndExtract(inputs: Seq[(Path, String)], helium: Helium, start: String, end: String): IO[String] = transformer(helium.build).use { t =>
    for {
      resultTree <- t.fromInput(build(inputs)).toOutput(StringTreeOutput).transform
      res        <- IO.fromEither(resultTree.extractTidiedSubstring(Root / "doc-1.html", start, end)
        .toRight(new RuntimeException("Missing document under test")))
    } yield res
  }
    
  test("main navigation - one level") {
    val expected = """<ul class="nav-list">
                     |<li class="level1 active"><a href="#">Doc 1</a></li>
                     |<li class="level1"><a href="doc-2.html">Doc 2</a></li>
                     |<li class="level1"><a href="doc-3.html">Doc 3</a></li>
                     |</ul>""".stripMargin
    transformAndExtract(threeDocs, Helium.defaults, "<nav id=\"sidebar\">", "</nav>").assertEquals(expected)
  }

  test("page navigation - two levels") {
    val expected =
      """<p class="header"><a href="#">Doc 1</a></p>
        |<ul class="nav-list">
        |<li class="level1"><a href="#section-1">Section 1</a></li>
        |<li class="level2"><a href="#section-1-1">Section 1.1</a></li>
        |<li class="level1"><a href="#section-2">Section 2</a></li>
        |<li class="level2"><a href="#section-2-1">Section 2.1</a></li>
        |</ul>
        |<p class="footer"></p>""".stripMargin
    transformAndExtract(threeDocs, Helium.defaults, "<nav id=\"page-nav\">", "</nav>").assertEquals(expected)
  }

  test("page navigation - with footer link") {
    val expected =
      """<p class="header"><a href="#">Doc 1</a></p>
        |<ul class="nav-list">
        |<li class="level1"><a href="#section-1">Section 1</a></li>
        |<li class="level2"><a href="#section-1-1">Section 1.1</a></li>
        |<li class="level1"><a href="#section-2">Section 2</a></li>
        |<li class="level2"><a href="#section-2-1">Section 2.1</a></li>
        |</ul>
        |<p class="footer"><a href="https://github.com/my-project/doc-1.md">Source for this page</a></p>""".stripMargin
    val helium = Helium.defaults.site.markupEditLinks("Source for this page", "https://github.com/my-project")
    transformAndExtract(threeDocs, helium, "<nav id=\"page-nav\">", "</nav>").assertEquals(expected)
  }

  // TODO - should have home button in defaults
  test("top navigation - defaults") {
    val expected =
      """<a id="nav-icon">
        |<i class="icofont-navigation-menu icofont-xlg"></i>
        |</a>
        |<span class="row"></span>""".stripMargin
    transformAndExtract(threeDocs, Helium.defaults, "<header id=\"top-bar\">", "</header>").assertEquals(expected)
  }

  // TODO - logo should be link to homepage
  test("top navigation - with custom links") {
    val expected =
      """<a id="nav-icon">
        |<i class="icofont-navigation-menu icofont-xlg"></i>
        |</a>
        |<img src="home.png" alt="Homepage" title="Home">
        |<span class="row"><a href="doc-2.html"><i class="icofont-laika">&#xeeea;</i></a><a class="button" href="http://somewhere.com/">Somewhere</a></span>""".stripMargin
    val imagePath = Root / "home.png"
    val helium = Helium.defaults.site
      .topNavigationBar(
        logo = Some(Image(InternalTarget(imagePath), alt = Some("Homepage"), title = Some("Home"))), 
        links = Seq(
          IconLink(ThemeTarget.internal(Root / "doc-2.md"), HeliumIcon.demo),
          ButtonLink(ThemeTarget.external("http://somewhere.com/"), "Somewhere")
        ))
    transformAndExtract(threeDocs, helium, "<header id=\"top-bar\">", "</header>").assertEquals(expected)
  }
  
}
