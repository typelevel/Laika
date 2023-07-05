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
import laika.ast.{ Image, Path }
import laika.ast.Path.Root
import laika.format.{ HTML, Markdown }
import laika.helium.config._
import laika.io.api.TreeTransformer
import laika.io.helper.{ InputBuilder, ResultExtractor, StringOps }
import laika.io.implicits._
import laika.io.model.StringTreeOutput
import laika.rewrite.link.LinkConfig
import laika.rewrite.{ Version, Versions }
import laika.theme._
import munit.CatsEffectSuite

class HeliumHTMLNavSpec extends CatsEffectSuite with InputBuilder with ResultExtractor
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

  def transformer(
      theme: ThemeProvider,
      excludeFromValidation: Seq[Path] = Seq(Root)
  ): Resource[IO, TreeTransformer[IO]] = Transformer
    .from(Markdown)
    .to(HTML)
    .withConfigValue(LinkConfig(excludeFromValidation = excludeFromValidation))
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

  def configHeader(content: String): String =
    s"""{%
       |  $content
       |%}
       |""".stripMargin

  val flatInputs = Seq(
    Root / "doc-1.md" -> inputWithTitle(1),
    Root / "doc-2.md" -> inputWithTitle(2),
    Root / "doc-3.md" -> inputWithTitle(3),
    Root / "home.png" -> ""
  )

  def flatInputsWithConfig(config: String): Seq[(Path, String)] = Seq(
    Root / "doc-1.md" -> (configHeader(config) + inputWithTitle(1)),
    Root / "doc-2.md" -> inputWithTitle(2),
    Root / "doc-3.md" -> inputWithTitle(3),
    Root / "home.png" -> ""
  )

  val nestedInputs = Seq(
    Root / "dir-A" / "doc-1.md" -> inputWithTitle(1),
    Root / "dir-A" / "doc-2.md" -> inputWithTitle(2),
    Root / "dir-B" / "doc-3.md" -> inputWithTitle(3),
    Root / "dir-B" / "doc-4.md" -> inputWithTitle(4),
    Root / "home.png"           -> ""
  )

  val nestedAndRootInputs = Seq(
    Root / "doc-1.md"           -> inputWithTitle(1),
    Root / "doc-2.md"           -> inputWithTitle(2),
    Root / "dir-A" / "doc-3.md" -> inputWithTitle(3),
    Root / "dir-A" / "doc-4.md" -> inputWithTitle(4),
    Root / "dir-B" / "doc-5.md" -> inputWithTitle(5),
    Root / "dir-B" / "doc-6.md" -> inputWithTitle(6),
    Root / "home.png"           -> ""
  )

  val nestedPathUnderTest = Root / "dir-A" / "doc-1.html"

  private val defaultTopNavClasses = "light-default dark-default"

  def transformAndExtract(
      inputs: Seq[(Path, String)],
      helium: Helium,
      start: String,
      end: String,
      docPath: Path = Root / "doc-1.html",
      excludeFromValidation: Seq[Path] = Seq(Root)
  ): IO[String] =
    transformer(helium.build, excludeFromValidation = excludeFromValidation).use { t =>
      for {
        resultTree <- t.fromInput(build(inputs)).toOutput(StringTreeOutput).transform
        res        <- IO.fromEither(
          resultTree.extractTidiedSubstring(docPath, start, end)
            .toRight(new RuntimeException("Missing document under test"))
        )
      } yield res
    }

  def transformAndAssertNoPageNav(
      inputs: Seq[(Path, String)],
      helium: Helium,
      docPath: Path = Root / "doc-1.html"
  ): IO[Unit] = {
    val startElement = "<div id=\"container\">"
    val endElement   = """<main class="content">"""
    transformAndExtract(inputs, helium, startElement, endElement, docPath)
      .assertEquals("")
  }

  test("main navigation - one level") {
    val expected = """<div class="row">
                     |</div>
                     |<ul class="nav-list">
                     |<li class="level1 active nav-leaf"><a href="#">Doc 1</a></li>
                     |<li class="level1 nav-leaf"><a href="doc-2.html">Doc 2</a></li>
                     |<li class="level1 nav-leaf"><a href="doc-3.html">Doc 3</a></li>
                     |</ul>""".stripMargin
    transformAndExtract(
      flatInputs,
      Helium.defaults.site.landingPage(),
      "<nav id=\"sidebar\">",
      "</nav>"
    ).assertEquals(expected)
  }

  test("main navigation - two levels") {
    val expected = """<div class="row">
                     |</div>
                     |<ul class="nav-list">
                     |<li class="level1 nav-header">dir-A</li>
                     |<li class="level2 active nav-leaf"><a href="#">Doc 1</a></li>
                     |<li class="level2 nav-leaf"><a href="doc-2.html">Doc 2</a></li>
                     |<li class="level1 nav-header">dir-B</li>
                     |<li class="level2 nav-leaf"><a href="../dir-B/doc-3.html">Doc 3</a></li>
                     |<li class="level2 nav-leaf"><a href="../dir-B/doc-4.html">Doc 4</a></li>
                     |</ul>""".stripMargin
    transformAndExtract(
      nestedInputs,
      Helium.defaults.site.landingPage(),
      "<nav id=\"sidebar\">",
      "</nav>",
      nestedPathUnderTest
    ).assertEquals(expected)
  }

  test("main navigation - customized depth") {
    val expected = """<div class="row">
                     |</div>
                     |<ul class="nav-list">
                     |<li class="level1 active nav-leaf"><a href="#">Doc 1</a></li>
                     |<li class="level1 nav-leaf"><a href="doc-2.html">Doc 2</a></li>
                     |</ul>""".stripMargin
    val helium   = Helium.defaults.site.landingPage().site.mainNavigation(depth = 1)
    transformAndExtract(nestedAndRootInputs, helium, "<nav id=\"sidebar\">", "</nav>").assertEquals(
      expected
    )
  }

  test("main navigation - include page sections") {
    val expected = """<div class="row">
                     |</div>
                     |<ul class="nav-list">
                     |<li class="level1 active nav-node"><a href="#">Doc 1</a></li>
                     |<li class="level2 nav-leaf"><a href="#section-1">Section 1</a></li>
                     |<li class="level2 nav-leaf"><a href="#section-2">Section 2</a></li>
                     |<li class="level1 nav-node"><a href="doc-2.html">Doc 2</a></li>
                     |<li class="level2 nav-leaf"><a href="doc-2.html#section-1">Section 1</a></li>
                     |<li class="level2 nav-leaf"><a href="doc-2.html#section-2">Section 2</a></li>
                     |<li class="level1 nav-node"><a href="doc-3.html">Doc 3</a></li>
                     |<li class="level2 nav-leaf"><a href="doc-3.html#section-1">Section 1</a></li>
                     |<li class="level2 nav-leaf"><a href="doc-3.html#section-2">Section 2</a></li>
                     |</ul>""".stripMargin
    val helium = Helium.defaults.site.landingPage().site.mainNavigation(includePageSections = true)
    transformAndExtract(flatInputs, helium, "<nav id=\"sidebar\">", "</nav>").assertEquals(expected)
  }

  test("main navigation - append links") {
    val expected = """<div class="row">
                     |</div>
                     |<ul class="nav-list">
                     |<li class="level1 active nav-leaf"><a href="#">Doc 1</a></li>
                     |<li class="level1 nav-leaf"><a href="doc-2.html">Doc 2</a></li>
                     |<li class="level1 nav-leaf"><a href="doc-3.html">Doc 3</a></li>
                     |<li class="level1 nav-header">Additional Links</li>
                     |<li class="level2 active nav-leaf"><a href="#">Add Link 1</a></li>
                     |<li class="level2 nav-leaf"><a href="https://foo.com">Add Link 2</a></li>
                     |</ul>""".stripMargin
    val section  = ThemeNavigationSection(
      "Additional Links",
      TextLink.internal(Root / "doc-1.md", "Add Link 1"),
      TextLink.external("https://foo.com", "Add Link 2")
    )
    val helium = Helium.defaults.site.landingPage().site.mainNavigation(appendLinks = Seq(section))
    transformAndExtract(flatInputs, helium, "<nav id=\"sidebar\">", "</nav>").assertEquals(expected)
  }

  test("main navigation - prepend links") {
    val expected = """<div class="row">
                     |</div>
                     |<ul class="nav-list">
                     |<li class="level1 nav-header">Additional Links</li>
                     |<li class="level2 active nav-leaf"><a href="#">Add Link 1</a></li>
                     |<li class="level2 nav-leaf"><a href="https://foo.com">Add Link 2</a></li>
                     |<li class="level1 active nav-leaf"><a href="#">Doc 1</a></li>
                     |<li class="level1 nav-leaf"><a href="doc-2.html">Doc 2</a></li>
                     |<li class="level1 nav-leaf"><a href="doc-3.html">Doc 3</a></li>
                     |</ul>""".stripMargin
    val section  = ThemeNavigationSection(
      "Additional Links",
      TextLink.internal(Root / "doc-1.md", "Add Link 1"),
      TextLink.external("https://foo.com", "Add Link 2")
    )
    val helium = Helium.defaults.site.landingPage().site.mainNavigation(prependLinks = Seq(section))
    transformAndExtract(flatInputs, helium, "<nav id=\"sidebar\">", "</nav>").assertEquals(expected)
  }

  test("page navigation - two levels") {
    val expected =
      """<p class="header"><a href="#">Doc 1</a></p>
        |<ul class="nav-list">
        |<li class="level1 nav-node"><a href="#section-1">Section 1</a></li>
        |<li class="level2 nav-leaf"><a href="#section-1-1">Section 1.1</a></li>
        |<li class="level1 nav-node"><a href="#section-2">Section 2</a></li>
        |<li class="level2 nav-leaf"><a href="#section-2-1">Section 2.1</a></li>
        |</ul>
        |<p class="footer"></p>""".stripMargin
    transformAndExtract(
      flatInputs,
      Helium.defaults.site.landingPage(),
      "<nav id=\"page-nav\">",
      "</nav>"
    ).assertEquals(expected)
  }

  test("page navigation - one level only, configured globally") {
    val expected =
      """<p class="header"><a href="#">Doc 1</a></p>
        |<ul class="nav-list">
        |<li class="level1 nav-leaf"><a href="#section-1">Section 1</a></li>
        |<li class="level1 nav-leaf"><a href="#section-2">Section 2</a></li>
        |</ul>
        |<p class="footer"></p>""".stripMargin
    val helium   = Helium.defaults.site.landingPage().site.pageNavigation(depth = 1)
    transformAndExtract(flatInputs, helium, "<nav id=\"page-nav\">", "</nav>").assertEquals(
      expected
    )
  }

  test("page navigation - show on small screens, configured globally") {
    val expected =
      """<p class="header"><a href="#">Doc 1</a></p>
        |<ul class="nav-list">
        |<li class="level1 nav-node"><a href="#section-1">Section 1</a></li>
        |<li class="level2 nav-leaf"><a href="#section-1-1">Section 1.1</a></li>
        |<li class="level1 nav-node"><a href="#section-2">Section 2</a></li>
        |<li class="level2 nav-leaf"><a href="#section-2-1">Section 2.1</a></li>
        |</ul>
        |<p class="footer"></p>""".stripMargin
    val helium   = Helium.defaults.site.landingPage().site.pageNavigation(keepOnSmallScreens = true)
    transformAndExtract(
      flatInputs,
      helium,
      "<nav id=\"page-nav\" class=\"all-screens\">",
      "</nav>"
    ).assertEquals(
      expected
    )
  }

  test("page navigation - one level only, configured in configuration header in markup") {
    val expected =
      """<p class="header"><a href="#">Doc 1</a></p>
        |<ul class="nav-list">
        |<li class="level1 nav-leaf"><a href="#section-1">Section 1</a></li>
        |<li class="level1 nav-leaf"><a href="#section-2">Section 2</a></li>
        |</ul>
        |<p class="footer"></p>""".stripMargin
    val config   = "helium.site.pageNavigation.depth = 1"
    transformAndExtract(
      flatInputsWithConfig(config),
      Helium.defaults.site.landingPage(),
      "<nav id=\"page-nav\">",
      "</nav>"
    ).assertEquals(expected)
  }

  test("page navigation - show on small screens, configured in configuration header in markup") {
    val expected =
      """<p class="header"><a href="#">Doc 1</a></p>
        |<ul class="nav-list">
        |<li class="level1 nav-node"><a href="#section-1">Section 1</a></li>
        |<li class="level2 nav-leaf"><a href="#section-1-1">Section 1.1</a></li>
        |<li class="level1 nav-node"><a href="#section-2">Section 2</a></li>
        |<li class="level2 nav-leaf"><a href="#section-2-1">Section 2.1</a></li>
        |</ul>
        |<p class="footer"></p>""".stripMargin
    val config   = "helium.site.pageNavigation.keepOnSmallScreens = true"
    val helium   = Helium.defaults.site.landingPage()
    transformAndExtract(
      flatInputsWithConfig(config),
      helium,
      "<nav id=\"page-nav\" class=\"all-screens\">",
      "</nav>"
    ).assertEquals(
      expected
    )
  }

  test("page navigation - disabled globally") {
    val helium = Helium.defaults.site.landingPage().site.pageNavigation(enabled = false)
    transformAndAssertNoPageNav(flatInputs, helium)
  }

  test("page navigation - disabled in configuration header in markup") {
    val helium = Helium.defaults.site.landingPage()
    val config = "helium.site.pageNavigation.enabled = false"
    transformAndAssertNoPageNav(flatInputsWithConfig(config), helium)
  }

  test("page navigation - with footer link") {
    val expected =
      """<p class="header"><a href="#">Doc 1</a></p>
        |<ul class="nav-list">
        |<li class="level1 nav-node"><a href="#section-1">Section 1</a></li>
        |<li class="level2 nav-leaf"><a href="#section-1-1">Section 1.1</a></li>
        |<li class="level1 nav-node"><a href="#section-2">Section 2</a></li>
        |<li class="level2 nav-leaf"><a href="#section-2-1">Section 2.1</a></li>
        |</ul>
        |<p class="footer"><a href="https://github.com/my-project/doc-1.md"><i class="icofont-laika edit" title="Edit">&#xef10;</i>Source for this page</a></p>""".stripMargin
    val helium   = Helium.defaults.site.pageNavigation(sourceBaseURL =
      Some("https://github.com/my-project")
    ).site.landingPage()
    transformAndExtract(flatInputs, helium, "<nav id=\"page-nav\">", "</nav>").assertEquals(
      expected
    )
  }

  test("page navigation - disabled in table of content") {
    val helium = Helium.defaults
      .site.tableOfContent("Table of Content", 2)
      .site.landingPage()
    transformAndAssertNoPageNav(flatInputs, helium, Root / "table-of-content.html")
  }

  test("top navigation - defaults") {
    val expected =
      """<div class="row">
        |<a id="nav-icon">
        |<i class="icofont-laika navigationMenu" title="Navigation">&#xefa2;</i>
        |</a>
        |</div>
        |<a class="icon-link glyph-link" href="index.html"><i class="icofont-laika home" title="Home">&#xef47;</i></a>
        |<div class="row links">
        |</div>""".stripMargin
    transformAndExtract(
      flatInputs,
      Helium.defaults.site.landingPage(),
      s"""<header id="top-bar" class="$defaultTopNavClasses">""",
      "</header>"
    ).assertEquals(expected)
  }

  test("top navigation - highContrast flag set") {
    val expected =
      """<div class="row">
        |<a id="nav-icon">
        |<i class="icofont-laika navigationMenu" title="Navigation">&#xefa2;</i>
        |</a>
        |</div>
        |<a class="icon-link glyph-link" href="index.html"><i class="icofont-laika home" title="Home">&#xef47;</i></a>
        |<div class="row links">
        |</div>""".stripMargin
    transformAndExtract(
      flatInputs,
      Helium.defaults.site.landingPage().site.topNavigationBar(highContrast = true),
      "<header id=\"top-bar\" class=\"light-inverted dark-inverted\">",
      "</header>"
    ).assertEquals(expected)
  }

  test("top navigation - with custom links - references to other versions not validated") {
    val expected  =
      """<div class="row">
        |<a id="nav-icon">
        |<i class="icofont-laika navigationMenu" title="Navigation">&#xefa2;</i>
        |</a>
        |<div class="menu-container version-menu">
        |<a class="text-link menu-toggle" href="#">Choose Version</a>
        |<nav class="menu-content">
        |<ul class="nav-list">
        |</ul>
        |</nav>
        |</div>
        |</div>
        |<a class="image-link" href="index.html"><img src="home.png" alt="Homepage" title="Home"></a>
        |<div class="row links">
        |<a class="icon-link glyph-link" href="doc-2.html"><i class="icofont-laika demo" title="Demo">&#xeeea;</i></a>
        |<a class="icon-link glyph-link" href="0.43/doc-9.txt"><i class="icofont-laika demo" title="Demo">&#xeeea;</i></a>
        |<a class="button-link" href="http://somewhere.com/">Somewhere</a>
        |</div>""".stripMargin
    val imagePath = Root / "home.png"
    val helium    = Helium.defaults.site.landingPage()
      .site.versions(versions)
      .site.topNavigationBar(
        homeLink = ImageLink.internal(
          Root / "README",
          Image.internal(imagePath, alt = Some("Homepage"), title = Some("Home"))
        ),
        navLinks = Seq(
          IconLink.internal(Root / "doc-2.md", HeliumIcon.demo), // validated internal ref
          IconLink.internal(
            Root / "0.43" / "doc-9.txt",
            HeliumIcon.demo
          ), // not validated as referring to a different version
          ButtonLink.external("http://somewhere.com/", "Somewhere")
        )
      )
    transformAndExtract(
      flatInputs,
      helium,
      s"""<header id="top-bar" class="$defaultTopNavClasses">""",
      "</header>",
      excludeFromValidation = Nil
    ).assertEquals(expected)
  }

  test("top navigation - with menu") {
    val expected  =
      """<div class="row">
        |<a id="nav-icon">
        |<i class="icofont-laika navigationMenu" title="Navigation">&#xefa2;</i>
        |</a>
        |</div>
        |<a class="image-link" href="index.html"><img src="home.png" alt="Homepage" title="Home"></a>
        |<div class="row links">
        |<div class="menu-container">
        |<a class="text-link menu-toggle" href="#">Menu Label</a>
        |<nav class="menu-content">
        |<ul class="nav-list">
        |<li class="level1 nav-leaf"><a href="doc-2.html">Link 1</a></li>
        |<li class="level1 nav-leaf"><a href="doc-3.html">Link 2</a></li>
        |</ul>
        |</nav>
        |</div>
        |</div>""".stripMargin
    val imagePath = Root / "home.png"
    val helium    = Helium.defaults.site.landingPage()
      .site.topNavigationBar(
        homeLink = ImageLink.internal(
          Root / "README",
          Image.internal(imagePath, alt = Some("Homepage"), title = Some("Home"))
        ),
        navLinks = Seq(
          Menu.create(
            "Menu Label",
            TextLink.internal(Root / "doc-2.md", "Link 1"),
            TextLink.internal(Root / "doc-3.md", "Link 2")
          )
        )
      )
    transformAndExtract(
      flatInputs,
      helium,
      s"""<header id="top-bar" class="$defaultTopNavClasses">""",
      "</header>"
    ).assertEquals(
      expected
    )
  }

  test("top navigation - with menu, but after calling resetDefaults") {
    val expected  =
      """<div class="row">
        |<a id="nav-icon">
        |<i class="icofont-laika navigationMenu" title="Navigation">&#xefa2;</i>
        |</a>
        |</div>
        |<a class="image-link" href="index.html"><img src="home.png" alt="Homepage" title="Home"></a>
        |<div class="row links">
        |<div class="menu-container">
        |<a class="text-link menu-toggle" href="#">Menu Label</a>
        |<nav class="menu-content">
        |<ul class="nav-list">
        |<li class="level1 nav-leaf"><a href="doc-2.html">Link 1</a></li>
        |<li class="level1 nav-leaf"><a href="doc-3.html">Link 2</a></li>
        |</ul>
        |</nav>
        |</div>
        |</div>""".stripMargin
    val imagePath = Root / "home.png"
    val helium    = Helium.defaults.site.landingPage()
      .site.topNavigationBar(
        navLinks = Seq(
          ButtonLink.internal(Root / "doc-1.md", "Removed")
        )
      )
      .site.resetDefaults(topNavigation = true)
      .site.topNavigationBar(
        homeLink = ImageLink.internal(
          Root / "README",
          Image.internal(imagePath, alt = Some("Homepage"), title = Some("Home"))
        ),
        navLinks = Seq(
          Menu.create(
            "Menu Label",
            TextLink.internal(Root / "doc-2.md", "Link 1"),
            TextLink.internal(Root / "doc-3.md", "Link 2")
          )
        )
      )
    transformAndExtract(
      flatInputs,
      helium,
      s"""<header id="top-bar" class="$defaultTopNavClasses">""",
      "</header>"
    ).assertEquals(
      expected
    )
  }

  test("top navigation - with version dropdown on a versioned page") {
    val versionMenu = VersionMenu.create("Version:", "Choose Version")
    val helium      = Helium.defaults
      .site.versions(versions)
      .site.topNavigationBar(versionMenu = versionMenu)
      .site.landingPage()
    val expected    =
      """<div class="row">
        |<a id="nav-icon">
        |<i class="icofont-laika navigationMenu" title="Navigation">&#xefa2;</i>
        |</a>
        |<div class="menu-container version-menu">
        |<a class="text-link menu-toggle" href="#">Version: 0.42.x</a>
        |<nav class="menu-content">
        |<ul class="nav-list">
        |</ul>
        |</nav>
        |</div>
        |</div>
        |<a class="icon-link glyph-link" href="../"><i class="icofont-laika home" title="Home">&#xef47;</i></a>
        |<div class="row links">
        |</div>""".stripMargin
    val config      = Root / "directory.conf" -> "laika.versioned = true"
    transformAndExtract(
      flatInputs :+ config,
      helium,
      s"""<header id="top-bar" class="$defaultTopNavClasses">""",
      "</header>",
      Root / "0.42" / "doc-1.html"
    ).assertEquals(expected)
  }

  test("top navigation - with version dropdown on an unversioned page") {
    val versionMenu = VersionMenu.create(
      "Version:",
      "Choose Version",
      Seq(TextLink.internal(Root / "doc-2.md", "Extra-Link"))
    )
    val helium      = Helium.defaults
      .site.versions(versions)
      .site.topNavigationBar(versionMenu = versionMenu)
      .site.landingPage()
    val expected    =
      """<div class="row">
        |<a id="nav-icon">
        |<i class="icofont-laika navigationMenu" title="Navigation">&#xefa2;</i>
        |</a>
        |<div class="menu-container version-menu">
        |<a class="text-link menu-toggle" href="#">Choose Version</a>
        |<nav class="menu-content">
        |<ul class="nav-list">
        |<li class="level1 nav-leaf"><a href="doc-2.html">Extra-Link</a></li>
        |</ul>
        |</nav>
        |</div>
        |</div>
        |<a class="icon-link glyph-link" href="index.html"><i class="icofont-laika home" title="Home">&#xef47;</i></a>
        |<div class="row links">
        |</div>""".stripMargin
    transformAndExtract(
      flatInputs,
      helium,
      s"""<header id="top-bar" class="$defaultTopNavClasses">""",
      "</header>"
    ).assertEquals(
      expected
    )
  }

}
