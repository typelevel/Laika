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

import cats.effect.{ Async, IO, Resource }
import laika.api.{ MarkupParser, Renderer, Transformer }
import laika.ast.Path
import laika.ast.Path.Root
import laika.config.LinkValidation
import laika.format.{ HTML, Markdown }
import laika.helium.config.Favicon
import laika.io.api.{ TreeParser, TreeRenderer, TreeTransformer }
import laika.io.helper.{ InputBuilder, ResultExtractor, StringOps, TestThemeBuilder }
import laika.io.syntax.*
import laika.io.model.InputTree
import laika.theme.ThemeProvider
import laika.theme.config.{
  CrossOrigin,
  Font,
  FontDefinition,
  FontStyle,
  FontWeight,
  ScriptAttributes,
  StyleAttributes
}
import munit.CatsEffectSuite

class HeliumHTMLHeadSpec extends CatsEffectSuite with InputBuilder with ResultExtractor
    with TestVersions
    with StringOps {

  val parser: Resource[IO, TreeParser[IO]] = MarkupParser
    .of(Markdown)
    .using(Markdown.GitHubFlavor)
    .parallel[IO]
    .build

  val renderer: Resource[IO, TreeRenderer[IO]] = {
    val builder = Renderer.of(HTML)
    builder.withConfig(
      builder.config.withBundlesFor(Markdown)
    ) // TODO - there should be dedicated API for this scenario
      .parallel[IO]
      .build
  }

  val parserAndRenderer: Resource[IO, (TreeParser[IO], TreeRenderer[IO])] = for {
    p <- parser
    r <- renderer
  } yield (p, r)

  def transformer(
      theme: ThemeProvider,
      validation: LinkValidation = LinkValidation.Local
  ): Resource[IO, TreeTransformer[IO]] = Transformer
    .from(Markdown)
    .to(HTML)
    .using(Markdown.GitHubFlavor)
    .withConfigValue(validation)
    .parallel[IO]
    .withTheme(theme)
    .build

  val singleDoc = Seq(
    Root / "name.md" -> "text"
  )

  val singleDocPlusHome = Seq(
    Root / "README.md" -> "text",
    Root / "name.md"   -> "text"
  )

  val docWithCanonicalLink = Seq(
    Root / "name.md" ->
      """{%
        |  laika.metadata.canonicalLink = "http://very.canonical/"
        |%}
        |
        |content""".stripMargin
  )

  val singleVersionedDoc = Seq(
    Root / "name.md"        -> "text",
    Root / "directory.conf" -> "laika.versioned = true"
  )

  val meta = s"""<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
                |<meta charset="utf-8">
                |<meta name="viewport" content="width=device-width, initial-scale=1.0">
                |<meta name="generator" content="Typelevel Laika + Helium Theme" />""".stripMargin

  val defaultResult =
    meta ++ """
              |<title></title>
              |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
              |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Fira+Mono:500">
              |<link rel="stylesheet" type="text/css" href="helium/site/icofont.min.css" />
              |<link rel="stylesheet" type="text/css" href="helium/site/laika-helium.css" />
              |<script src="helium/site/laika-helium.js"></script>
              |<script> /* for avoiding page load transitions */ </script>""".stripMargin

  val heliumBase = Helium.defaults.site.landingPage()

  private val testFonts = Seq(
    FontDefinition(
      Font.withWebCSS("http://fonts.com/font-1.css"),
      "Font-1",
      FontWeight.Normal,
      FontStyle.Normal
    ),
    FontDefinition(
      Font.withWebCSS("http://fonts.com/font-2.css"),
      "Font-2",
      FontWeight.Normal,
      FontStyle.Normal
    )
  )

  def transformAndExtractHead(inputs: Seq[(Path, String)]): IO[String] =
    transformAndExtractHead(inputs, Helium.defaults)

  def transformAndExtractHead(
      inputs: Seq[(Path, String)],
      helium: Helium,
      underTest: Path = Root / "name.html",
      validation: LinkValidation = LinkValidation.Local
  ): IO[String] = transformer(helium.build, validation).use { t =>
    for {
      resultTree <- t.fromInput(build(inputs)).toMemory.transform
      res        <- IO.fromEither(
        resultTree.extractTidiedTagContent(underTest, "head")
          .toRight(new RuntimeException("Missing document under test"))
      )
    } yield res
  }

  def transformAndExtract(
      inputs: Seq[(Path, String)],
      helium: Helium,
      start: String,
      end: String
  ): IO[String] = transformer(helium.build).use { t =>
    for {
      resultTree <- t.fromInput(build(inputs)).toMemory.transform
      res        <- IO.fromEither(
        resultTree.extractTidiedSubstring(Root / "name.html", start, end)
          .toRight(new RuntimeException("Missing document under test"))
      )
    } yield res
  }

  test("Helium defaults via separate parser and renderer") {
    parserAndRenderer.use { case (p, r) =>
      p.fromInput(build(singleDocPlusHome)).parse.flatMap { tree =>
        r.from(tree.root).toMemory.render
      }
    }
      .map(_.extractTidiedSubstring(Root / "name.html", "<head>", "</head>"))
      .assertEquals(Some(defaultResult))
  }

  test("Helium defaults via transformer") {
    transformAndExtractHead(singleDocPlusHome).assertEquals(defaultResult)
  }

  test("exclude CSS and JS from API directory") {
    val inputs = Seq(
      Root / "name.md"         -> "text",
      Root / "api" / "foo.js"  -> "",
      Root / "api" / "foo.css" -> ""
    )
    transformAndExtractHead(inputs, heliumBase).assertEquals(defaultResult)
  }

  test("internal CSS and JS resources, some using conditions") {
    val inputs   = Seq(
      Root / "name.md"            -> "text",
      Root / "web-1" / "foo-1.js" -> "",
      Root / "web-1" / "foo-2.js" -> "",
      Root / "web-1" / "foo.css"  -> "",
      Root / "web-2" / "bar.js"   -> "",
      Root / "web-2" / "bar.css"  -> ""
    )
    val helium   = heliumBase
      .site.internalJS(Root / "web-1", condition = _.path.name == "name.md")
      .site.internalJS(Root / "web-2", condition = _.path.name == "none.md")
      .site.internalCSS(Root / "web-1", condition = _.path.name == "name.md")
      .site.internalCSS(Root / "web-2", condition = _.path.name == "none.md")
    val expected =
      meta ++
        """
          |<title></title>
          |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
          |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Fira+Mono:500">
          |<link rel="stylesheet" type="text/css" href="helium/site/icofont.min.css" />
          |<link rel="stylesheet" type="text/css" href="helium/site/laika-helium.css" />
          |<link rel="stylesheet" type="text/css" href="web-1/foo.css" />
          |<script src="helium/site/laika-helium.js"></script>
          |<script src="web-1/foo-1.js"></script>
          |<script src="web-1/foo-2.js"></script>
          |<script> /* for avoiding page load transitions */ </script>""".stripMargin
    transformAndExtractHead(inputs, helium).assertEquals(expected)
  }

  test("internal CSS and JS resources, including a file from a theme extension") {
    val inputs                               = Seq(
      Root / "name.md"         -> "text",
      Root / "web" / "foo.js"  -> "",
      Root / "web" / "foo.css" -> ""
    )
    val themeInputs: TestThemeBuilder.Inputs = new TestThemeBuilder.Inputs {
      def build[G[_]: Async] = InputTree[G].addString("", Root / "theme" / "bar.css")
    }

    val themeExt = TestThemeBuilder.forInputs(themeInputs)
    val helium   = heliumBase
      .site.internalCSS(Root / "theme")
      .site.internalJS(Root / "web")
      .site.internalCSS(Root / "web")
    val expected =
      meta ++
        """
          |<title></title>
          |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
          |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Fira+Mono:500">
          |<link rel="stylesheet" type="text/css" href="helium/site/icofont.min.css" />
          |<link rel="stylesheet" type="text/css" href="helium/site/laika-helium.css" />
          |<link rel="stylesheet" type="text/css" href="theme/bar.css" />
          |<link rel="stylesheet" type="text/css" href="web/foo.css" />
          |<script src="helium/site/laika-helium.js"></script>
          |<script src="web/foo.js"></script>
          |<script> /* for avoiding page load transitions */ </script>""".stripMargin
    transformAndExtractHead(inputs, helium.extendWith(themeExt)).assertEquals(expected)
  }

  test("external CSS and JS resources, using attribute properties") {
    val inputs           = Seq(
      Root / "name.md" -> "text"
    )
    val scriptAttributes =
      ScriptAttributes.defaults.defer.withIntegrity("xyz").withCrossOrigin(CrossOrigin.Anonymous)
    val styleAttributes  =
      StyleAttributes.defaults.withIntegrity("xyz").withCrossOrigin(CrossOrigin.Anonymous)
    val helium           = heliumBase
      .site.externalJS("https://foo.com/script.js", scriptAttributes)
      .site.externalCSS("https://foo.com/styles.css", styleAttributes)
    val expected         =
      meta ++
        """
          |<title></title>
          |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
          |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Fira+Mono:500">
          |<link rel="stylesheet" type="text/css" href="https://foo.com/styles.css" integrity="xyz" crossorigin="anonymous" />
          |<link rel="stylesheet" type="text/css" href="helium/site/icofont.min.css" />
          |<link rel="stylesheet" type="text/css" href="helium/site/laika-helium.css" />
          |<script src="https://foo.com/script.js" defer integrity="xyz" crossorigin="anonymous"></script>
          |<script src="helium/site/laika-helium.js"></script>
          |<script> /* for avoiding page load transitions */ </script>""".stripMargin
    transformAndExtractHead(inputs, helium).assertEquals(expected)
  }

  test("inline styles and script") {
    val inputs   = Seq(
      Root / "name.md" -> "text"
    )
    val script   =
      """import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.esm.min.mjs';
        |mermaid.initialize({ startOnLoad: true });""".stripMargin
    val styles   =
      """h1 {
        |  color: #111111;
        |}""".stripMargin
    val helium   = heliumBase
      .site.inlineJS(script, isModule = true)
      .site.inlineCSS(styles)
    val expected =
      meta ++
        s"""
           |<title></title>
           |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
           |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Fira+Mono:500">
           |<link rel="stylesheet" type="text/css" href="helium/site/icofont.min.css" />
           |<link rel="stylesheet" type="text/css" href="helium/site/laika-helium.css" />
           |<style>
           |h1 {
           |color: #111111;
           |}
           |</style>
           |<script src="helium/site/laika-helium.js"></script>
           |<script type="module">
           |$script
           |</script>
           |<script> /* for avoiding page load transitions */ </script>""".stripMargin
    transformAndExtractHead(inputs, helium).assertEquals(expected)
  }

  test("metadata (authors, description)") {
    val helium   = heliumBase.all.metadata(
      authors = Seq("Maria Green", "Elena Blue"),
      description = Some("Some description")
    )
    val expected =
      meta ++ """
                |<title></title>
                |<meta name="author" content="Maria Green"/>
                |<meta name="author" content="Elena Blue"/>
                |<meta name="description" content="Some description"/>
                |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
                |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Fira+Mono:500">
                |<link rel="stylesheet" type="text/css" href="helium/site/icofont.min.css" />
                |<link rel="stylesheet" type="text/css" href="helium/site/laika-helium.css" />
                |<script src="helium/site/laika-helium.js"></script>
                |<script> /* for avoiding page load transitions */ </script>""".stripMargin
    transformAndExtractHead(singleDoc, helium).assertEquals(expected)
  }

  test("metadata (language)") {
    val helium = heliumBase.all.metadata(
      language = Some("de")
    )
    transformAndExtract(singleDoc, helium, "<html ", ">").assertEquals("""lang="de"""")
  }

  test("metadata (canonical link)") {
    val expected =
      meta ++ """
                |<title></title>
                |<link rel="canonical" href="http://very.canonical/"/>
                |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
                |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Fira+Mono:500">
                |<link rel="stylesheet" type="text/css" href="helium/site/icofont.min.css" />
                |<link rel="stylesheet" type="text/css" href="helium/site/laika-helium.css" />
                |<script src="helium/site/laika-helium.js"></script>
                |<script> /* for avoiding page load transitions */ </script>""".stripMargin
    transformAndExtractHead(docWithCanonicalLink, heliumBase).assertEquals(expected)
  }

  test("favicons") {
    val inputs   = Seq(
      Root / "name.md"    -> "text",
      Root / "icon-1.png" -> "",
      Root / "icon-2.png" -> ""
    )
    val helium   = heliumBase.site.favIcons(
      Favicon.internal(Root / "icon-1.png", "32x32"),
      Favicon.internal(Root / "icon-2.png", "64x64"),
      Favicon.internal(Root / "icon.svg")
    )
    val expected =
      meta ++ """
                |<title></title>
                |<link rel="icon" sizes="32x32" type="image/png" href="icon-1.png"/>
                |<link rel="icon" sizes="64x64" type="image/png" href="icon-2.png"/>
                |<link rel="icon"  type="image/svg+xml" href="icon.svg"/>
                |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
                |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Fira+Mono:500">
                |<link rel="stylesheet" type="text/css" href="helium/site/icofont.min.css" />
                |<link rel="stylesheet" type="text/css" href="helium/site/laika-helium.css" />
                |<script src="helium/site/laika-helium.js"></script>
                |<script> /* for avoiding page load transitions */ </script>""".stripMargin
    transformAndExtractHead(inputs, helium).assertEquals(expected)
  }

  test("favicons with explicit target formats") {
    val inputs   = Seq(
      Root / "name.md"                 -> "text",
      Root / "site" / "directory.conf" -> "laika.targetFormats = [html]",
      Root / "site" / "icon-1.png"     -> "",
      Root / "site" / "icon-2.png"     -> ""
    )
    val helium   = heliumBase.site.favIcons(
      Favicon.internal(Root / "site" / "icon-1.png", "32x32"),
      Favicon.internal(Root / "site" / "icon-2.png", "64x64")
    )
    val expected =
      meta ++
        """
          |<title></title>
          |<link rel="icon" sizes="32x32" type="image/png" href="site/icon-1.png"/>
          |<link rel="icon" sizes="64x64" type="image/png" href="site/icon-2.png"/>
          |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
          |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Fira+Mono:500">
          |<link rel="stylesheet" type="text/css" href="helium/site/icofont.min.css" />
          |<link rel="stylesheet" type="text/css" href="helium/site/laika-helium.css" />
          |<script src="helium/site/laika-helium.js"></script>
          |<script> /* for avoiding page load transitions */ </script>""".stripMargin
    transformAndExtractHead(inputs, helium, validation = LinkValidation.Global())
      .assertEquals(expected)
  }

  test("unversioned favicons in a versioned input tree") {

    val pathUnderTest = Root / "0.42" / "dir" / "name.html"
    val inputs        = Seq(
      Root / "directory.conf"         -> "laika.versioned = true",
      Root / "dir" / "name.md"        -> "text",
      Root / "img" / "directory.conf" -> "laika.versioned = false",
      Root / "img" / "icon-1.png"     -> "",
      Root / "img" / "icon-2.png"     -> ""
    )
    val helium        = heliumBase
      .site.versions(versions)
      .site.favIcons(
        Favicon.internal(Root / "img" / "icon-1.png", "32x32"),
        Favicon.internal(Root / "img" / "icon-2.png", "64x64")
      )
    val expected      =
      meta ++ """
                |<title></title>
                |<link rel="icon" sizes="32x32" type="image/png" href="../../img/icon-1.png"/>
                |<link rel="icon" sizes="64x64" type="image/png" href="../../img/icon-2.png"/>
                |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
                |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Fira+Mono:500">
                |<link rel="stylesheet" type="text/css" href="../helium/site/icofont.min.css" />
                |<link rel="stylesheet" type="text/css" href="../helium/site/laika-helium.css" />
                |<script src="../helium/site/laika-helium.js"></script>
                |<script src="../helium/site/laika-versions.js"></script>
                |<script>initVersions("../../", "/dir/name.html", "0.42", null);</script>
                |<script> /* for avoiding page load transitions */ </script>""".stripMargin
    transformAndExtractHead(inputs, helium, pathUnderTest).assertEquals(expected)
  }

  test("custom web fonts - added to default fonts") {
    val helium   = heliumBase.site.addFontResources(testFonts *)
    val expected =
      meta ++ """
                |<title></title>
                |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
                |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Fira+Mono:500">
                |<link rel="stylesheet" href="http://fonts.com/font-1.css">
                |<link rel="stylesheet" href="http://fonts.com/font-2.css">
                |<link rel="stylesheet" type="text/css" href="helium/site/icofont.min.css" />
                |<link rel="stylesheet" type="text/css" href="helium/site/laika-helium.css" />
                |<script src="helium/site/laika-helium.js"></script>
                |<script> /* for avoiding page load transitions */ </script>""".stripMargin
    transformAndExtractHead(singleDoc, helium).assertEquals(expected)
  }

  test("custom web fonts - replacing default fonts") {
    val helium   = heliumBase.site.clearFontResources.site.addFontResources(testFonts *)
    val expected =
      meta ++ """
                |<title></title>
                |<link rel="stylesheet" href="http://fonts.com/font-1.css">
                |<link rel="stylesheet" href="http://fonts.com/font-2.css">
                |<link rel="stylesheet" type="text/css" href="helium/site/icofont.min.css" />
                |<link rel="stylesheet" type="text/css" href="helium/site/laika-helium.css" />
                |<script src="helium/site/laika-helium.js"></script>
                |<script> /* for avoiding page load transitions */ </script>""".stripMargin
    transformAndExtractHead(singleDoc, helium).assertEquals(expected)
  }

  test("version menu on a versioned page") {
    val helium   = heliumBase.site.versions(versions).site.baseURL("https://foo.org/")
    val expected =
      meta ++ """
                |<title></title>
                |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
                |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Fira+Mono:500">
                |<link rel="stylesheet" type="text/css" href="helium/site/icofont.min.css" />
                |<link rel="stylesheet" type="text/css" href="helium/site/laika-helium.css" />
                |<script src="helium/site/laika-helium.js"></script>
                |<script src="helium/site/laika-versions.js"></script>
                |<script>initVersions("../", "/name.html", "0.42", "https://foo.org/");</script>
                |<script> /* for avoiding page load transitions */ </script>""".stripMargin
    transformAndExtractHead(singleVersionedDoc, helium, Root / "0.42" / "name.html").assertEquals(
      expected
    )
  }

  test("version menu on an unversioned page") {
    val helium   = heliumBase.site.versions(versions)
    val expected =
      meta ++ """
                |<title></title>
                |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
                |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Fira+Mono:500">
                |<link rel="stylesheet" type="text/css" href="helium/site/icofont.min.css" />
                |<link rel="stylesheet" type="text/css" href="helium/site/laika-helium.css" />
                |<script src="helium/site/laika-helium.js"></script>
                |<script src="helium/site/laika-versions.js"></script>
                |<script>initVersions("", "", "", null);</script>
                |<script> /* for avoiding page load transitions */ </script>""".stripMargin
    transformAndExtractHead(singleDoc, helium).assertEquals(expected)
  }

  test("title - ignoring markup") {
    val markup =
      """
        |Title **1**
        |===========
        |
        |Text
      """.stripMargin
    val inputs = Seq(
      Root / "name.md" -> markup
    )
    transformAndExtractHead(inputs, heliumBase).map(_.extractTag("title")).assertEquals(
      Some("Title 1")
    )
  }

  test("override head template fragment per page") {
    val markup   =
      """
        |{%
        |  helium.site.templates.head = "/head.template.html"
        |%}
        |
        |Title
        |=====
        |
        |Text
      """.stripMargin
    val inputs   = Seq(
      Root / "name.md" -> markup,
      Root / "head.template.html" -> "<head><title>XX ${cursor.currentDocument.title}</title></head>"
    )
    val expected = "<title>XX Title</title>"
    transformAndExtractHead(inputs, heliumBase).assertEquals(expected)
  }

  test("override head template fragment globally") {
    val markup   =
      """
        |Title
        |=====
        |
        |Text
      """.stripMargin
    val inputs   = Seq(
      Root / "name.md" -> markup,
      Root / "helium" / "templates" / "head.template.html" -> "<head><title>XX ${cursor.currentDocument.title}</title></head>"
    )
    val expected = "<title>XX Title</title>"
    transformAndExtractHead(inputs, heliumBase).assertEquals(expected)
  }

  test("add mermaid initializer when document contains one or more mermaid diagrams") {
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
    val inputs   = Seq(
      Root / "name.md" -> markup
    )
    val expected =
      meta ++
        """
          |<title>Title</title>
          |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
          |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Fira+Mono:500">
          |<link rel="stylesheet" type="text/css" href="helium/site/icofont.min.css" />
          |<link rel="stylesheet" type="text/css" href="helium/site/laika-helium.css" />
          |<script src="helium/site/laika-helium.js"></script>
          |<script type="module">
          |import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.esm.min.mjs';
          |const dark = window.matchMedia('(prefers-color-scheme: dark)').matches;
          |mermaid.initialize({
          |startOnLoad: true,
          |theme: "base",
          |themeVariables: {
          |'darkMode': dark,
          |'primaryColor': dark ? '#125d75' : '#eef5f6',
          |'primaryTextColor': dark ? '#a7d4de' : '#007c99',
          |'primaryBorderColor': dark ? '#a7d4de' : '#a7d4de',
          |'lineColor': dark ? '#a7d4de' : '#007c99',
          |'background': dark ? '#064458' : '#ffffff',
          |}
          |});
          |</script>
          |<script> /* for avoiding page load transitions */ </script>""".stripMargin
    transformAndExtractHead(inputs, heliumBase).assertEquals(expected)
  }

}
