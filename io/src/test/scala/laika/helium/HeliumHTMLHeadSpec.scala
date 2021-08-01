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
import laika.api.{MarkupParser, Renderer, Transformer}
import laika.ast.Path
import laika.ast.Path.Root
import laika.config.LaikaKeys
import laika.format.{HTML, Markdown}
import laika.helium.config.Favicon
import laika.io.IOFunSuite
import laika.io.api.{TreeParser, TreeRenderer, TreeTransformer}
import laika.io.helper.{InputBuilder, ResultExtractor, StringOps}
import laika.io.implicits._
import laika.io.model.StringTreeOutput
import laika.rewrite.link.LinkConfig
import laika.rewrite.{Version, Versions}
import laika.theme._
import laika.theme.config.{Font, FontDefinition, FontStyle, FontWeight}

class HeliumHTMLHeadSpec extends IOFunSuite with InputBuilder with ResultExtractor with StringOps {

  val parser: Resource[IO, TreeParser[IO]] = MarkupParser
    .of(Markdown)
    .withConfigValue(LinkConfig(excludeFromValidation = Seq(Root)))
    .parallel[IO]
    .build

  val renderer: Resource[IO, TreeRenderer[IO]] = {
    val builder = Renderer.of(HTML)
    builder.withConfig(builder.config.withBundlesFor(Markdown)) // TODO - there should be dedicated API for this scenario
      .parallel[IO]
      .build
  }
  
  val parserAndRenderer: Resource[IO, (TreeParser[IO], TreeRenderer[IO])] = for {
    p <- parser
    r <- renderer
  } yield (p, r)
  
  def transformer (theme: ThemeProvider): Resource[IO, TreeTransformer[IO]] = Transformer
    .from(Markdown)
    .to(HTML)
    .withConfigValue(LaikaKeys.links.child("excludeFromValidation"), Seq("/"))
    .parallel[IO]
    .withTheme(theme)
    .build
  
  val singleDoc = Seq(
    Root / "name.md" -> "text"
  )
  
  val meta = """<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
               |<meta charset="utf-8">
               |<meta name="viewport" content="width=device-width, initial-scale=1.0">
               |<meta name="generator" content="Laika 0.18.0 + Helium Theme" />""".stripMargin
  
  val defaultResult = meta ++ """
    |<title></title>
    |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
    |<link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/tonsky/FiraCode@1.207/distr/fira_code.css">
    |<link rel="stylesheet" type="text/css" href="helium/icofont.min.css" />
    |<link rel="stylesheet" type="text/css" href="helium/laika-helium.css" />
    |<script src="helium/laika-helium.js"></script>
    |<script> /* for avoiding page load transitions */ </script>""".stripMargin

  val versions = Versions(
    Version("0.42.x", "0.42"),
    Seq(
      Version("0.41.x", "0.41"),
      Version("0.40.x", "0.40", "toc.html")
    ),
    Seq(
      Version("0.43.x", "0.43")
    )
  )
  
  def transformAndExtractHead(inputs: Seq[(Path, String)]): IO[String] = transformAndExtractHead(inputs, Helium.defaults)

  def transformAndExtractHead(inputs: Seq[(Path, String)], helium: Helium, underTest: Path = Root / "name.html"): IO[String] = transformer(helium.build).use { t =>
    for {
      resultTree <- t.fromInput(build(inputs)).toOutput(StringTreeOutput).transform
      res        <- IO.fromEither(resultTree.extractTidiedTagContent(underTest, "head")
        .toRight(new RuntimeException("Missing document under test")))
    } yield res
  }

  def transformAndExtract(inputs: Seq[(Path, String)], helium: Helium, start: String, end: String): IO[String] = transformer(helium.build).use { t =>
    for {
      resultTree <- t.fromInput(build(inputs)).toOutput(StringTreeOutput).transform
      res        <- IO.fromEither(resultTree.extractTidiedSubstring(Root / "name.html", start, end)
        .toRight(new RuntimeException("Missing document under test")))
    } yield res
  }
    
  test("Helium defaults via separate parser and renderer") {
    parserAndRenderer.use {
      case (p, r) => p.fromInput(build(singleDoc)).parse.flatMap { tree =>
        r.from(tree.root).toOutput(StringTreeOutput).render
      }
    }
      .map(_.extractTidiedSubstring(Root / "name.html", "<head>", "</head>"))
      .assertEquals(Some(defaultResult))
  }

  test("Helium defaults via transformer") {
    transformAndExtractHead(singleDoc).assertEquals(defaultResult)
  }
  
  test("exclude CSS and JS from API directory") {
    val inputs = Seq(
      Root / "name.md" -> "text",
      Root / "api" / "foo.js" -> "",
      Root / "api" / "foo.css" -> "",
    )
    transformAndExtractHead(inputs).assertEquals(defaultResult)
  }
  
  test("custom CSS and JS files") {
    val inputs = Seq(
      Root / "name.md" -> "text",
      Root / "web" / "foo.js" -> "",
      Root / "web" / "foo.css" -> "",
    )
    val expected = meta ++ """
                   |<title></title>
                   |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
                   |<link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/tonsky/FiraCode@1.207/distr/fira_code.css">
                   |<link rel="stylesheet" type="text/css" href="helium/icofont.min.css" />
                   |<link rel="stylesheet" type="text/css" href="helium/laika-helium.css" />
                   |<link rel="stylesheet" type="text/css" href="web/foo.css" />
                   |<script src="helium/laika-helium.js"></script>
                   |<script src="web/foo.js"></script>
                   |<script> /* for avoiding page load transitions */ </script>""".stripMargin
    transformAndExtractHead(inputs).assertEquals(expected)
  }

  test("custom configuration for CSS and JS file locations") {
    val inputs = Seq(
      Root / "name.md" -> "text",
      Root / "web" / "foo.js" -> "",
      Root / "web" / "foo.css" -> "",
      Root / "custom-js" / "foo.js" -> "",
      Root / "custom-css" / "foo.css" -> "",
    )
    val helium = Helium.defaults
      .site.autoLinkCSS(Root / "custom-css")
      .site.autoLinkJS(Root / "custom-js")
    val expected = meta ++ """
                   |<title></title>
                   |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
                   |<link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/tonsky/FiraCode@1.207/distr/fira_code.css">
                   |<link rel="stylesheet" type="text/css" href="helium/icofont.min.css" />
                   |<link rel="stylesheet" type="text/css" href="helium/laika-helium.css" />
                   |<link rel="stylesheet" type="text/css" href="custom-css/foo.css" />
                   |<script src="helium/laika-helium.js"></script>
                   |<script src="custom-js/foo.js"></script>
                   |<script> /* for avoiding page load transitions */ </script>""".stripMargin
    transformAndExtractHead(inputs, helium).assertEquals(expected)
  }
  
  test("metadata (authors, description)") {
    val helium = Helium.defaults.all.metadata(
      authors = Seq("Maria Green", "Elena Blue"),
      description = Some("Some description")
    )
    val expected = meta ++ """
                   |<title></title>
                   |<meta name="author" content="Maria Green"/>
                   |<meta name="author" content="Elena Blue"/>
                   |<meta name="description" content="Some description"/>
                   |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
                   |<link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/tonsky/FiraCode@1.207/distr/fira_code.css">
                   |<link rel="stylesheet" type="text/css" href="helium/icofont.min.css" />
                   |<link rel="stylesheet" type="text/css" href="helium/laika-helium.css" />
                   |<script src="helium/laika-helium.js"></script>
                   |<script> /* for avoiding page load transitions */ </script>""".stripMargin
    transformAndExtractHead(singleDoc, helium).assertEquals(expected)
  }

  test("metadata (language)") {
    val helium = Helium.defaults.all.metadata(
      language = Some("de")
    )
    transformAndExtract(singleDoc, helium, "<html ", ">").assertEquals("""lang="de"""")
  }

  test("favicons") {
    val inputs = Seq(
      Root / "name.md" -> "text",
      Root / "icon-1.png" -> "",
      Root / "icon-2.png" -> "",
    )
    val helium = Helium.defaults.site.favIcons(
      Favicon.internal(Root / "icon-1.png", "32x32"),
      Favicon.internal(Root / "icon-2.png", "64x64")
    )
    val expected = meta ++ """
                   |<title></title>
                   |<link rel="icon" sizes="32x32" type="image/png" href="icon-1.png"/>
                   |<link rel="icon" sizes="64x64" type="image/png" href="icon-2.png"/>
                   |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
                   |<link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/tonsky/FiraCode@1.207/distr/fira_code.css">
                   |<link rel="stylesheet" type="text/css" href="helium/icofont.min.css" />
                   |<link rel="stylesheet" type="text/css" href="helium/laika-helium.css" />
                   |<script src="helium/laika-helium.js"></script>
                   |<script> /* for avoiding page load transitions */ </script>""".stripMargin
    transformAndExtractHead(inputs, helium).assertEquals(expected)
  }

  test("unversioned favicons in a versioned input tree") {

    val pathUnderTest = Root / "0.42" / "dir" / "name.html"
    val inputs = Seq(
      Root / "directory.conf"         -> "laika.versioned = true",
      Root / "dir" / "name.md"        -> "text",
      Root / "img" / "directory.conf" -> "laika.versioned = false",
      Root / "img" / "icon-1.png"     -> "",
      Root / "img" / "icon-2.png"     -> "",
    )
    val helium = Helium.defaults
      .site.versions(versions)
      .site.favIcons(
        Favicon.internal(Root / "img" / "icon-1.png", "32x32"),
        Favicon.internal(Root / "img" / "icon-2.png", "64x64")
      )
    val expected = meta ++ """
                   |<title></title>
                   |<link rel="icon" sizes="32x32" type="image/png" href="../../img/icon-1.png"/>
                   |<link rel="icon" sizes="64x64" type="image/png" href="../../img/icon-2.png"/>
                   |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
                   |<link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/tonsky/FiraCode@1.207/distr/fira_code.css">
                   |<link rel="stylesheet" type="text/css" href="../helium/icofont.min.css" />
                   |<link rel="stylesheet" type="text/css" href="../helium/laika-helium.css" />
                   |<script src="../helium/laika-helium.js"></script>
                   |<script src="../helium/laika-versions.js"></script>
                   |<script>initVersions("../../", "/dir/name.html", "0.42.x");</script>
                   |<script> /* for avoiding page load transitions */ </script>""".stripMargin
    transformAndExtractHead(inputs, helium, pathUnderTest).assertEquals(expected)
  }

  test("custom web fonts") {
    val helium = Helium.defaults.site.fontResources(
      FontDefinition(Font.webCSS("http://fonts.com/font-1.css"), "Font-1", FontWeight.Normal, FontStyle.Normal),
      FontDefinition(Font.webCSS("http://fonts.com/font-2.css"), "Font-2", FontWeight.Normal, FontStyle.Normal)
    )
    val expected = meta ++ """
                   |<title></title>
                   |<link rel="stylesheet" href="http://fonts.com/font-1.css">
                   |<link rel="stylesheet" href="http://fonts.com/font-2.css">
                   |<link rel="stylesheet" type="text/css" href="helium/icofont.min.css" />
                   |<link rel="stylesheet" type="text/css" href="helium/laika-helium.css" />
                   |<script src="helium/laika-helium.js"></script>
                   |<script> /* for avoiding page load transitions */ </script>""".stripMargin
    transformAndExtractHead(singleDoc, helium).assertEquals(expected)
  }

  test("versioning") {
    val versions = Versions(
      Version("0.42.x", "0.42"),
      Seq(
        Version("0.41.x", "0.41"),
        Version("0.40.x", "0.40", "toc.html")
      ),
      Seq(
        Version("0.43.x", "0.43")
      )
    )
    val helium = Helium.defaults.site.landingPage().site.versions(versions)
    val expected = meta ++ """
                    |<title></title>
                    |<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato:400,700">
                    |<link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/tonsky/FiraCode@1.207/distr/fira_code.css">
                    |<link rel="stylesheet" type="text/css" href="helium/icofont.min.css" />
                    |<link rel="stylesheet" type="text/css" href="helium/laika-helium.css" />
                    |<script src="helium/laika-helium.js"></script>
                    |<script src="helium/laika-versions.js"></script>
                    |<script>initVersions("../", "/name.html", "0.42.x");</script>
                    |<script> /* for avoiding page load transitions */ </script>""".stripMargin
    transformAndExtractHead(singleDoc, helium).assertEquals(expected)
  }

  test("title") {
    val markup =
      """
        |Title
        |=====
        |
        |Text
      """.stripMargin
    val inputs = Seq(
      Root / "name.md" -> markup
    )
    transformAndExtractHead(inputs).map(_.extractTag("title")).assertEquals(Some("Title"))
  }
  
}
