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
import laika.io.implicits._
import laika.api.{MarkupParser, Renderer, Transformer}
import laika.ast.Path
import laika.ast.Path.Root
import laika.format.{HTML, Markdown}
import laika.io.helper.{InputBuilder, ResultExtractor, StringOps}
import laika.io.model.StringTreeOutput
import laika.io.{FileIO, IOFunSuite}
import laika.theme.{Font, FontDefinition, FontStyle, FontWeight, Theme}

class HeliumHTMLHeadSpec extends IOFunSuite with InputBuilder with ResultExtractor with StringOps {

  val parser = MarkupParser
    .of(Markdown)
    .io(FileIO.blocker)
    .parallel[IO]
    .build

  val renderer = Renderer
    .of(HTML)
    .io(FileIO.blocker)
    .parallel[IO]
    .build
  
  val parserAndRenderer = for {
    p <- parser
    r <- renderer
  } yield (p, r)
  
  def transformer (theme: Resource[IO, Theme[IO]]) = Transformer
    .from(Markdown)
    .to(HTML)
    .io(FileIO.blocker)
    .parallel[IO]
    .withTheme(theme)
    .build
  
  val singleDoc = Seq(
    Root / "name.md" -> "text"
  )
  
  val defaultResult = """<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
                        |<meta charset="utf-8">
                        |<meta name="viewport" content="width=device-width, initial-scale=1.0">
                        |<meta name="generator" content="Laika 0.16.0 + Helium Theme" />
                        |<title></title>
                        |<link rel="stylesheet" href="http://fonts.googleapis.com/css?family=Lato:400,700">
                        |<link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/tonsky/FiraCode@1.207/distr/fira_code.css">
                        |<link rel="stylesheet" href="../icons/icofont.min.css">
                        |<link rel="stylesheet" type="text/css" href="helium/laika-helium.css" />
                        |<script src="helium/laika-helium.js"></script>
                        |<script> /* for avoiding page load transitions */ </script>""".stripMargin
  
  def transformAndExtractHead(inputs: Seq[(Path, String)]): IO[String] = transformAndExtractHead(inputs, Helium.defaults)

  def transformAndExtractHead(inputs: Seq[(Path, String)], helium: Helium): IO[String] = transformer(helium.build).use { t =>
    for {
      resultTree <- t.fromInput(build(inputs)).toOutput(StringTreeOutput).transform
      res        <- IO.fromEither(resultTree.extractTidiedTagContent(Root / "name.html", "head")
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
    val expected = """<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
                     |<meta charset="utf-8">
                     |<meta name="viewport" content="width=device-width, initial-scale=1.0">
                     |<meta name="generator" content="Laika 0.16.0 + Helium Theme" />
                     |<title></title>
                     |<link rel="stylesheet" href="http://fonts.googleapis.com/css?family=Lato:400,700">
                     |<link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/tonsky/FiraCode@1.207/distr/fira_code.css">
                     |<link rel="stylesheet" href="../icons/icofont.min.css">
                     |<link rel="stylesheet" type="text/css" href="helium/laika-helium.css" />
                     |<link rel="stylesheet" type="text/css" href="web/foo.css" />
                     |<script src="helium/laika-helium.js"></script>
                     |<script src="web/foo.js"></script>
                     |<script> /* for avoiding page load transitions */ </script>""".stripMargin
    transformAndExtractHead(inputs).assertEquals(expected)
  }
  
  test("metadata (authors, description)") {
    val helium = Helium.defaults.allFormats.metadata(
      authors = Seq("Maria Green", "Elena Blue"),
      description = Some("Some description")
    )
    val expected = """<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
                     |<meta charset="utf-8">
                     |<meta name="viewport" content="width=device-width, initial-scale=1.0">
                     |<meta name="generator" content="Laika 0.16.0 + Helium Theme" />
                     |<title></title>
                     |<meta name="author" content="Maria Green"/>
                     |<meta name="author" content="Elena Blue"/>
                     |<meta name="description" content="Some description"/>
                     |<link rel="stylesheet" href="http://fonts.googleapis.com/css?family=Lato:400,700">
                     |<link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/tonsky/FiraCode@1.207/distr/fira_code.css">
                     |<link rel="stylesheet" href="../icons/icofont.min.css">
                     |<link rel="stylesheet" type="text/css" href="helium/laika-helium.css" />
                     |<script src="helium/laika-helium.js"></script>
                     |<script> /* for avoiding page load transitions */ </script>""".stripMargin
    transformAndExtractHead(singleDoc, helium).assertEquals(expected)
  }

  test("metadata (language)") {
    val helium = Helium.defaults.allFormats.metadata(
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
    val expected = """<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
                     |<meta charset="utf-8">
                     |<meta name="viewport" content="width=device-width, initial-scale=1.0">
                     |<meta name="generator" content="Laika 0.16.0 + Helium Theme" />
                     |<title></title>
                     |<link rel="icon" sizes="32x32" type="image/png" href="icon-1.png" />
                     |<link rel="icon" sizes="64x64" type="image/png" href="icon-2.png" />
                     |<link rel="stylesheet" href="http://fonts.googleapis.com/css?family=Lato:400,700">
                     |<link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/tonsky/FiraCode@1.207/distr/fira_code.css">
                     |<link rel="stylesheet" href="../icons/icofont.min.css">
                     |<link rel="stylesheet" type="text/css" href="helium/laika-helium.css" />
                     |<script src="helium/laika-helium.js"></script>
                     |<script> /* for avoiding page load transitions */ </script>""".stripMargin
    transformAndExtractHead(inputs, helium).assertEquals(expected)
  }

  test("custom web fonts") {
    val helium = Helium.defaults.site.fontResources(
      FontDefinition(Font.webCSS("http://fonts.com/font-1.css"), "Font-1", FontWeight.Normal, FontStyle.Normal),
      FontDefinition(Font.webCSS("http://fonts.com/font-2.css"), "Font-2", FontWeight.Normal, FontStyle.Normal)
    )
    val expected = """<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
                     |<meta charset="utf-8">
                     |<meta name="viewport" content="width=device-width, initial-scale=1.0">
                     |<meta name="generator" content="Laika 0.16.0 + Helium Theme" />
                     |<title></title>
                     |<link rel="stylesheet" href="http://fonts.com/font-1.css">
                     |<link rel="stylesheet" href="http://fonts.com/font-2.css">
                     |<link rel="stylesheet" type="text/css" href="helium/laika-helium.css" />
                     |<script src="helium/laika-helium.js"></script>
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
