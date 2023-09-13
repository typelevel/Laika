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
import laika.ast.Path
import laika.ast.Path.Root
import laika.format.{ EPUB, Markdown }
import laika.io.api.TreeTransformer
import laika.io.helper.{ InputBuilder, ResultExtractor, StringOps }
import laika.io.implicits.*
import laika.io.model.StringTreeOutput
import laika.theme.*
import munit.CatsEffectSuite

class HeliumEPUBHeadSpec extends CatsEffectSuite with InputBuilder with ResultExtractor
    with StringOps {

  def transformer(theme: ThemeProvider): Resource[IO, TreeTransformer[IO]] = Transformer
    .from(Markdown)
    .to(EPUB.XHTML)
    .parallel[IO]
    .withTheme(theme)
    .build

  val singleDoc = Seq(
    Root / "name.md" -> "text"
  )

  private val defaultResult =
    s"""<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
       |<meta name="viewport" content="width=device-width, initial-scale=1.0"/>
       |<meta name="generator" content="Typelevel Laika + Helium Theme" />
       |<title></title>
       |<link rel="stylesheet" type="text/css" href="helium/epub/laika-helium.css" />""".stripMargin

  def transformAndExtractHead(inputs: Seq[(Path, String)]): IO[String] =
    transformAndExtractHead(inputs, Helium.defaults)

  def transformAndExtractHead(inputs: Seq[(Path, String)], helium: Helium): IO[String] =
    transformer(helium.build).use { t =>
      for {
        resultTree <- t.fromInput(build(inputs)).toOutput(StringTreeOutput).transform
        res        <- IO.fromEither(
          resultTree.extractTidiedTagContent((Root / "name").withSuffix("xhtml"), "head")
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
      resultTree <- t.fromInput(build(inputs)).toOutput(StringTreeOutput).transform
      res        <- IO.fromEither(
        resultTree.extractTidiedSubstring(Root / "name.xhtml", start, end)
          .toRight(new RuntimeException("Missing document under test"))
      )
    } yield res
  }

  test("Helium defaults via transformer") {
    transformAndExtractHead(singleDoc).assertEquals(defaultResult)
  }

  test("custom configuration for CSS and JS file locations") {
    val inputs   = Seq(
      Root / "name.md"                       -> "text",
      Root / "web" / "foo.js"                -> "",
      Root / "web" / "foo.shared.css"        -> "",
      Root / "custom-js" / "foo.epub.js"     -> "",
      Root / "custom-css" / "foo.shared.css" -> ""
    )
    val helium   = Helium.defaults
      .epub.internalCSS(Root / "custom-css")
      .epub.internalJS(Root / "custom-js")
    val expected =
      s"""<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
         |<meta name="viewport" content="width=device-width, initial-scale=1.0"/>
         |<meta name="generator" content="Typelevel Laika + Helium Theme" />
         |<title></title>
         |<link rel="stylesheet" type="text/css" href="helium/epub/laika-helium.css" />
         |<link rel="stylesheet" type="text/css" href="custom-css/foo.shared.css" />
         |<script src="custom-js/foo.epub.js"></script>""".stripMargin
    transformAndExtractHead(inputs, helium).assertEquals(expected)
  }

  test("metadata (authors, description)") {
    val helium   = Helium.defaults.all.metadata(
      authors = Seq("Maria Green", "Elena Blue"),
      description = Some("Some description")
    )
    val expected =
      s"""<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
         |<meta name="viewport" content="width=device-width, initial-scale=1.0"/>
         |<meta name="generator" content="Typelevel Laika + Helium Theme" />
         |<title></title>
         |<meta name="author" content="Maria Green"/>
         |<meta name="author" content="Elena Blue"/>
         |<meta name="description" content="Some description"/>
         |<link rel="stylesheet" type="text/css" href="helium/epub/laika-helium.css" />""".stripMargin
    transformAndExtractHead(singleDoc, helium).assertEquals(expected)
  }

  test("metadata (language)") {
    val helium = Helium.defaults.all.metadata(
      language = Some("de")
    )
    transformAndExtract(singleDoc, helium, "<html ", ">").assertEquals(
      """lang="de" xmlns="http://www.w3.org/1999/xhtml" xmlns:epub="http://www.idpf.org/2007/ops""""
    )
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
