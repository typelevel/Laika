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

import cats.effect.IO
import laika.io.implicits._
import laika.api.{MarkupParser, Renderer, Transformer}
import laika.ast.Path.Root
import laika.format.{HTML, Markdown}
import laika.io.helper.{InputBuilder, ResultExtractor}
import laika.io.model.StringTreeOutput
import laika.io.{FileIO, IOFunSuite}

class HeliumHTMLHeadSpec extends IOFunSuite with InputBuilder with ResultExtractor {

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
  
  val transformer = Transformer
    .from(Markdown)
    .to(HTML)
    .io(FileIO.blocker)
    .parallel[IO]
    .build
  
  val defaultResult = """<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
                        |<meta charset="utf-8">
                        |<meta name="viewport" content="width=device-width, initial-scale=1.0">
                        |<meta name="generator" content="Laika 0.16.0 + Helium Theme" />
                        |<title></title>
                        |<link rel="stylesheet" href="http://fonts.googleapis.com/css?family=Lato:400,700">
                        |<link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/tonsky/FiraCode@1.207/distr/fira_code.css">
                        |<link rel="stylesheet" href="../icons/icofont.min.css">
                        |<link rel="stylesheet" type="text/css" href="css/laika-helium.css" />
                        |<script src="laika/helium.js"></script>
                        |<script> /* for avoiding page load transitions */ </script>""".stripMargin

  test("Helium defaults via transformer") {
    val inputs = Seq(
      Root / "name.md" -> "text"
    )
    transformer.use {
      t => t.fromInput(build(inputs)).toOutput(StringTreeOutput).transform
    }
      .map(_.extractTidiedSubstring(Root / "name.html", "<head>", "</head>"))
      .assertEquals(Some(defaultResult))
  }
  
  test("Helium defaults via separate parser and renderer") {
    val inputs = Seq(
      Root / "name.md" -> "text"
    )
    parserAndRenderer.use {
      case (p, r) => p.fromInput(build(inputs)).parse.flatMap { tree =>
        r.from(tree.root).toOutput(StringTreeOutput).render
      }
    }
      .map(_.extractTidiedSubstring(Root / "name.html", "<head>", "</head>"))
      .assertEquals(Some(defaultResult))
  }
  
}
