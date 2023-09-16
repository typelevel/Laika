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
import laika.format.{ Markdown, XSLFO }
import laika.io.api.TreeTransformer
import laika.io.helper.{ InputBuilder, ResultExtractor, StringOps }
import laika.io.syntax._
import laika.theme._
import munit.CatsEffectSuite

class HeliumFOTocPageSpec extends CatsEffectSuite with InputBuilder with ResultExtractor
    with StringOps {

  def transformer(theme: ThemeProvider): Resource[IO, TreeTransformer[IO]] = Transformer
    .from(Markdown)
    .to(XSLFO)
    .parallel[IO]
    .withTheme(theme)
    .build

  val twoDocs = Seq(
    Root / "doc-1.md"                       -> "text",
    Root / "doc-2.md"                       -> "text",
    Root / "dir-1" / "doc-3.md"             -> "text",
    Root / "dir-1" / "sub-dir" / "doc-4.md" -> "text"
  )

  def transformAndExtract(
      inputs: Seq[(Path, String)],
      helium: Helium,
      start: String,
      end: String
  ): IO[String] = transformer(helium.build).use { t =>
    for {
      resultTree <- t.fromInput(build(inputs)).toMemory.transform
      res        <- IO.fromEither(
        resultTree.extractTidiedSubstring(Root / "table-of-content.fo", start, end)
          .toRight(new RuntimeException("Missing document under test"))
      )
    } yield res
  }

  test("no table of content page configured") {
    transformAndExtract(twoDocs, Helium.defaults, "", "")
      .interceptMessage[RuntimeException]("Missing document under test")
  }

  test("table of content included") {
    val expected =
      """<fo:block id="_table-of-content" page-break-before="always">
        |<fo:marker marker-class-name="chapter"><fo:block>Contents</fo:block></fo:marker>
        |</fo:block>
        |<fo:block color="#007c99" font-family="Lato" font-size="24pt" font-weight="bold" keep-with-next="always" space-after="6mm" space-before="0mm">Contents</fo:block>
        |<fo:block color="#931813" font-family="Lato" font-size="22pt" font-weight="bold" line-height="1.5" margin-left="0mm" space-after="0mm" space-before="15mm" text-align="justify" text-align-last="center" text-transform="uppercase"><fo:basic-link color="#007c99" font-weight="bold" internal-destination="_doc-1">doc-1.md<fo:leader leader-pattern="dots" padding-left="2mm" padding-right="2mm"></fo:leader><fo:page-number-citation ref-id="_doc-1" /></fo:basic-link></fo:block>
        |<fo:block color="#931813" font-family="Lato" font-size="22pt" font-weight="bold" line-height="1.5" margin-left="0mm" space-after="0mm" space-before="15mm" text-align="justify" text-align-last="center" text-transform="uppercase"><fo:basic-link color="#007c99" font-weight="bold" internal-destination="_doc-2">doc-2.md<fo:leader leader-pattern="dots" padding-left="2mm" padding-right="2mm"></fo:leader><fo:page-number-citation ref-id="_doc-2" /></fo:basic-link></fo:block>
        |<fo:block color="#931813" font-family="Lato" font-size="22pt" font-weight="bold" keep-with-next="always" line-height="1.5" margin-left="0mm" space-after="0mm" space-before="15mm" text-align="justify" text-align-last="center" text-transform="uppercase">dir-1</fo:block>
        |<fo:block color="#931813" font-family="Lato" font-size="17pt" keep-with-previous="always" line-height="1.5" margin-left="4mm" space-after="0mm" space-before="7mm" text-align="justify" text-align-last="justify"><fo:basic-link color="#931813" font-weight="bold" internal-destination="_dir-1_doc-3">doc-3.md<fo:leader leader-pattern="dots" padding-left="2mm" padding-right="2mm"></fo:leader><fo:page-number-citation ref-id="_dir-1_doc-3" /></fo:basic-link></fo:block>""".stripMargin
    val helium   = Helium.defaults.pdf.tableOfContent("Contents", 2)
    transformAndExtract(
      twoDocs,
      helium,
      "<fo:flow flow-name=\"xsl-region-body\">",
      "</fo:flow>"
    ).assertEquals(expected)
  }

}
