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

package laika.internal.rst

import laika.api.builder.OperationConfig
import laika.api.config.{ Config, ConfigBuilder }
import laika.ast.*
import laika.ast.sample.{ ParagraphCompanionShortcuts, TestSourceBuilders }
import laika.api.config.Config.ConfigResult
import laika.config.LaikaKeys
import laika.internal.rst.ast.Underline
import laika.parse.SourceCursor
import munit.FunSuite

class RewriteRulesSpec extends FunSuite with ParagraphCompanionShortcuts with TestSourceBuilders {

  private def rewritten(
      root: RootElement,
      withTitles: Boolean = true
  ): ConfigResult[RootElement] = {
    val config =
      if (withTitles)
        ConfigBuilder.empty.withValue(LaikaKeys.firstHeaderAsTitle, true).build
      else Config.empty
    val doc    = Document(Path.Root / "doc", root).withConfig(config)
    OperationConfig.default
      .rewriteRulesFor(doc, RewritePhase.Resolve)
      .flatMap(doc.rewrite(_).map(_.content))
  }

  private def linkIdRef(id: String = "name") =
    LinkIdReference(List(Text("text")), id, generatedSource(s"<<$id>>"))

  private def extLink(url: String) = SpanLink.external(url)("text")

  private def runRoot(input: RootElement, expected: RootElement): Unit =
    assertEquals(rewritten(input), Right(expected))

  private def runRootWithoutTitles(input: RootElement, expected: RootElement): Unit =
    assertEquals(rewritten(input, withTitles = false), Right(expected))

  test("link id refs - resolve references when some parent element also gets rewritten") {
    val rootElem = RootElement(
      DecoratedHeader(Underline('#'), List(Text("text "), linkIdRef()), SourceCursor.Generated),
      LinkDefinition("name", ExternalTarget("http://foo/"))
    )
    val resolved =
      RootElement(Title(List(Text("text "), extLink("http://foo/")), Id("text-text") + Style.title))
    runRoot(rootElem, resolved)
  }

  test("decorated headers - set the level of the header in a flat list of headers") {
    val rootElem = RootElement(
      DecoratedHeader(Underline('#'), List(Text("Title")), SourceCursor.Generated),
      DecoratedHeader(Underline('#'), List(Text("Header 1")), SourceCursor.Generated),
      DecoratedHeader(Underline('#'), List(Text("Header 2")), SourceCursor.Generated)
    )
    val expected = RootElement(
      Title(List(Text("Title")), Id("title") + Style.title),
      Section(Header(1, List(Text("Header 1")), Id("header-1") + Style.section), Nil),
      Section(Header(1, List(Text("Header 2")), Id("header-2") + Style.section), Nil)
    )
    runRoot(rootElem, expected)
  }

  test("decorated headers - set the level of the header in a nested list of headers") {
    val rootElem = RootElement(
      DecoratedHeader(Underline('#'), List(Text("Title")), SourceCursor.Generated),
      DecoratedHeader(Underline('#'), List(Text("Header 1")), SourceCursor.Generated),
      DecoratedHeader(Underline('-'), List(Text("Header 2")), SourceCursor.Generated),
      DecoratedHeader(Underline('#'), List(Text("Header 3")), SourceCursor.Generated)
    )
    val expected = RootElement(
      Title(List(Text("Title")), Id("title") + Style.title),
      Section(
        Header(1, List(Text("Header 1")), Id("header-1") + Style.section),
        List(Section(Header(2, List(Text("Header 2")), Id("header-2") + Style.section), Nil))
      ),
      Section(Header(1, List(Text("Header 3")), Id("header-3") + Style.section), Nil)
    )
    runRoot(rootElem, expected)
  }

  test(
    "decorated headers - do not create title nodes in the default configuration for orphan documents"
  ) {
    val rootElem = RootElement(
      DecoratedHeader(Underline('#'), List(Text("Title")), SourceCursor.Generated),
      DecoratedHeader(Underline('#'), List(Text("Header 1")), SourceCursor.Generated),
      DecoratedHeader(Underline('#'), List(Text("Header 2")), SourceCursor.Generated)
    )
    val expected = RootElement(
      Section(Header(1, List(Text("Title")), Id("title") + Style.section), Nil),
      Section(Header(1, List(Text("Header 1")), Id("header-1") + Style.section), Nil),
      Section(Header(1, List(Text("Header 2")), Id("header-2") + Style.section), Nil)
    )
    runRootWithoutTitles(rootElem, expected)
  }

  test(
    "decorated headers - respect explicitly assigned ids"
  ) {
    val rootElem = RootElement(
      DecoratedHeader(Underline('#'), List(Text("Title")), SourceCursor.Generated).withId(
        "explicit"
      )
    )
    val expected = RootElement(
      Section(Header(1, List(Text("Title")), Id("explicit") + Style.section), Nil)
    )
    runRootWithoutTitles(rootElem, expected)
  }

}
