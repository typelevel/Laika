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

package laika.ast

import laika.api.MarkupParser
import laika.api.builder.OperationConfig
import laika.ast.sample.ParagraphCompanionShortcuts
import laika.config.LaikaKeys
import laika.format.Markdown
import munit.FunSuite

class DocumentAPISpec extends FunSuite
    with ParagraphCompanionShortcuts {

  val defaultParser: MarkupParser = MarkupParser.of(Markdown).build

  test("specify a title in a config section") {
    val markup = """{% laika.title: Foo and Bar %}
                   |
                   |# Ignored Title
                   |
                   |Some text
                   |
                   |## Section
                   |
                   |Some more text""".stripMargin

    assertEquals(
      defaultParser.parse(markup).map(_.title),
      Right(Some(SpanSequence("Foo and Bar")))
    )
  }

  test("no title if run in the default mode for pure parsers where title extraction is disabled") {
    val markup = """# Title
                   |
                   |Some text
                   |
                   |## Section
                   |
                   |Some more text""".stripMargin

    assertEquals(
      defaultParser.parse(markup).map(_.title),
      Right(None)
    )
  }

  test(
    "use the title from the first headline if it is not overridden in a config section and title extraction is enabled"
  ) {
    val markup = """# Title
                   |
                   |Some text
                   |
                   |## Section
                   |
                   |Some more text""".stripMargin

    val res = MarkupParser
      .of(Markdown)
      .withConfigValue(LaikaKeys.firstHeaderAsTitle, true)
      .build
      .parse(markup)
      .flatMap(_.title.toRight("no title"))

    assertEquals(res, Right(SpanSequence("Title")))
  }

  test(
    "return an empty list if there is neither a structure with a title nor a title in a config section"
  ) {
    val markup = """Some text
                   |
                   |Some more text""".stripMargin

    assertEquals(
      defaultParser.parse(markup).map(_.title),
      Right(None)
    )
  }

  test("produce the same result when rewriting a document once or twice") {
    val markup = """# Section 1
                   |
                   |Some text
                   |
                   |# Section 2
                   |
                   |Some more text""".stripMargin

    val doc = defaultParser.parseUnresolved(markup).toOption.get.document

    val rewritten1 = OperationConfig.default
      .rewriteRulesFor(doc, RewritePhase.Resolve)
      .flatMap(doc.rewrite)
    val rewritten2 = rewritten1
      .flatMap(doc =>
        OperationConfig.default
          .rewriteRulesFor(doc, RewritePhase.Resolve)
          .flatMap(doc.rewrite)
      )

    assertEquals(
      rewritten1.map(_.content),
      rewritten2.map(_.content)
    )
  }

  test("allow to rewrite the document using a custom rule") {
    val markup = """# Title
                   |
                   |# Section 1
                   |
                   |Some text
                   |
                   |# Section 2
                   |
                   |Some more text""".stripMargin

    val raw       = defaultParser.parseUnresolved(markup).toOption.get.document.modifyConfig(
      _.withValue(LaikaKeys.orphan, false)
    )
    val testRule  = RewriteRules.forSpans { case Text("Some text", _) =>
      Replace(Text("Swapped"))
    }
    val rewritten = OperationConfig.default
      .rewriteRulesFor(raw, RewritePhase.Resolve)
      .flatMap(r => raw.rewrite(testRule ++ r))

    assertEquals(
      rewritten.map(_.content),
      Right(
        RootElement(
          Section(
            Header(1, List(Text("Title")), Id("title") + Style.section),
            Nil
          ),
          Section(
            Header(1, List(Text("Section 1")), Id("section-1") + Style.section),
            List(p("Swapped"))
          ),
          Section(
            Header(1, List(Text("Section 2")), Id("section-2") + Style.section),
            List(p("Some more text"))
          )
        )
      )
    )
  }

}
