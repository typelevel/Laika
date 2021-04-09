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
import laika.ast.Path.Root
import laika.ast.sample.ParagraphCompanionShortcuts
import laika.config.LaikaKeys
import laika.format.Markdown
import laika.parse.markup.DocumentParser.ParserError
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DocumentAPISpec extends AnyFlatSpec
  with Matchers
  with ParagraphCompanionShortcuts {


  val defaultParser = MarkupParser.of(Markdown).build

  "The Document API" should "allow to specify a title in a config section" in {
    val markup = """{% laika.title: Foo and Bar %}
      |
      |# Ignored Title
      |
      |Some text
      |
      |## Section
      |
      |Some more text""".stripMargin

    defaultParser.parse(markup).toOption.get.title should be (Some(SpanSequence("Foo and Bar")))
  }

  it should "should have no title if run in the default mode for pure parsers where title extraction is disabled" in {
    val markup = """# Title
      |
      |Some text
      |
      |## Section
      |
      |Some more text""".stripMargin

    defaultParser.parse(markup).toOption.get.title should be (None)
  }

  it should "use the title from the first headline if it is not overridden in a config section and title extraction is enabled" in {
    val markup = """# Title
                   |
                   |Some text
                   |
                   |## Section
                   |
                   |Some more text""".stripMargin

    MarkupParser
      .of(Markdown)
      .withConfigValue(LaikaKeys.firstHeaderAsTitle, true)
      .build
      .parse(markup)
      .flatMap(_.title.toRight(ParserError("no title", Root))) should be (Right(SpanSequence("Title")))
  }

  it should "return an empty list if there is neither a structure with a title nor a title in a config section" in {
    val markup = """Some text
      |
      |Some more text""".stripMargin

    defaultParser.parse(markup).toOption.get.title should be (None)
  }

  it should "produce the same result when rewriting a document once or twice" in {
    val markup = """# Section 1
      |
      |Some text
      |
      |# Section 2
      |
      |Some more text""".stripMargin
    
    val doc = defaultParser.parseUnresolved(markup).toOption.get.document

    val rewritten1 = OperationConfig.default.rewriteRulesFor(doc).map(doc.rewrite)
    val rewritten2 = rewritten1.flatMap(doc => OperationConfig.default.rewriteRulesFor(doc).map(doc.rewrite))
    rewritten1.map(_.content) should be (rewritten2.map(_.content))
  }

  it should "allow to rewrite the document using a custom rule" in {
    val markup = """# Title
      |
      |# Section 1
      |
      |Some text
      |
      |# Section 2
      |
      |Some more text""".stripMargin

    val raw = defaultParser.parseUnresolved(markup).toOption.get.document
    val testRule = RewriteRules.forSpans {
      case Text("Some text",_) => Replace(Text("Swapped"))
    }
    val rewritten = OperationConfig.default
      .rewriteRulesFor(raw.copy(position = TreePosition.root))
      .map(r => raw.rewrite(testRule ++ r))
    rewritten.map(_.content) should be (Right(RootElement(
      Title(List(Text("Title")), Id("title") + Style.title),
      Section(Header(1, List(Text("Section 1")), Id("section-1") + Style.section), List(p("Swapped"))),
      Section(Header(1, List(Text("Section 2")), Id("section-2") + Style.section), List(p("Some more text")))
    )))
  }


}
