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

package laika.rst

import laika.api.builder.OperationConfig
import laika.ast._
import laika.ast.sample.{ ParagraphCompanionShortcuts, TestSourceBuilders }
import laika.format.ReStructuredText
import laika.parse.Parser
import laika.parse.markup.RootParser
import munit.FunSuite

class ExplicitBlockParsersSpec extends FunSuite with ParagraphCompanionShortcuts
    with TestSourceBuilders {

  private val defaultParser: Parser[RootElement] =
    new RootParser(
      ReStructuredText,
      OperationConfig(ReStructuredText.extensions).markupExtensions
    ).rootElement

  def run(input: String, blocks: Block*)(implicit loc: munit.Location): Unit =
    assertEquals(defaultParser.parse(input).toEither, Right(RootElement(blocks)))

  test("citation spanning a single line") {
    val input = """.. [label] This is a citation"""
    run(input, Citation("label", List(p("This is a citation"))))
  }

  test("citation spanning two lines") {
    val input = """.. [label] This is a
                  |   citation""".stripMargin
    run(input, Citation("label", List(p("This is a\ncitation"))))
  }

  test("footnote with autonumber label") {
    val input = ".. [#] This is a footnote"
    run(
      input,
      FootnoteDefinition(Autonumber, List(p("This is a footnote")), generatedSource(input))
    )
  }

  test("footnote with autosymbol label") {
    val input = ".. [*] This is a footnote"
    run(
      input,
      FootnoteDefinition(Autosymbol, List(p("This is a footnote")), generatedSource(input))
    )
  }

  test("footnote with an autonumber named label") {
    val input = ".. [#foo] This is a footnote"
    run(
      input,
      FootnoteDefinition(
        AutonumberLabel("foo"),
        List(p("This is a footnote")),
        generatedSource(input)
      )
    )
  }

  test("footnote with a numeric label") {
    val input = ".. [17] This is a footnote"
    run(
      input,
      FootnoteDefinition(NumericLabel(17), List(p("This is a footnote")), generatedSource(input))
    )
  }

  test("named external target on one line") {
    val input = """.. _some-link: http://www.foo.bar/"""
    run(input, LinkDefinition("some-link", ExternalTarget("http://www.foo.bar/")))
  }

  test("named internal target on one line") {
    val input = """.. _some-link: ../foo/bar.md#ref"""
    run(input, LinkDefinition("some-link", InternalTarget(RelativePath.parse("../foo/bar.md#ref"))))
  }

  test("named external target with the reference name in backticks") {
    val input = """.. _`some:link`: http://www.foo.bar/"""
    run(input, LinkDefinition("some:link", ExternalTarget("http://www.foo.bar/")))
  }

  test("named external target on two lines") {
    val input = """.. _some-link: http://www.
                  |     foo.bar/""".stripMargin
    run(input, LinkDefinition("some-link", ExternalTarget("http://www.foo.bar/")))
  }

  test("named external target with the URL entirely on the next line") {
    val input = """.. _some-link: 
                  |     http://www.foo.bar/""".stripMargin
    run(input, LinkDefinition("some-link", ExternalTarget("http://www.foo.bar/")))
  }

  test("named internal target with the URL entirely on the next line") {
    val input = """.. _some-link: 
                  |     ../foo/bar.md#ref""".stripMargin
    run(input, LinkDefinition("some-link", InternalTarget(RelativePath.parse("../foo/bar.md#ref"))))
  }

  test("anonymous external target") {
    val input = """.. __: http://www.foo.bar/"""
    run(input, LinkDefinition("", ExternalTarget("http://www.foo.bar/")))
  }

  test("anonymous internal target") {
    val input = """.. __: ../foo/bar.md#ref"""
    run(input, LinkDefinition("", InternalTarget(RelativePath.parse("../foo/bar.md#ref"))))
  }

  test("short anonymous external target") {
    val input = """__ http://www.foo.bar/"""
    run(input, LinkDefinition("", ExternalTarget("http://www.foo.bar/")))
  }

  test("short anonymous internal target") {
    val input = """__ ../foo/bar.md#ref"""
    run(input, LinkDefinition("", InternalTarget(RelativePath.parse("../foo/bar.md#ref"))))
  }

  test("indirect simple reference") {
    val input = """.. _ref: other_"""
    run(input, LinkAlias("ref", "other"))
  }

  test("indirect phrase reference on one line") {
    val input = """.. _ref: `other ref`_"""
    run(input, LinkAlias("ref", "other ref"))
  }

  test("indirect phrase reference on two lines") {
    val input = """.. _ref: `other
                  | ref`_""".stripMargin
    run(input, LinkAlias("ref", "other ref"))
  }

  test("indirect phrase reference on the following line") {
    val input = """.. _ref: 
                  | `other ref`_""".stripMargin
    run(input, LinkAlias("ref", "other ref"))
  }

  test("internal target") {
    val input = """.. _some-target:
                  |
                  |Some text""".stripMargin
    run(input, Paragraph(List(Text("Some text")), Id("some-target")))
  }

  test("comment on one line") {
    val input = """.. This is a comment"""
    run(input, Comment("This is a comment"))
  }

  test("comment on two lines") {
    val input = """.. This is
                  |  a comment""".stripMargin
    run(input, Comment("This is\na comment"))
  }

  test("comment with all text on the second line") {
    val input = """..  
                  |  This is a comment""".stripMargin
    run(input, Comment("This is a comment"))
  }

}
