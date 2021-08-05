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
import laika.ast.sample.{ParagraphCompanionShortcuts, TestSourceBuilders}
import laika.format.ReStructuredText
import laika.parse.Parser
import laika.parse.helper.MigrationFlatSpec
import laika.parse.markup.RootParser
import laika.rst.ext.Directives.DirectivePart
import laika.rst.ext.TextRoles.RoleDirectivePart
import org.scalatest.Assertion
    
class ExplicitBlockParsersSpec extends MigrationFlatSpec with ParagraphCompanionShortcuts with TestSourceBuilders {


  val defaultParser: Parser[RootElement] = 
    new RootParser(ReStructuredText, OperationConfig(ReStructuredText.extensions).markupExtensions).rootElement

  
  def blockDirective (name: String): Option[DirectivePart[Block]] = None
  def spanDirective (name: String): Option[DirectivePart[Span]] = None
  def textRole (name: String): Option[RoleDirectivePart[String => Span]] = None

  def run (input: String, blocks: Block*): Assertion =
    assertEquals(defaultParser.parse(input).toEither, Right(RootElement(blocks)))
    
  
  "The citation parser" should "parse a citation spanning a single line" in {
    val input = """.. [label] This is a citation"""
    run(input, Citation("label", List(p("This is a citation"))))
  }
  
  it should "parse a citation spanning two lines" in {
    val input = """.. [label] This is a
      |   citation""".stripMargin
    run(input, Citation("label", List(p("This is a\ncitation"))))
  }
  
  
  "The footnote parser" should "parse a footnote with autonumber label" in {
    val input = ".. [#] This is a footnote"
    run(input, FootnoteDefinition(Autonumber, List(p("This is a footnote")), generatedSource(input)))
  }
  
  it should "parse a footnote with autosymbol label" in {
    val input = ".. [*] This is a footnote"
    run(input, FootnoteDefinition(Autosymbol, List(p("This is a footnote")), generatedSource(input)))
  }
  
  it should "parse a footnote with an autonumber named label" in {
    val input = ".. [#foo] This is a footnote"
    run(input, FootnoteDefinition(AutonumberLabel("foo"), List(p("This is a footnote")), generatedSource(input)))
  }
  
  it should "parse a footnote with a numeric label" in {
    val input = ".. [17] This is a footnote"
    run(input, FootnoteDefinition(NumericLabel(17), List(p("This is a footnote")), generatedSource(input)))
  }
  
  
  "The link target parser" should "parse a named external target on one line" in {
    val input = """.. _some-link: http://www.foo.bar/"""
    run(input, LinkDefinition("some-link", ExternalTarget("http://www.foo.bar/")))
  }

  it should "parse a named internal target on one line" in {
    val input = """.. _some-link: ../foo/bar.md#ref"""
    run(input, LinkDefinition("some-link", InternalTarget(RelativePath.parse("../foo/bar.md#ref"))))
  }
  
  it should "parse a named external target with the reference name in backticks" in {
    val input = """.. _`some:link`: http://www.foo.bar/"""
    run(input, LinkDefinition("some:link", ExternalTarget("http://www.foo.bar/")))
  }
  
  it should "parse a named external target on two lines" in {
    val input = """.. _some-link: http://www.
      |     foo.bar/""".stripMargin
    run(input, LinkDefinition("some-link", ExternalTarget("http://www.foo.bar/")))
  }
  
  it should "parse a named external target with the URL entirely on the next line" in {
    val input = """.. _some-link: 
      |     http://www.foo.bar/""".stripMargin
    run(input, LinkDefinition("some-link", ExternalTarget("http://www.foo.bar/")))
  }

  it should "parse a named internal target with the URL entirely on the next line" in {
    val input = """.. _some-link: 
                  |     ../foo/bar.md#ref""".stripMargin
    run(input, LinkDefinition("some-link", InternalTarget(RelativePath.parse("../foo/bar.md#ref"))))
  }
  
  it should "parse an anonymous external target" in {
    val input = """.. __: http://www.foo.bar/"""
    run(input, LinkDefinition("", ExternalTarget("http://www.foo.bar/")))
  }

  it should "parse an anonymous internal target" in {
    val input = """.. __: ../foo/bar.md#ref"""
    run(input, LinkDefinition("", InternalTarget(RelativePath.parse("../foo/bar.md#ref"))))
  }
  
  it should "parse a short anonymous external target" in {
    val input = """__ http://www.foo.bar/"""
    run(input, LinkDefinition("", ExternalTarget("http://www.foo.bar/")))
  }

  it should "parse a short anonymous internal target" in {
    val input = """__ ../foo/bar.md#ref"""
    run(input, LinkDefinition("", InternalTarget(RelativePath.parse("../foo/bar.md#ref"))))
  }
  
  it should "parse an indirect simple reference" in {
    val input = """.. _ref: other_"""
    run(input, LinkAlias("ref", "other"))
  }
  
  it should "parse an indirect phrase reference on one line" in {
    val input = """.. _ref: `other ref`_"""
    run(input, LinkAlias("ref", "other ref"))
  }
  
  it should "parse an indirect phrase reference on two lines" in {
    val input = """.. _ref: `other
      | ref`_""".stripMargin
    run(input, LinkAlias("ref", "other ref"))
  }
  
  it should "parse an indirect phrase reference on the following" in {
    val input = """.. _ref: 
      | `other ref`_""".stripMargin
    run(input, LinkAlias("ref", "other ref"))
  }
  
  it should "parse an internal target" in {
    val input = """.. _some-target:
      |
      |Some text""".stripMargin
    run(input, Paragraph(List(Text("Some text")), Id("some-target")))
  }
  
  
  "The comment parser" should "parse a comment on one line" in {
    val input = """.. This is a comment"""
    run(input, Comment("This is a comment"))
  }
  
  it should "parse a comment on two lines" in {
    val input = """.. This is
      |  a comment""".stripMargin
    run(input, Comment("This is\na comment"))
  }
  
  it should "parse a comment with all text on the second line" in {
    val input = """..  
      |  This is a comment""".stripMargin
    run(input, Comment("This is a comment"))
  }
  
  
}
