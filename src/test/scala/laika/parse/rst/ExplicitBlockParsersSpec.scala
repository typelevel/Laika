/*
 * Copyright 2013 the original author or authors.
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

package laika.parse.rst

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import laika.parse.helper.DefaultParserHelpers
import laika.parse.helper.ParseResultHelpers
import laika.tree.Elements.Span
import laika.tree.helper.ModelBuilder
import laika.parse.rst.Elements._
import laika.tree.Elements._
import laika.parse.rst.TextRoles.TextRole
import laika.parse.rst.Directives.DirectivePart
    
class ExplicitBlockParsersSpec extends FlatSpec 
                        with ShouldMatchers 
                        with BlockParsers 
                        with InlineParsers
                        with ParseResultHelpers 
                        with DefaultParserHelpers[Document] 
                        with ModelBuilder {

  
  val defaultParser: Parser[Document] = document
  
  
  val blockDirectives: Map[String, DirectivePart[Block]] = Map.empty
  val spanDirectives: Map[String, DirectivePart[Span]] = Map.empty
  val textRoles: Map[String, TextRole] = Map.empty
  
  
  "The citation parser" should "parse a citation spanning a single line" in {
    val input = """.. [label] This is a citation"""
    Parsing (input) should produce (doc (Citation("label", List(p("This is a citation")))))
  }
  
  it should "parse a citation spanning two lines" in {
    val input = """.. [label] This is a
      |   citation""".stripMargin
    Parsing (input) should produce (doc (Citation("label", List(p("This is a\ncitation")))))
  }
  
  
  "The footnote parser" should "parse a footnote with autonumber label" in {
    Parsing (".. [#] This is a footnote") should produce (doc(Footnote(Autonumber, List(p("This is a footnote")))))
  }
  
  it should "parse a footnote with autosymbol label" in {
    Parsing (".. [*] This is a footnote") should produce (doc(Footnote(Autosymbol, List(p("This is a footnote")))))
  }
  
  it should "parse a footnote with an autonumber named label" in {
    Parsing (".. [#foo] This is a footnote") should produce (doc(Footnote(AutonumberLabel("foo"), List(p("This is a footnote")))))
  }
  
  it should "parse a footnote with a numeric label" in {
    Parsing (".. [17] This is a footnote") should produce (doc(Footnote(NumericLabel(17), List(p("This is a footnote")))))
  }
  
  
  "The link definition parser" should "parse a named external definition on one line" in {
    val input = """.. _some-link: http://www.foo.bar/"""
    Parsing (input) should produce (doc(LinkDefinition("some-link", "http://www.foo.bar/")))
  }
  
  it should "parse a named external definition on two lines" in {
    val input = """.. _some-link: http://www.
      |     foo.bar/""".stripMargin
    Parsing (input) should produce (doc(LinkDefinition("some-link", "http://www.foo.bar/")))
  }
  
  it should "parse a named external definition with the URL entirely on the next line" in {
    val input = """.. _some-link: 
      |     http://www.foo.bar/""".stripMargin
    Parsing (input) should produce (doc(LinkDefinition("some-link", "http://www.foo.bar/")))
  }
  
  it should "parse an anonymous external definition" in {
    val input = """.. __: http://www.foo.bar/"""
    Parsing (input) should produce (doc(LinkDefinition("", "http://www.foo.bar/")))
  }
  
  it should "parse an internal target" in {
    val input = """.. _some-target:"""
    Parsing (input) should produce (doc(InternalLinkTarget("some-target")))
  }
  
  
  "The comment parser" should "parse a comment on one line" in {
    val input = """.. This is a comment"""
    Parsing (input) should produce (doc(Comment("This is a comment")))
  }
  
  it should "parse a comment on two lines" in {
    val input = """.. This is 
      |  a comment""".stripMargin
    Parsing (input) should produce (doc(Comment("This is\na comment")))
  }
  
  it should "parse a comment with all text on the second line" in {
    val input = """..  
      |  This is a comment""".stripMargin
    Parsing (input) should produce (doc(Comment("This is a comment")))
  }
  
  
  
}