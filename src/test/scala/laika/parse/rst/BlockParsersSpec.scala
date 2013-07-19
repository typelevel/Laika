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
import laika.parse.rst.TextRoles.RoleDirectivePart
import laika.parse.rst.Directives.DirectivePart
    
class BlockParsersSpec extends FlatSpec 
                        with ShouldMatchers 
                        with BlockParsers 
                        with InlineParsers
                        with ParseResultHelpers 
                        with DefaultParserHelpers[Document] 
                        with ModelBuilder {

  
  val defaultParser: Parser[Document] = document
  
  
  def blockDirective (name: String): Option[DirectivePart[Block]] = None
  def spanDirective (name: String): Option[DirectivePart[Span]] = None
  def textRole (name: String): Option[RoleDirectivePart[String => Span]] = None
  
  def ul (char: Char) = Underline(char)
  def ulol (char: Char) = OverlineAndUnderline(char)
  
  
  "The doctest parser" should "parse a doctest block" in {
    val input = """>>> print 'this is a doctest block'
      |this is a doctest block""".stripMargin
    Parsing (input) should produce (doc (DoctestBlock(input.drop(4))))
  }
  
  
  "The literal block parser" should "parse an indented literal block in expanded form" in {
    val input = """Paragraph:
      |
      |::
      |
      |  Literal Block""".stripMargin
    Parsing (input) should produce (doc (p("Paragraph:"), LiteralBlock("Literal Block")))  
  }
  
  it should "parse an indented literal block in partially minimized form" in {
    val input = """Paragraph: ::
      |
      |  Literal Block""".stripMargin
    Parsing (input) should produce (doc (p("Paragraph:"), LiteralBlock("Literal Block")))  
  }
  
  it should "parse an indented literal block in fully minimized form" in {
    val input = """Paragraph::
      |
      |  Literal Block""".stripMargin
    Parsing (input) should produce (doc (p("Paragraph:"), LiteralBlock("Literal Block")))  
  }
  
  it should "parse a quoted literal block in expanded form" in {
    val input = """Paragraph:
      |
      |::
      |
      |> Literal Block""".stripMargin
    Parsing (input) should produce (doc (p("Paragraph:"), LiteralBlock("> Literal Block")))  
  }
  
  it should "parse a quoted literal block in partially minimized form" in {
    val input = """Paragraph: ::
      |
      |> Literal Block""".stripMargin
    Parsing (input) should produce (doc (p("Paragraph:"), LiteralBlock("> Literal Block")))  
  }
  
  it should "parse a quoted literal block in fully minimized form" in {
    val input = """Paragraph::
      |
      |> Literal Block""".stripMargin
    Parsing (input) should produce (doc (p("Paragraph:"), LiteralBlock("> Literal Block")))  
  }
  
  it should "parse an indented literal block with blank lines" in {
    val input = """Paragraph::
      |
      |    Line 1
      |
      |  Line 2""".stripMargin
    Parsing (input) should produce (doc (p("Paragraph:"), LiteralBlock("  Line 1\n\nLine 2")))  
  }
  
  it should "parse a quoted literal block with blank lines" in {
    val input = """Paragraph::
      |
      |>   Line 1
      |>
      |>  Line 2""".stripMargin
    Parsing (input) should produce (doc (p("Paragraph:"), LiteralBlock(">   Line 1\n>\n>  Line 2")))  
  }
  
  
  "The blockquote parser" should "parse block quote with two paragraphs" in {
    val input = """ Paragraph 1
      |
      | Paragraph 2""".stripMargin
    Parsing (input) should produce (doc (quote(p("Paragraph 1"), p("Paragraph 2"))))  
  }
  
  it should "parse block quote with an attribution" in {
    val input = """ Paragraph 1
      |
      | -- attribution""".stripMargin
    Parsing (input) should produce (doc (quote("Paragraph 1", " attribution")))  
  }
  
  
  "The transition parser" should "parse a line of = characters" in {
    val input = """Paragraph
      |
      |=======""".stripMargin
    Parsing (input) should produce (doc (p("Paragraph"), Rule()))
  }
  
  it should "ignore lines shorter than 4 characters" in {
    val input = """Paragraph
      |
      |===""".stripMargin
    Parsing (input) should produce (doc (p("Paragraph"), p("===")))
  }
  
  def ilt (id: String = "header") = InternalLinkTarget(Id(id))
  
  "The header parser" should "parse a header with overline and underline" in {
    val input = """========
      | Header
      |========""".stripMargin
    Parsing (input) should produce (doc (dh(ulol('='),"Header","header")))
  }
  
  it should "parse a header with underline only" in {
    val input = """Header
      |========""".stripMargin
    Parsing (input) should produce (doc (dh(ul('='),"Header","header")))
  }
  
  it should "parse headers with varying levels" in {
    val input = """==========
      | Header 1
      |==========
      |
      |Header 2
      |========
      |
      |Header 3
      |--------
      |
      |Header 2b
      |=========""".stripMargin
    Parsing (input) should produce (doc (dh(ulol('='),"Header 1","header 1"), dh(ul('='),"Header 2","header 2"), 
                                         dh(ul('-'),"Header 3","header 3"), dh(ul('='),"Header 2b","header 2b")))
  }
  
  it should "ignore headers where the underline is shorter than the text" in {
    val input = """Header
      |=====""".stripMargin
    Parsing (input) should produce (doc (p("Header\n=====")))
  }
  
  it should "ignore headers where the underline does not have the same length as the overline" in {
    val input = """=======
      |Header
      |========""".stripMargin
    Parsing (input) should produce (doc (p("=======\nHeader\n========")))
  }
  
  
  "The block list parser" should "apply an internal link target to the following regular block" in {
    val input = """.. _target:
      |
      |Text""".stripMargin
    Parsing (input) should produce (doc (Paragraph(List(Text("Text")), Id("target"))))
  }
  
  it should "not apply an internal link target to the following regular block when that already has an id" in {
    val input = """.. _target:
      |
      |Header
      |======""".stripMargin
    Parsing (input) should produce (doc (InternalLinkTarget(Id("target")), dh(ul('='),"Header","header")))
  }

  it should "treat an internal link target followed by another internal link target like an alias" in {
    val input = """.. _target1:
      |.. _target2:
      |
      |Text""".stripMargin
    Parsing (input) should produce (doc (LinkAlias("target1", "target2"), 
                                         Paragraph(List(Text("Text")), Id("target2"))))
  }
  
  it should "treat an internal link target followed by an external link target as an external link target" in {
    val input = """.. _target1:
      |.. _target2: http://www.foo.com""".stripMargin
    Parsing (input) should produce (doc (ExternalLinkDefinition("target1","http://www.foo.com"), 
                                         ExternalLinkDefinition("target2","http://www.foo.com")))
  }
  
  
}