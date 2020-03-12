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
import laika.ast.helper.ModelBuilder
import laika.format.ReStructuredText
import laika.parse.Parser
import laika.parse.helper.{DefaultParserHelpers, ParseResultHelpers}
import laika.parse.markup.RootParser
import laika.rst.ast.{DoctestBlock, OverlineAndUnderline, Underline}
import laika.rst.ext.Directives.DirectivePart
import laika.rst.ext.TextRoles.RoleDirectivePart
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
    
class BlockParsersSpec extends AnyFlatSpec 
                        with Matchers 
                        with ParseResultHelpers
                        with DefaultParserHelpers[RootElement] 
                        with ModelBuilder {


  val rootParser = new RootParser(ReStructuredText, OperationConfig(ReStructuredText.extensions).markupExtensions)
  val defaultParser: Parser[RootElement] = rootParser.rootElement
  
  
  def blockDirective (name: String): Option[DirectivePart[Block]] = None
  def spanDirective (name: String): Option[DirectivePart[Span]] = None
  def textRole (name: String): Option[RoleDirectivePart[String => Span]] = None
  
  def ul (char: Char) = Underline(char)
  def ulol (char: Char) = OverlineAndUnderline(char)
  def dh (deco: HeaderDecoration, content: String, id: String) = DecoratedHeader(deco, List(Text(content)), Id(id))


  "The doctest parser" should "parse a doctest block" in {
    val input = """>>> print 'this is a doctest block'
      |this is a doctest block""".stripMargin
    Parsing (input) should produce (root (DoctestBlock(input.drop(4))))
  }
  
  
  "The literal block parser" should "parse an indented literal block in expanded form" in {
    val input = """Paragraph:
      |
      |::
      |
      |  Literal Block""".stripMargin
    Parsing (input) should produce (root (p("Paragraph:"), LiteralBlock("Literal Block")))  
  }
  
  it should "parse an indented literal block in partially minimized form" in {
    val input = """Paragraph: ::
      |
      |  Literal Block""".stripMargin
    Parsing (input) should produce (root (p("Paragraph:"), LiteralBlock("Literal Block")))  
  }
  
  it should "parse an indented literal block in fully minimized form" in {
    val input = """Paragraph::
      |
      |  Literal Block""".stripMargin
    Parsing (input) should produce (root (p("Paragraph:"), LiteralBlock("Literal Block")))  
  }
  
  it should "parse a quoted literal block in expanded form" in {
    val input = """Paragraph:
      |
      |::
      |
      |> Literal Block""".stripMargin
    Parsing (input) should produce (root (p("Paragraph:"), LiteralBlock("> Literal Block")))  
  }
  
  it should "parse a quoted literal block in partially minimized form" in {
    val input = """Paragraph: ::
      |
      |> Literal Block""".stripMargin
    Parsing (input) should produce (root (p("Paragraph:"), LiteralBlock("> Literal Block")))  
  }
  
  it should "parse a quoted literal block in fully minimized form" in {
    val input = """Paragraph::
      |
      |> Literal Block""".stripMargin
    Parsing (input) should produce (root (p("Paragraph:"), LiteralBlock("> Literal Block")))  
  }
  
  it should "parse an indented literal block with blank lines" in {
    val input = """Paragraph::
      |
      |    Line 1
      |
      |  Line 2""".stripMargin
    Parsing (input) should produce (root (p("Paragraph:"), LiteralBlock("  Line 1\n\nLine 2")))  
  }
  
  it should "parse a quoted literal block with blank lines" in {
    val input = """Paragraph::
      |
      |>   Line 1
      |>
      |>  Line 2""".stripMargin
    Parsing (input) should produce (root (p("Paragraph:"), LiteralBlock(">   Line 1\n>\n>  Line 2")))  
  }
  
  
  "The blockquote parser" should "parse block quote with two paragraphs" in {
    val input = """ Paragraph 1
      |
      | Paragraph 2""".stripMargin
    Parsing (input) should produce (root (QuotedBlock(p("Paragraph 1"), p("Paragraph 2"))))  
  }
  
  it should "parse block quote with an attribution" in {
    val input = """ Paragraph 1
      |
      | -- attribution""".stripMargin
    Parsing (input) should produce (root (quote("Paragraph 1", "attribution")))  
  }
  
  
  "The transition parser" should "parse a line of = characters" in {
    val input = """Paragraph
      |
      |=======""".stripMargin
    Parsing (input) should produce (root (p("Paragraph"), Rule()))
  }
  
  it should "ignore lines shorter than 4 characters" in {
    val input = """Paragraph
      |
      |===""".stripMargin
    Parsing (input) should produce (root (p("Paragraph"), p("===")))
  }
  
  def ilt (id: String = "header") = InternalLinkTarget(Id(id))
  
  "The header parser" should "parse a header with overline and underline" in {
    val input = """========
      | Header
      |========""".stripMargin
    Parsing (input) should produce (root (dh(ulol('='),"Header","header")))
  }
  
  it should "parse a header with underline only" in {
    val input = """Header
      |========""".stripMargin
    Parsing (input) should produce (root (dh(ul('='),"Header","header")))
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
    Parsing (input) should produce (root (dh(ulol('='),"Header 1","header 1"), dh(ul('='),"Header 2","header 2"), 
                                         dh(ul('-'),"Header 3","header 3"), dh(ul('='),"Header 2b","header 2b")))
  }
  
  it should "ignore headers where the underline is shorter than the text" in {
    val input = """Header
      |=====""".stripMargin
    Parsing (input) should produce (root (p("Header\n=====")))
  }
  
  it should "ignore headers where the underline does not have the same length as the overline" in {
    val input = """=======
      |Header
      |========""".stripMargin
    Parsing (input) should produce (root (p("=======\nHeader\n========")))
  }
  
  
  "The block list parser" should "apply an internal link target to the following regular block" in {
    val input = """.. _target:
      |
      |Text""".stripMargin
    Parsing (input) should produce (root (Paragraph(List(Text("Text")), Id("target"))))
  }
  
  it should "not apply an internal link target to the following regular block when that already has an id" in {
    val input = """.. _target:
      |
      |Header
      |======""".stripMargin
    Parsing (input) should produce (root(DecoratedHeader(Underline('='), List(InternalLinkTarget(Id("target")),Text("Header")), Id("header"))))  
  }

  it should "treat an internal link target followed by another internal link target like an alias" in {
    val input = """.. _target1:
      |.. _target2:
      |
      |Text""".stripMargin
    Parsing (input) should produce (root (LinkAlias("target1", "target2"), 
                                         Paragraph(List(Text("Text")), Id("target2"))))
  }
  
  it should "treat an internal link target followed by an external link target as an external link target" in {
    val input = """.. _target1:
      |.. _target2: http://www.foo.com""".stripMargin
    Parsing (input) should produce (root (ExternalLinkDefinition("target1","http://www.foo.com"), 
                                         ExternalLinkDefinition("target2","http://www.foo.com")))
  }

  it should "treat an internal reference followed by an internal link target as two internal link targets" in {
    val input = """.. _target1:
                  |.. _target2: ../foo/bar.md#xy""".stripMargin
    val path = RelativePath.parse("../foo/bar.md#xy")
    Parsing (input) should produce (root (InternalLinkDefinition("target1", path),
      InternalLinkDefinition("target2", path)))
  }
  
  
}
