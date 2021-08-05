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
import laika.bundle.{BlockParser, BundleProvider}
import laika.format.ReStructuredText
import laika.parse.Parser
import laika.parse.helper.MigrationFlatSpec
import laika.parse.markup.RootParser
import laika.parse.text.TextParsers
import laika.rst.ast.{DoctestBlock, OverlineAndUnderline, Underline}
import laika.rst.ext.Directives.DirectivePart
import laika.rst.ext.TextRoles.RoleDirectivePart
import org.scalatest.Assertion
    
class BlockParsersSpec extends MigrationFlatSpec with ParagraphCompanionShortcuts with TestSourceBuilders {


  val interruptions = BundleProvider.forMarkupParser(
    blockParsers = Seq(
      BlockParser
        .standalone(TextParsers.literal("£££").as(PageBreak()))
        .interruptsParagraphWith(TextParsers.literal("£££"))
    )
  )
  
  val rootParser = new RootParser(ReStructuredText, OperationConfig(ReStructuredText.extensions :+ interruptions).markupExtensions)
  val defaultParser: Parser[RootElement] = rootParser.rootElement
  
  
  def blockDirective (name: String): Option[DirectivePart[Block]] = None
  def spanDirective (name: String): Option[DirectivePart[Span]] = None
  def textRole (name: String): Option[RoleDirectivePart[String => Span]] = None
  
  def ul (char: Char) = Underline(char)
  def ulol (char: Char) = OverlineAndUnderline(char)
  def dh (deco: HeaderDecoration, content: String, fragment: String): DecoratedHeader = dh(deco, content, fragment, fragment)
  def dh (deco: HeaderDecoration, content: String, fragment: String, input: String): DecoratedHeader = 
    DecoratedHeader(deco, List(Text(content)), source(fragment, input))


  def run (input: String, blocks: Block*): Assertion =
    assertEquals(defaultParser.parse(input).toEither, Right(RootElement(blocks)))
  

  "The doctest parser" should "parse a doctest block" in {
    val input = """>>> print 'this is a doctest block'
      |this is a doctest block""".stripMargin
    run(input, DoctestBlock(input.drop(4)))
  }
  
  
  "The literal block parser" should "parse an indented literal block in expanded form" in {
    val input = """Paragraph:
      |
      |::
      |
      |  Literal Block""".stripMargin
    run(input, p("Paragraph:"), LiteralBlock("Literal Block"))  
  }
  
  it should "parse an indented literal block in partially minimized form" in {
    val input = """Paragraph: ::
      |
      |  Literal Block""".stripMargin
    run(input, p("Paragraph:"), LiteralBlock("Literal Block"))  
  }
  
  it should "parse an indented literal block in fully minimized form" in {
    val input = """Paragraph::
      |
      |  Literal Block""".stripMargin
    run(input, p("Paragraph:"), LiteralBlock("Literal Block"))  
  }
  
  it should "parse a quoted literal block in expanded form" in {
    val input = """Paragraph:
      |
      |::
      |
      |> Literal Block""".stripMargin
    run(input, p("Paragraph:"), LiteralBlock("> Literal Block"))  
  }
  
  it should "parse a quoted literal block in partially minimized form" in {
    val input = """Paragraph: ::
      |
      |> Literal Block""".stripMargin
    run(input, p("Paragraph:"), LiteralBlock("> Literal Block"))  
  }
  
  it should "parse a quoted literal block in fully minimized form" in {
    val input = """Paragraph::
      |
      |> Literal Block""".stripMargin
    run(input, p("Paragraph:"), LiteralBlock("> Literal Block"))  
  }
  
  it should "parse an indented literal block with blank lines" in {
    val input = """Paragraph::
      |
      |    Line 1
      |
      |  Line 2""".stripMargin
    run(input, p("Paragraph:"), LiteralBlock("  Line 1\n\nLine 2"))  
  }
  
  it should "parse a quoted literal block with blank lines" in {
    val input = """Paragraph::
      |
      |>   Line 1
      |>
      |>  Line 2""".stripMargin
    run(input, p("Paragraph:"), LiteralBlock(">   Line 1\n>\n>  Line 2"))  
  }
  
  
  "The blockquote parser" should "parse block quote with two paragraphs" in {
    val input = """ Paragraph 1
      |
      | Paragraph 2""".stripMargin
    run(input, QuotedBlock(p("Paragraph 1"), p("Paragraph 2")))  
  }
  
  it should "parse block quote with an attribution" in {
    val input = """ Paragraph 1
      |
      | -- attribution""".stripMargin
    run(input, QuotedBlock(List(p("Paragraph 1")), List(Text("attribution"))))
  }
  
  
  "The transition parser" should "parse a line of = characters" in {
    val input = """Paragraph
      |
      |=======""".stripMargin
    run(input, p("Paragraph"), Rule())
  }
  
  it should "ignore lines shorter than 4 characters" in {
    val input = """Paragraph
      |
      |===""".stripMargin
    run(input, p("Paragraph"), p("==="))
  }
  
  def ilt (id: String = "header") = InternalLinkTarget(Id(id))
  
  "The header parser" should "parse a header with overline and underline" in {
    val input = """========
      | Header
      |========""".stripMargin
    run(input, dh(ulol('='),"Header", input))
  }
  
  it should "parse a header with underline only" in {
    val input = """Header
      |========""".stripMargin
    run(input, dh(ul('='),"Header", input))
  }
  
  it should "parse headers with varying levels" in {
    val h1 = """==========
               | Header 1
               |==========""".stripMargin
    val h2 = """Header 2
               |========""".stripMargin
    val h3 = """Header 3
               |--------""".stripMargin
    val h4 = """Header 2b
               |=========""".stripMargin
    val input = 
      s"""$h1
      |
      |$h2
      |
      |$h3
      |
      |$h4""".stripMargin
    run(input,
      dh(ulol('='),"Header 1", h1, input), 
      dh(ul('='),"Header 2", h2, input), 
      dh(ul('-'),"Header 3", h3, input), 
      dh(ul('='),"Header 2b", h4, input)
    )
  }
  
  it should "ignore headers where the underline is shorter than the text" in {
    val input = """Header
      |=====""".stripMargin
    run(input, p("Header\n====="))
  }
  
  it should "ignore headers where the underline does not have the same length as the overline" in {
    val input = """=======
      |Header
      |========""".stripMargin
    run(input, p("=======\nHeader\n========"))
  }
  
  
  "The block list parser" should "apply an internal link target to the following regular block" in {
    val input = """.. _target:
      |
      |Text""".stripMargin
    run(input, Paragraph(List(Text("Text")), Id("target")))
  }
  
  it should "not apply an internal link target to the following regular block when that already has an id" in {
    val header = """Header
                   |======""".stripMargin
    val input = s""".. _target:
      |
      |$header""".stripMargin
    run(input,
      DecoratedHeader(Underline('='), List(InternalLinkTarget(Id("target")),Text("Header")), source(header, input))
    )
  }

  it should "treat an internal link target followed by another internal link target like an alias" in {
    val input = """.. _target1:
      |.. _target2:
      |
      |Text""".stripMargin
    run(input, 
      LinkAlias("target1", "target2"),
      Paragraph(List(Text("Text")), Id("target2"))
    )
  }
  
  it should "treat an internal link target followed by an external link target as an external link target" in {
    val input = """.. _target1:
      |.. _target2: http://www.foo.com""".stripMargin
    run(input, 
      LinkDefinition("target1",ExternalTarget("http://www.foo.com")),
      LinkDefinition("target2",ExternalTarget("http://www.foo.com"))
    )
  }

  it should "treat an internal reference followed by an internal link target as two internal link targets" in {
    val input = """.. _target1:
                  |.. _target2: ../foo/bar.md#xy""".stripMargin
    val path = RelativePath.parse("../foo/bar.md#xy")
    run(input, 
      LinkDefinition("target1", InternalTarget(path)),
      LinkDefinition("target2", InternalTarget(path))
    )
  }
  
  "The paragraph parser" should "support a parser extension that can interrupt paragraphs" in {
    val input = """line 1
                  |£££
                  |line 2
                  |line 3""".stripMargin
    run(input, BlockSequence(p("line 1"), PageBreak()), p("line 2\nline 3"))
  }
  
}
