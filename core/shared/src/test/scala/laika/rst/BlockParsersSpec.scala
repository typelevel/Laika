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
import laika.parse.markup.RootParser
import laika.parse.text.TextParsers
import laika.rst.ast.{DoctestBlock, OverlineAndUnderline, Underline}
import munit.FunSuite
    
class BlockParsersSpec extends FunSuite with ParagraphCompanionShortcuts with TestSourceBuilders {


  private val interruptions = BundleProvider.forMarkupParser(
    blockParsers = Seq(
      BlockParser
        .standalone(TextParsers.literal("£££").as(PageBreak()))
        .interruptsParagraphWith(TextParsers.literal("£££"))
    )
  )
  
  val rootParser = new RootParser(ReStructuredText, OperationConfig(ReStructuredText.extensions :+ interruptions).markupExtensions)
  val defaultParser: Parser[RootElement] = rootParser.rootElement
  
  def ul (char: Char): Underline = Underline(char)
  def ulol (char: Char): OverlineAndUnderline = OverlineAndUnderline(char)
  def dh (deco: HeaderDecoration, content: String, fragment: String): DecoratedHeader = dh(deco, content, fragment, fragment)
  def dh (deco: HeaderDecoration, content: String, fragment: String, input: String): DecoratedHeader = 
    DecoratedHeader(deco, List(Text(content)), source(fragment, input))


  def run (input: String, blocks: Block*)(implicit loc: munit.Location): Unit =
    assertEquals(defaultParser.parse(input).toEither, Right(RootElement(blocks)))
  

  test("doctest block") {
    val input = """>>> print 'this is a doctest block'
      |this is a doctest block""".stripMargin
    run(input, DoctestBlock(input.drop(4)))
  }
  
  
  test("indented literal block in expanded form") {
    val input = """Paragraph:
      |
      |::
      |
      |  Literal Block""".stripMargin
    run(input, p("Paragraph:"), LiteralBlock("Literal Block"))  
  }
  
  test("indented literal block in partially minimized form") {
    val input = """Paragraph: ::
      |
      |  Literal Block""".stripMargin
    run(input, p("Paragraph:"), LiteralBlock("Literal Block"))  
  }
  
  test("indented literal block in fully minimized form") {
    val input = """Paragraph::
      |
      |  Literal Block""".stripMargin
    run(input, p("Paragraph:"), LiteralBlock("Literal Block"))  
  }
  
  test("quoted literal block in expanded form") {
    val input = """Paragraph:
      |
      |::
      |
      |> Literal Block""".stripMargin
    run(input, p("Paragraph:"), LiteralBlock("> Literal Block"))  
  }
  
  test("quoted literal block in partially minimized form") {
    val input = """Paragraph: ::
      |
      |> Literal Block""".stripMargin
    run(input, p("Paragraph:"), LiteralBlock("> Literal Block"))  
  }
  
  test("quoted literal block in fully minimized form") {
    val input = """Paragraph::
      |
      |> Literal Block""".stripMargin
    run(input, p("Paragraph:"), LiteralBlock("> Literal Block"))  
  }
  
  test("indented literal block with blank lines") {
    val input = """Paragraph::
      |
      |    Line 1
      |
      |  Line 2""".stripMargin
    run(input, p("Paragraph:"), LiteralBlock("  Line 1\n\nLine 2"))  
  }
  
  test("quoted literal block with blank lines") {
    val input = """Paragraph::
      |
      |>   Line 1
      |>
      |>  Line 2""".stripMargin
    run(input, p("Paragraph:"), LiteralBlock(">   Line 1\n>\n>  Line 2"))  
  }
  
  
  test("block quote with two paragraphs") {
    val input = """ Paragraph 1
      |
      | Paragraph 2""".stripMargin
    run(input, QuotedBlock(p("Paragraph 1"), p("Paragraph 2")))  
  }
  
  test("block quote with an attribution") {
    val input = """ Paragraph 1
      |
      | -- attribution""".stripMargin
    run(input, QuotedBlock(List(p("Paragraph 1")), List(Text("attribution"))))
  }
  
  
  test("transition - line of = characters") {
    val input = """Paragraph
      |
      |=======""".stripMargin
    run(input, p("Paragraph"), Rule())
  }
  
  test("transition - ignore lines shorter than 4 characters") {
    val input = """Paragraph
      |
      |===""".stripMargin
    run(input, p("Paragraph"), p("==="))
  }
  
  def ilt (id: String = "header"): InternalLinkTarget = InternalLinkTarget(Id(id))
  
  test("header with overline and underline") {
    val input = """========
      | Header
      |========""".stripMargin
    run(input, dh(ulol('='),"Header", input))
  }
  
  test("header with underline only") {
    val input = """Header
      |========""".stripMargin
    run(input, dh(ul('='),"Header", input))
  }
  
  test("headers with varying levels") {
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
  
  test("ignore headers where the underline is shorter than the text") {
    val input = """Header
      |=====""".stripMargin
    run(input, p("Header\n====="))
  }
  
  test("ignore headers where the underline does not have the same length as the overline") {
    val input = """=======
      |Header
      |========""".stripMargin
    run(input, p("=======\nHeader\n========"))
  }
  
  
  test("block lists - apply an internal link target to the following regular block") {
    val input = """.. _target:
      |
      |Text""".stripMargin
    run(input, Paragraph(List(Text("Text")), Id("target")))
  }
  
  test("block lists - do not apply an internal link target to the following regular block when that already has an id") {
    val header = """Header
                   |======""".stripMargin
    val input = s""".. _target:
      |
      |$header""".stripMargin
    run(input,
      DecoratedHeader(Underline('='), List(InternalLinkTarget(Id("target")),Text("Header")), source(header, input))
    )
  }

  test("block lists - treat an internal link target followed by another internal link target like an alias") {
    val input = """.. _target1:
      |.. _target2:
      |
      |Text""".stripMargin
    run(input, 
      LinkAlias("target1", "target2"),
      Paragraph(List(Text("Text")), Id("target2"))
    )
  }
  
  test("block lists - treat an internal link target followed by an external link target as an external link target") {
    val input = """.. _target1:
      |.. _target2: http://www.foo.com""".stripMargin
    run(input, 
      LinkDefinition("target1",ExternalTarget("http://www.foo.com")),
      LinkDefinition("target2",ExternalTarget("http://www.foo.com"))
    )
  }

  test("block lists - treat an internal reference followed by an internal link target as two internal link targets") {
    val input = """.. _target1:
                  |.. _target2: ../foo/bar.md#xy""".stripMargin
    val path = RelativePath.parse("../foo/bar.md#xy")
    run(input, 
      LinkDefinition("target1", InternalTarget(path)),
      LinkDefinition("target2", InternalTarget(path))
    )
  }
  
  test("paragraph parser - support a parser extension that can interrupt paragraphs") {
    val input = """line 1
                  |£££
                  |line 2
                  |line 3""".stripMargin
    run(input, BlockSequence(p("line 1"), PageBreak()), p("line 2\nline 3"))
  }
  
}
