package laika.asciidoc

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

import laika.api.builder.OperationConfig
import laika.ast._
import laika.ast.sample.ParagraphCompanionShortcuts
import laika.parse.Parser
import laika.parse.markup.RootParser
import munit.FunSuite
import laika.format.AsciiDoc
    
class BlockParsersSpec extends FunSuite with ParagraphCompanionShortcuts {


  val rootParser = new RootParser(AsciiDoc, OperationConfig().forStrictMode.markupExtensions)

  val defaultParser: Parser[RootElement] = rootParser.rootElement

  def run (input: String, blocks: Block*): Unit =
    assertEquals(defaultParser.parse(input).toEither, Right(RootElement(blocks)))
  
  def fp (content: String): ForcedParagraph = ForcedParagraph(List(Text(content)))


    test("paragraphs - blocks without block-level markup parsed as normal paragraphs") {
    val input = """aaa
      |bbb
      |ccc
      |
      |ddd
      |eee""".stripMargin
    run(input, p("aaa\nbbb\nccc"), p("ddd\neee"))
  }
  
  test("paragraphs - ' +' at a line end parsed as a hard line break") {
    run("some text  +\nsome more", p(Text("some text "), LineBreak(), Text("some more")))
  }


  test("blockquotes - paragraph decorated with '>' at the beginning of each line") {
    val input = """> aaa
      |> bbb
      |> ccc""".stripMargin
    run(input, QuotedBlock("aaabbbccc"))
  }


    
  test("blockquotes - paragraph decorated with '>' only at the beginning of the first line") {
    val input = """> aaa
      |bbb
      |ccc""".stripMargin
    run(input, QuotedBlock("aaabbbccc"))
  }
  
  test("blockquotes - ignores line break") {
    val input = """> aaa
      |> bbb""".stripMargin
    run(input, QuotedBlock("aaabbb"))
  }
  
  
  test("literal blocks - paragraphs indented with 1 or more spaces as a code block") {
    val input = """ code
      |
      |text
      |
      |  code
      |
      |text
      |
      |   code""".stripMargin
    run(input, LiteralBlock("code"), p("text"), LiteralBlock("code"), p("text"), LiteralBlock("code"))
  }

  test("literal blocks - paragraphs indented with 1 or more tabs as a code block") {
    val input = """	code
      |
      |text
      |
      |		code
      |
      |text
      |
      |			code""".stripMargin
    run(input, LiteralBlock("code"), p("text"), LiteralBlock("code"), p("text"), LiteralBlock("code"))
  }
/*

  test("descrete title -  may contain markup") {
    val input = """aaa
      |bbb
      |
      |===== Title *may* `contain` markup""".stripMargin
    run(input, p("aaa\nbbb"), Header(5, Text("Title "), Strong("may"),Literal("contain"), Text(" markup")))
  }


  test("descrete title - must have at least one space after '='") {
    val input = """aaa
      |bbb
      |
      |== AA BB
      |=== CCC DDD EEE""".stripMargin
    run(input, p("aaa\nbbb"), Header(2, "AA BB"),Header(3, "CCC DDD EEE"))
  }

  test("descrete title - trim leading and trailing spaces") {
    val input = """aaa
      |bbb
      |
      |==       AA BB CC     
      |===    CCC DDD EEE       """.stripMargin
    run(input, p("aaa\nbbb"), Header(2, "AA BB CC"),Header(3, "CCC DDD EEE"))
  }

  test("descrete title - keep all trailing '=' in the header if the number of '=' does not match") {
    val input = """aaa
      |bbb
      |
      |== AA BB CC ===
      |=== CCC DDD EEE ====""".stripMargin
    run(input, p("aaa\nbbb"), Header(2, "AA BB CC ==="),Header(3, "CCC DDD EEE ===="))
  }

  test("descrete title - strip all trailing '=' from the header if the number of '=' matches") {
    val input = """aaa
      |bbb
      |
      |=== CCC DDD EEE ===""".stripMargin
    run(input, p("aaa\nbbb"), Header(3, "CCC DDD EEE"))
  }*/

  test("rules - line decorated by exact 3 '-' and optional space characters ending on a '-' without any paragraph") {
    val input = """
      |---
      |
      |- - -
      |""".stripMargin
    run(input, Rule(),Rule())
  }
    test("rules - line decorated by exact 3 '-' and optional space characters ending on a '-' without any paragraph nor blank line") {
    val input = """
      |---
      |- - -
      |""".stripMargin
    run(input, Rule(),Rule())
  }
  test("rules - line decorated by exact 3 '-' and optional space characters ending on a '-'") {
    val input = """aaa
      |bbb
      |
      |---
      |
      |- - -
      |
      |ccc""".stripMargin
    run(input, p("aaa\nbbb"),Rule(),Rule(), p("ccc"))
  }

    test("rules - ' and * behave in the same way as '-'") {
    val input = """aaa
      |bbb
      |
      |- - -
      |* * *
      |' ' '
      |ccc""".stripMargin
    run(input, p("aaa\nbbb"), Rule(),Rule(),Rule(), p("ccc"))
  }

  /* disable leading space for now
  test("rules - line decorated by exact 3 '-' and space characters with leading spaces up to 3") {
    val input = """aaa
      |bbb
      |
      |   - - -
      |
      |ccc""".stripMargin
    run(input, p("aaa\nbbb"), Rule(), p("ccc"))
  }*/

  test("rules - line decorated by exact 3 '-' and space characters with several trailing spaces") {
    val input = """aaa
      |bbb
      |
      |- - -    
      |
      |ccc""".stripMargin
    run(input, p("aaa\nbbb"), Rule(), p("ccc"))
  }
  
  test("rules - line decorated by exact 3 '-' and arbitrary number of space characters between '-'. The number of spaces between '-' must not varies.") {
    val input = """aaa
      |bbb
      |
      |-   -   -
      |
      |ccc""".stripMargin
    run(input, p("aaa\nbbb"), Rule(), p("ccc"))
  }
  
}
