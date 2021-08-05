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

package laika.markdown

import laika.api.builder.OperationConfig
import laika.ast._
import laika.ast.sample.ParagraphCompanionShortcuts
import laika.format.Markdown
import laika.markdown.github.GitHubFlavor
import laika.parse.Parser
import laika.parse.helper.MigrationSpec
import laika.parse.markup.RootParser
import org.scalatest.Assertion

class GitHubFlavorSpec extends MigrationSpec with ParagraphCompanionShortcuts {

  val rootParser = new RootParser(Markdown, OperationConfig(Markdown.extensions)
    .withBundles(Seq(GitHubFlavor)).markupExtensions)

  val defaultParser: Parser[RootElement] = rootParser.rootElement

  def headerRow(cells: String*): TableHead =
    TableHead(Seq(Row(cells.map(c => HeadCell(c)))))

  def bodyRow(cells: String*): Row =
    Row(cells.map(c => BodyCell(c)))

  def paddedBodyRow(count: Int, cells: String*): Row = {
    val cellsWithText = bodyRow(cells:_*).content
    Row(cellsWithText.padTo(count, BodyCell.empty))
  }

  def bodyRowSpans(cells: Seq[Span]*): Row =
    Row(cells.map(c => BodyCell(Paragraph(c))))

  def runBlocks (input: String, blocks: Block*): Assertion =
    assertEquals(defaultParser.parse(input).toEither, Right(RootElement(blocks)))

  def runSpans (input: String, spans: Span*): Assertion =
    runBlocks(input, Paragraph(spans))

  "The Markdown parser with GitHubFlavor extension" should {

    "parse standard Markdown" in {
      val input = """aaa
                    |bbb
                    |
                    |# CCC""".stripMargin
      runBlocks(input, p("aaa\nbbb"), Header(1, Seq(Text("CCC"))))
    }

  }

  "The GitHubFlavor strikethrough parser" should {

    "parse content enclosed in ~~ at the beginning of a phrase" in {
      runSpans("~~some~~ text", Deleted("some"),Text(" text"))
    }

    "parse content enclosed in ~~ at the end of a phrase" in {
      runSpans("some ~~text~~", Text("some "),Deleted("text"))
    }

    "parse content enclosed in ~~ in the middle of a phrase" in {
      runSpans("some ~~text~~ here", Text("some "),Deleted("text"),Text(" here"))
    }

    "parse content enclosed in ~~ with a nested em span" in {
      runSpans("some ~~*text*~~ here", Text("some "),Deleted(Emphasized("text")),Text(" here"))
    }

    "parse content enclosed in ~~ when it spans the entire phrase" in {
      runSpans("~~text~~", Deleted("text"))
    }

    "ignore a ~~ sequence when it is enclosed in spaces" in {
      runSpans("some ~~ text ~~ here", Text("some ~~ text ~~ here"))
    }

    "ignore a ~~ sequence when it is not matched by a second ~~" in {
      runSpans("some ~~text here", Text("some ~~text here"))
    }

  }

  "The GitHubFlavor auto-link parser" should {

    "parse a http URI" in {
      val uri = "http://www.link.com"
      runSpans("some http://www.link.com here", Text("some "),
        SpanLink.external(uri)(uri), Text(" here"))
    }

    "parse a http URI containing an IP4 address" in {
      val uri = "http://127.0.0.1/path"
      runSpans(s"some $uri here", Text("some "),
        SpanLink.external(uri)(uri), Text(" here"))
    }

    "parse a https URI" in {
      val uri = "https://www.link.com"
      runSpans("some https://www.link.com here", Text("some "),
        SpanLink.external(uri)(uri), Text(" here"))
    }

    "parse a www URI" in {
      val uri = "www.link.com"
      runSpans("some www.link.com here", Text("some "),
        SpanLink.external(uri)(uri), Text(" here"))
    }

    "parse an email address" in {
      val email = "user@domain.com"
      runSpans("some user@domain.com here", Text("some "),
        SpanLink.external("mailto:"+email)(email), Text(" here"))
    }

    "parse a http URI without trailing punctuation" in {
      val uri = "http://www.link.com"
      runSpans("some http://www.link.com. here", Text("some "),
        SpanLink.external(uri)(uri), Text(". here"))
    }

    "parse a www URI without trailing punctuation" in {
      val uri = "www.link.com"
      runSpans("some www.link.com. here", Text("some "),
        SpanLink.external(uri)(uri), Text(". here"))
    }

    "not parse a URI containing unicode characters" in {
      val text = "some http://www.link.com/fooá here"
      runSpans(text, Text(text))
    }

    "parse an email address without surrounding punctuation" in {
      val email = "user@domain.com"
      runSpans("some (user@domain.com) here", Text("some ("),
        SpanLink.external("mailto:"+email)(email), Text(") here"))
    }

  }

  "The GitHubFlavor parser for fenced code blocks" should {

    "parse a code block with backtick fences" in {
      val input =
        """```
          |code
          |```
        """.stripMargin
      runBlocks(input, LiteralBlock("code"))
    }

    "parse a code block with tilde fences" in {
      val input =
        """~~~
          |code
          |~~~
        """.stripMargin
      runBlocks(input, LiteralBlock("code"))
    }

    "parse a code block with a closing fence that is longer than the opening fence" in {
      val input =
        """~~~
          |code
          |~~~~~
        """.stripMargin
      runBlocks(input, LiteralBlock("code"))
    }

    "not recognize a closing fence that is shorter than the opening fence" in {
      val input =
        """~~~~~
          |code
          |~~~
          |~~~~~
        """.stripMargin
      runBlocks(input, LiteralBlock("code\n~~~"))
    }

    "not recognize a closing fence that consists of different fence characters" in {
      val input =
        """~~~~~
          |code
          |`````
          |~~~~~
        """.stripMargin
      runBlocks(input, LiteralBlock("code\n`````"))
    }

    "parse a code block with an info/language hint" in {
      val input =
        """~~~ foo
          |code
          |~~~
        """.stripMargin
      runBlocks(input, CodeBlock("foo", Seq(Text("code"))))
    }

    "parse a code block that is indented" in {
      val input =
        """  ~~~ foo
          |  code
          |    indent
          |  code
          |  ~~~
        """.stripMargin
      runBlocks(input, CodeBlock("foo", Seq(Text("code\n  indent\ncode"))))
    }

    "parse a code block inside a list item, indented by 4 spaces" in {
      val input =
        """- list item:
          |  
          |    ~~~ foo
          |    code
          |      indent
          |    code
          |    ~~~
        """.stripMargin
      val result = BulletList(StringBullet("-"))(Seq(
        Paragraph("list item:"),
        CodeBlock("foo", Seq(Text("code\n  indent\ncode")))
      ))
      runBlocks(input, result)
    }

    "parse a code block without a closing fence" in {
      val input =
        """~~~
          |code
        """.stripMargin
      runBlocks(input, LiteralBlock("code"))
    }

    "not recognize a fence that is shorter than 3 characters" in {
      val input =
        """~~
          |code
          |~~
        """.stripMargin
      runBlocks(input, p("~~\ncode\n~~"))
    }

    "not recognize a fence that is indented more than 3 characters" in {
      val input =
        """    ~~~~
          |    code
          |    ~~~~""".stripMargin
      runBlocks(input, LiteralBlock("~~~~\ncode\n~~~~"))
    }

    "not recognize a closing fence with additional characters" in {
      val input =
        """~~~
          |code
          |~~~xxx
        """.stripMargin
      runBlocks(input, LiteralBlock("code\n~~~xxx"))
    }

    "parse a code block with an empty line" in {
      val input =
        """```
          |code
          |
          |code
          |```
        """.stripMargin
      runBlocks(input, LiteralBlock("code\n\ncode"))
    }

    "parse a code block without a preceding empty line" in {
      val input =
        """aaa
          |```
          |code
          |```
          |bbb
        """.stripMargin
      runBlocks(input, BlockSequence(p("aaa"), LiteralBlock("code")), p("bbb"))
    }

  }

  "The GitHubFlavor table parser" should {

    "parse a table head and body" in {
      val input =
        """|| AAA | BBB |
           || --- | --- |
           || CCC | DDD |
           || EEE | FFF |
        """.stripMargin
      runBlocks(input, 
        Table(headerRow("AAA","BBB"), TableBody(Seq(bodyRow("CCC","DDD"), bodyRow("EEE","FFF"))))
      )
    }

    "parse a table with inline markup in one cell" in {
      val input =
        """|| AAA | BBB |
           || --- | --- |
           || CCC | DDD |
           || EEE | FFF *GGG* |
        """.stripMargin
      runBlocks(input, 
        Table(headerRow("AAA","BBB"), TableBody(Seq(
          bodyRow("CCC","DDD"),
          bodyRowSpans(Seq(Text("EEE")), Seq(Text("FFF "), Emphasized("GGG")))
        )))
      )
    }

    "parse a table head and body with leading and trailing '|' missing in some rows" in {
      val input =
        """|| AAA | BBB |
           || --- | --- |
           || CCC | DDD
           |  EEE | FFF |
        """.stripMargin
      runBlocks(input, 
        Table(headerRow("AAA","BBB"), TableBody(Seq(bodyRow("CCC","DDD"), bodyRow("EEE","FFF"))))
      )
    }

    "ignore cells that are exceeding the number of header cells" in {
      val input =
        """|| AAA | BBB |
           || --- | --- |
           || CCC | DDD | XXX |
           |  EEE | FFF |
        """.stripMargin
      runBlocks(input, 
        Table(headerRow("AAA","BBB"), TableBody(Seq(bodyRow("CCC","DDD"), bodyRow("EEE","FFF"))))
      )
    }

    "insert empty cells when the row has less cells than the header row" in {
      val input =
        """|| AAA | BBB |
           || --- | --- |
           || CCC |
           |  EEE | FFF |
        """.stripMargin
      runBlocks(input, 
        Table(headerRow("AAA","BBB"), TableBody(Seq(paddedBodyRow(2, "CCC"), bodyRow("EEE","FFF"))))
      )
    }

    "parse an escaped '|' as literal text" in {
      val input =
        """|| AAA | BBB |
           || --- | --- |
           || \|  | \|  |
        """.stripMargin
      runBlocks(input, 
        Table(headerRow("AAA","BBB"), TableBody(Seq(bodyRow("|","|"))))
      )
    }

    "parse a table that ends on a blank line" in {
      val input =
        """|| AAA | BBB |
           || --- | --- |
           |CCC
           |
           |DDD
        """.stripMargin
      runBlocks(input, 
        Table(headerRow("AAA","BBB"), TableBody(Seq(paddedBodyRow(2, "CCC")))),
        p("DDD")
      )
    }

    "parse a table that ends when a new block item starts" in {
      val input =
        """|| AAA | BBB |
           || --- | --- |
           |CCC
           |* DDD
           |* EEE
        """.stripMargin
      runBlocks(input, 
        Table(headerRow("AAA","BBB"), TableBody(Seq(paddedBodyRow(2, "CCC")))),
        BulletList("DDD", "EEE")
      )
    }

    "parse a table with alignments" in {
      val input =
        """|| AAA | BBB | CCC |
           || :--- | ---: | :--: |
           || DDD | EEE | FFF |
           || GGG | HHH | III |
        """.stripMargin
      val options = Seq(Style.alignLeft, Style.alignRight, Style.alignCenter)
      def applyOptions (rows: Seq[Row]): Seq[Row] = rows map { row =>
        Row(row.content.zip(options).map {
          case (cell, opt) => cell.withOptions(opt)
        })
      }
      runBlocks(input, Table(
        TableHead(applyOptions(headerRow("AAA","BBB","CCC").content)),
        TableBody(applyOptions(Seq(bodyRow("DDD","EEE","FFF"), bodyRow("GGG","HHH","III")))))
      )
    }

    "parse a table head without body" in {
      val input =
        """|| AAA | BBB |
           || --- | --- |
        """.stripMargin
      runBlocks(input, Table(headerRow("AAA","BBB"), TableBody(Nil)))
    }

    "parse a table head without body without leading '|' in the separator row" in {
      val input =
        """|| AAA | BBB |
           |  --- | --- |
        """.stripMargin
      runBlocks(input, Table(headerRow("AAA","BBB"), TableBody(Nil)))
    }

    "parse a table head without body without trailing '|' in the separator row" in {
      val input =
        """|| AAA | BBB |
           || --- | ---
        """.stripMargin
      runBlocks(input, Table(headerRow("AAA","BBB"), TableBody(Nil)))
    }

    "not recognize a table head where the number of cells in the separator row does not match the header row" in {
      val input =
        """|| AAA | BBB |
           |  --- |
           |  CCC |""".stripMargin
      runBlocks(input, p(input))
    }

  }

}
