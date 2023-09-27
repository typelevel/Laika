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
import laika.ast.*
import laika.ast.sample.ParagraphCompanionShortcuts
import laika.format.Markdown
import laika.internal.parse.markup.RootParser
import laika.parse.Parser
import munit.FunSuite

class GitHubFlavorSpec extends FunSuite with ParagraphCompanionShortcuts {

  val rootParser = new RootParser(
    Markdown,
    new OperationConfig(Markdown.extensions)
      .withBundles(Seq(Markdown.GitHubFlavor)).markupExtensions
  )

  val defaultParser: Parser[RootElement] = rootParser.rootElement

  def headerRow(cells: String*): TableHead =
    TableHead(Seq(Row(cells.map(c => CellType.HeadCell(c)))))

  def bodyRow(cells: String*): Row =
    Row(cells.map(c => CellType.BodyCell(c)))

  def paddedBodyRow(count: Int, cells: String*): Row = {
    val cellsWithText = bodyRow(cells *).content
    Row(cellsWithText.padTo(count, CellType.BodyCell.empty))
  }

  def bodyRowSpans(cells: Seq[Span]*): Row =
    Row(cells.map(c => CellType.BodyCell(Paragraph(c))))

  def runBlocks(input: String, blocks: Block*)(implicit loc: munit.Location): Unit =
    assertEquals(defaultParser.parse(input).toEither, Right(RootElement(blocks)))

  def runSpans(input: String, spans: Span*)(implicit loc: munit.Location): Unit =
    runBlocks(input, Paragraph(spans))

  test("parse standard Markdown") {
    val input = """aaa
                  |bbb
                  |
                  |# CCC""".stripMargin
    runBlocks(input, p("aaa\nbbb"), Header(1, Seq(Text("CCC"))))
  }

  test("strikethrough - enclosed in ~~ at the beginning of a phrase") {
    runSpans("~~some~~ text", Deleted("some"), Text(" text"))
  }

  test("strikethrough - enclosed in ~~ at the end of a phrase") {
    runSpans("some ~~text~~", Text("some "), Deleted("text"))
  }

  test("strikethrough - enclosed in ~~ in the middle of a phrase") {
    runSpans("some ~~text~~ here", Text("some "), Deleted("text"), Text(" here"))
  }

  test("strikethrough - enclosed in ~~ with a nested em span") {
    runSpans("some ~~*text*~~ here", Text("some "), Deleted(Emphasized("text")), Text(" here"))
  }

  test("strikethrough - enclosed in ~~ when it spans the entire phrase") {
    runSpans("~~text~~", Deleted("text"))
  }

  test("strikethrough - ignore a ~~ sequence when it is enclosed in spaces") {
    runSpans("some ~~ text ~~ here", Text("some ~~ text ~~ here"))
  }

  test("strikethrough - ignore a ~~ sequence when it is not matched by a second ~~") {
    runSpans("some ~~text here", Text("some ~~text here"))
  }

  test("auto-links - http URI") {
    val uri = "http://www.link.com"
    runSpans(
      "some http://www.link.com here",
      Text("some "),
      SpanLink.external(uri)(uri),
      Text(" here")
    )
  }

  test("auto-links - http URI containing an IP4 address") {
    val uri = "http://127.0.0.1/path"
    runSpans(s"some $uri here", Text("some "), SpanLink.external(uri)(uri), Text(" here"))
  }

  test("auto-links - https URI") {
    val uri = "https://www.link.com"
    runSpans(
      "some https://www.link.com here",
      Text("some "),
      SpanLink.external(uri)(uri),
      Text(" here")
    )
  }

  test("auto-links - www URI") {
    val uri = "www.link.com"
    runSpans("some www.link.com here", Text("some "), SpanLink.external(uri)(uri), Text(" here"))
  }

  test("auto-links - email address") {
    val email = "user@domain.com"
    runSpans(
      "some user@domain.com here",
      Text("some "),
      SpanLink.external("mailto:" + email)(email),
      Text(" here")
    )
  }

  test("auto-links - http URI without trailing punctuation") {
    val uri = "http://www.link.com"
    runSpans(
      "some http://www.link.com. here",
      Text("some "),
      SpanLink.external(uri)(uri),
      Text(". here")
    )
  }

  test("auto-links - www URI without trailing punctuation") {
    val uri = "www.link.com"
    runSpans("some www.link.com. here", Text("some "), SpanLink.external(uri)(uri), Text(". here"))
  }

  test("auto-links - do not parse a URI containing unicode characters") {
    val text = "some http://www.link.com/fooá here"
    runSpans(text, Text(text))
  }

  test("auto-links - email address without surrounding punctuation") {
    val email = "user@domain.com"
    runSpans(
      "some (user@domain.com) here",
      Text("some ("),
      SpanLink.external("mailto:" + email)(email),
      Text(") here")
    )
  }

  test("code block with backtick fences") {
    val input =
      """```
        |code
        |```
      """.stripMargin
    runBlocks(input, LiteralBlock("code"))
  }

  test("code block with tilde fences") {
    val input =
      """~~~
        |code
        |~~~
      """.stripMargin
    runBlocks(input, LiteralBlock("code"))
  }

  test("code block with a closing fence that is longer than the opening fence") {
    val input =
      """~~~
        |code
        |~~~~~
      """.stripMargin
    runBlocks(input, LiteralBlock("code"))
  }

  test("code blocks - do not recognize a closing fence that is shorter than the opening fence") {
    val input =
      """~~~~~
        |code
        |~~~
        |~~~~~
      """.stripMargin
    runBlocks(input, LiteralBlock("code\n~~~"))
  }

  test(
    "code blocks - do not recognize a closing fence that consists of different fence characters"
  ) {
    val input =
      """~~~~~
        |code
        |`````
        |~~~~~
      """.stripMargin
    runBlocks(input, LiteralBlock("code\n`````"))
  }

  test("code block with an info/language hint") {
    val input =
      """~~~ foo
        |code
        |~~~
      """.stripMargin
    runBlocks(input, CodeBlock("foo", Seq(Text("code"))))
  }

  test("code block that is indented") {
    val input =
      """  ~~~ foo
        |  code
        |    indent
        |  code
        |  ~~~
      """.stripMargin
    runBlocks(input, CodeBlock("foo", Seq(Text("code\n  indent\ncode"))))
  }

  test("code block inside a list item, indented by 4 spaces") {
    val input  =
      """- list item:
        |  
        |    ~~~ foo
        |    code
        |      indent
        |    code
        |    ~~~
      """.stripMargin
    val result = BulletList(BulletFormat.StringBullet("-"))(
      Seq(
        Paragraph("list item:"),
        CodeBlock("foo", Seq(Text("code\n  indent\ncode")))
      )
    )
    runBlocks(input, result)
  }

  test("code block without a closing fence") {
    val input =
      """~~~
        |code
      """.stripMargin
    runBlocks(input, LiteralBlock("code"))
  }

  test("code block - do not recognize a fence that is shorter than 3 characters") {
    val input =
      """~~
        |code
        |~~
      """.stripMargin
    runBlocks(input, p("~~\ncode\n~~"))
  }

  test("code block - do not recognize a fence that is indented more than 3 characters") {
    val input =
      """    ~~~~
        |    code
        |    ~~~~""".stripMargin
    runBlocks(input, LiteralBlock("~~~~\ncode\n~~~~"))
  }

  test("code block - do not recognize a closing fence with additional characters") {
    val input =
      """~~~
        |code
        |~~~xxx
      """.stripMargin
    runBlocks(input, LiteralBlock("code\n~~~xxx"))
  }

  test("code block with an empty line") {
    val input =
      """```
        |code
        |
        |code
        |```
      """.stripMargin
    runBlocks(input, LiteralBlock("code\n\ncode"))
  }

  test("code block without a preceding empty line") {
    val input =
      """aaa
        |```
        |code
        |```
        |bbb
      """.stripMargin
    runBlocks(input, BlockSequence(p("aaa"), LiteralBlock("code")), p("bbb"))
  }

  test("table head and body") {
    val input =
      """|| AAA | BBB |
         || --- | --- |
         || CCC | DDD |
         || EEE | FFF |
      """.stripMargin
    runBlocks(
      input,
      Table(headerRow("AAA", "BBB"), TableBody(Seq(bodyRow("CCC", "DDD"), bodyRow("EEE", "FFF"))))
    )
  }

  test("table with inline markup in one cell") {
    val input =
      """|| AAA | BBB |
         || --- | --- |
         || CCC | DDD |
         || EEE | FFF *GGG* |
      """.stripMargin
    runBlocks(
      input,
      Table(
        headerRow("AAA", "BBB"),
        TableBody(
          Seq(
            bodyRow("CCC", "DDD"),
            bodyRowSpans(Seq(Text("EEE")), Seq(Text("FFF "), Emphasized("GGG")))
          )
        )
      )
    )
  }

  test("table head and body with leading and trailing '|' missing in some rows") {
    val input =
      """|| AAA | BBB |
         || --- | --- |
         || CCC | DDD
         |  EEE | FFF |
      """.stripMargin
    runBlocks(
      input,
      Table(headerRow("AAA", "BBB"), TableBody(Seq(bodyRow("CCC", "DDD"), bodyRow("EEE", "FFF"))))
    )
  }

  test("tables - ignore cells that are exceeding the number of header cells") {
    val input =
      """|| AAA | BBB |
         || --- | --- |
         || CCC | DDD | XXX |
         |  EEE | FFF |
      """.stripMargin
    runBlocks(
      input,
      Table(headerRow("AAA", "BBB"), TableBody(Seq(bodyRow("CCC", "DDD"), bodyRow("EEE", "FFF"))))
    )
  }

  test("tables - insert empty cells when the row has less cells than the header row") {
    val input =
      """|| AAA | BBB |
         || --- | --- |
         || CCC |
         |  EEE | FFF |
      """.stripMargin
    runBlocks(
      input,
      Table(headerRow("AAA", "BBB"), TableBody(Seq(paddedBodyRow(2, "CCC"), bodyRow("EEE", "FFF"))))
    )
  }

  test("table with escaped '|' as literal text") {
    val input =
      """|| AAA | BBB |
         || --- | --- |
         || \|  | \|  |
      """.stripMargin
    runBlocks(input, Table(headerRow("AAA", "BBB"), TableBody(Seq(bodyRow("|", "|")))))
  }

  test("table that ends on a blank line") {
    val input =
      """|| AAA | BBB |
         || --- | --- |
         |CCC
         |
         |DDD
      """.stripMargin
    runBlocks(
      input,
      Table(headerRow("AAA", "BBB"), TableBody(Seq(paddedBodyRow(2, "CCC")))),
      p("DDD")
    )
  }

  test("table that ends when a new block item starts") {
    val input =
      """|| AAA | BBB |
         || --- | --- |
         |CCC
         |* DDD
         |* EEE
      """.stripMargin
    runBlocks(
      input,
      Table(headerRow("AAA", "BBB"), TableBody(Seq(paddedBodyRow(2, "CCC")))),
      BulletList("DDD", "EEE")
    )
  }

  test("table with alignments") {
    val input   =
      """|| AAA | BBB | CCC |
         || :--- | ---: | :--: |
         || DDD | EEE | FFF |
         || GGG | HHH | III |
      """.stripMargin
    val options = Seq(Style.alignLeft, Style.alignRight, Style.alignCenter)
    def applyOptions(rows: Seq[Row]): Seq[Row] = rows map { row =>
      Row(row.content.zip(options).map { case (cell, opt) =>
        cell.withOptions(opt)
      })
    }
    runBlocks(
      input,
      Table(
        TableHead(applyOptions(headerRow("AAA", "BBB", "CCC").content)),
        TableBody(applyOptions(Seq(bodyRow("DDD", "EEE", "FFF"), bodyRow("GGG", "HHH", "III"))))
      )
    )
  }

  test("table head without body") {
    val input =
      """|| AAA | BBB |
         || --- | --- |
      """.stripMargin
    runBlocks(input, Table(headerRow("AAA", "BBB"), TableBody(Nil)))
  }

  test("table head without body without leading '|' in the separator row") {
    val input =
      """|| AAA | BBB |
         |  --- | --- |
      """.stripMargin
    runBlocks(input, Table(headerRow("AAA", "BBB"), TableBody(Nil)))
  }

  test("table head without body without trailing '|' in the separator row") {
    val input =
      """|| AAA | BBB |
         || --- | ---
      """.stripMargin
    runBlocks(input, Table(headerRow("AAA", "BBB"), TableBody(Nil)))
  }

  test(
    "tables - do not recognize a table head where the number of cells in the separator row does not match the header row"
  ) {
    val input =
      """|| AAA | BBB |
         |  --- |
         |  CCC |""".stripMargin
    runBlocks(input, p(input))
  }

}
