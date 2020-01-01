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
import laika.ast.helper.ModelBuilder
import laika.format.Markdown
import laika.markdown.github.GitHubFlavor
import laika.parse.Parser
import laika.parse.helper.{DefaultParserHelpers, ParseResultHelpers}
import laika.parse.markup.RootParser
import org.scalatest.{Matchers, WordSpec}

class GitHubFlavorSpec extends WordSpec
  with Matchers
  with ParseResultHelpers
  with DefaultParserHelpers[RootElement]
  with ModelBuilder {

  val rootParser = new RootParser(Markdown, OperationConfig(Markdown.extensions)
    .withBundles(Seq(GitHubFlavor)).markupExtensions)

  val defaultParser: Parser[RootElement] = rootParser.rootElement

  def headerRow(cells: String*): TableHead =
    TableHead(Seq(Row(cells.map(c => Cell(HeadCell, Seq(Paragraph(Seq(Text(c)))))))))

  def bodyRow(cells: String*): Row =
    Row(cells.map(c => Cell(BodyCell, Seq(Paragraph(Seq(Text(c)))))))

  def paddedBodyRow(count: Int, cells: String*): Row = {
    val cellsWithText = bodyRow(cells:_*).content
    Row(cellsWithText.padTo(count, Cell(BodyCell, Nil)))
  }

  def bodyRowSpans(cells: Seq[Span]*): Row =
    Row(cells.map(c => Cell(BodyCell, Seq(Paragraph(c)))))

  "The Markdown parser with GitHubFlavor extension" should {

    "parse standard Markdown" in {
      val input = """aaa
                    |bbb
                    |
                    |# CCC""".stripMargin
      Parsing (input) should produce (root( p("aaa\nbbb"), h(1, "CCC", "ccc")))
    }

  }

  "The GitHubFlavor strikethrough parser" should {

    def r (spans: Seq[Span]): RootElement = root(p(spans:_*))

    def del (text: String): Span = Deleted(Seq(Text(text)))
    def delS (span: Span): Span = Deleted(Seq(span))

    "parse content enclosed in ~~ at the beginning of a phrase" in {
      Parsing ("~~some~~ text") should produce (r(spans(del("some"),txt(" text"))))
    }

    "parse content enclosed in ~~ at the end of a phrase" in {
      Parsing ("some ~~text~~") should produce (r(spans(txt("some "),del("text"))))
    }

    "parse content enclosed in ~~ in the middle of a phrase" in {
      Parsing ("some ~~text~~ here") should produce (r(spans(txt("some "),del("text"),txt(" here"))))
    }

    "parse content enclosed in ~~ with a nested em span" in {
      Parsing ("some ~~*text*~~ here") should produce (r(spans(txt("some "),delS(em(txt("text"))),txt(" here"))))
    }

    "parse content enclosed in ~~ when it spans the entire phrase" in {
      Parsing ("~~text~~") should produce (r(spans(del("text"))))
    }

    "ignore a ~~ sequence when it is enclosed in spaces" in {
      Parsing ("some ~~ text ~~ here") should produce (r(spans(txt("some ~~ text ~~ here"))))
    }

    "ignore a ~~ sequence when it is not matched by a second ~~" in {
      Parsing ("some ~~text here") should produce (r(spans(txt("some ~~text here"))))
    }

  }

  "The GitHubFlavor auto-link parser" should {

    def r (spans: Seq[Span]): RootElement = root(p(spans:_*))

    "parse a http URI" in {
      val uri = "http://www.link.com"
      Parsing ("some http://www.link.com here") should produce (r(spans(txt("some "),
        link(txt(uri)).url(uri), txt(" here"))))
    }

    "parse a http URI containing an IP4 address" in {
      val uri = "http://127.0.0.1/path"
      Parsing (s"some $uri here") should produce (r(spans(txt("some "),
        link(txt(uri)).url(uri), txt(" here"))))
    }

    "parse a https URI" in {
      val uri = "https://www.link.com"
      Parsing ("some https://www.link.com here") should produce (r(spans(txt("some "),
        link(txt(uri)).url(uri), txt(" here"))))
    }

    "parse a www URI" in {
      val uri = "www.link.com"
      Parsing ("some www.link.com here") should produce (r(spans(txt("some "),
        link(txt(uri)).url(uri), txt(" here"))))
    }

    "parse an email address" in {
      val email = "user@domain.com"
      Parsing ("some user@domain.com here") should produce (r(spans(txt("some "),
        link(txt(email)).url("mailto:"+email), txt(" here"))))
    }

    "parse a http URI without trailing punctuation" in {
      val uri = "http://www.link.com"
      Parsing ("some http://www.link.com. here") should produce (r(spans(txt("some "),
        link(txt(uri)).url(uri), txt(". here"))))
    }

    "parse a www URI without trailing punctuation" in {
      val uri = "www.link.com"
      Parsing ("some www.link.com. here") should produce (r(spans(txt("some "),
        link(txt(uri)).url(uri), txt(". here"))))
    }

    "not parse a URI containing unicode characters" in {
      val text = "some http://www.link.com/fooá here"
      Parsing (text) should produce (r(spans(txt(text))))
    }

    "parse an email address without surrounding punctuation" in {
      val email = "user@domain.com"
      Parsing ("some (user@domain.com) here") should produce (r(spans(txt("some ("),
        link(txt(email)).url("mailto:"+email), txt(") here"))))
    }

  }

  "The GitHubFlavor parser for fenced code blocks" should {

    "parse a code block with backtick fences" in {
      val input =
        """```
          |code
          |```
        """.stripMargin
      Parsing (input) should produce (root(LiteralBlock("code")))
    }

    "parse a code block with tilde fences" in {
      val input =
        """~~~
          |code
          |~~~
        """.stripMargin
      Parsing (input) should produce (root(LiteralBlock("code")))
    }

    "parse a code block with a closing fence that is longer than the opening fence" in {
      val input =
        """~~~
          |code
          |~~~~~
        """.stripMargin
      Parsing (input) should produce (root(LiteralBlock("code")))
    }

    "not recognize a closing fence that is shorter than the opening fence" in {
      val input =
        """~~~~~
          |code
          |~~~
          |~~~~~
        """.stripMargin
      Parsing (input) should produce (root(LiteralBlock("code\n~~~")))
    }

    "not recognize a closing fence that consists of different fence characters" in {
      val input =
        """~~~~~
          |code
          |`````
          |~~~~~
        """.stripMargin
      Parsing (input) should produce (root(LiteralBlock("code\n`````")))
    }

    "parse a code block with an info/language hint" in {
      val input =
        """~~~ foo
          |code
          |~~~
        """.stripMargin
      Parsing (input) should produce (root(CodeBlock("foo", Seq(Text("code")))))
    }

    "parse a code block without a closing fence" in {
      val input =
        """~~~
          |code
        """.stripMargin
      Parsing (input) should produce (root(LiteralBlock("code")))
    }

    "not recognize a fence that is shorter than 3 characters" in {
      val input =
        """~~
          |code
          |~~
        """.stripMargin
      Parsing (input) should produce (root(p("~~\ncode\n~~")))
    }

    "not recognize a fence that is indented more than 3 characters" in {
      val input =
        """    ~~~~
          |    code
          |    ~~~~""".stripMargin
      Parsing (input) should produce (root(LiteralBlock("~~~~\ncode\n~~~~")))
    }

    "not recognize a closing fence with additional characters" in {
      val input =
        """~~~
          |code
          |~~~xxx
        """.stripMargin
      Parsing (input) should produce (root(LiteralBlock("code\n~~~xxx")))
    }

    "parse a code block with an empty line" in {
      val input =
        """```
          |code
          |
          |code
          |```
        """.stripMargin
      Parsing (input) should produce (root(LiteralBlock("code\n\ncode")))
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
      Parsing (input) should produce (root(
        Table(headerRow("AAA","BBB"), TableBody(Seq(bodyRow("CCC","DDD"), bodyRow("EEE","FFF"))))
      ))
    }

    "parse a table with inline markup in one cell" in {
      val input =
        """|| AAA | BBB |
           || --- | --- |
           || CCC | DDD |
           || EEE | FFF *GGG* |
        """.stripMargin
      Parsing (input) should produce (root(
        Table(headerRow("AAA","BBB"), TableBody(Seq(
          bodyRow("CCC","DDD"),
          bodyRowSpans(Seq(Text("EEE")), Seq(Text("FFF "), Emphasized(Seq(Text("GGG")))))
        )))
      ))
    }

    "parse a table head and body with leading and trailing '|' missing in some rows" in {
      val input =
        """|| AAA | BBB |
           || --- | --- |
           || CCC | DDD
           |  EEE | FFF |
        """.stripMargin
      Parsing (input) should produce (root(
        Table(headerRow("AAA","BBB"), TableBody(Seq(bodyRow("CCC","DDD"), bodyRow("EEE","FFF"))))
      ))
    }

    "ignore cells that are exceeding the number of header cells" in {
      val input =
        """|| AAA | BBB |
           || --- | --- |
           || CCC | DDD | XXX |
           |  EEE | FFF |
        """.stripMargin
      Parsing (input) should produce (root(
        Table(headerRow("AAA","BBB"), TableBody(Seq(bodyRow("CCC","DDD"), bodyRow("EEE","FFF"))))
      ))
    }

    "insert empty cells when the row has less cells than the header row" in {
      val input =
        """|| AAA | BBB |
           || --- | --- |
           || CCC |
           |  EEE | FFF |
        """.stripMargin
      Parsing (input) should produce (root(
        Table(headerRow("AAA","BBB"), TableBody(Seq(paddedBodyRow(2, "CCC"), bodyRow("EEE","FFF"))))
      ))
    }

    "parse an escaped '|' as literal text" in {
      val input =
        """|| AAA | BBB |
           || --- | --- |
           || \|  | \|  |
        """.stripMargin
      Parsing (input) should produce (root(
        Table(headerRow("AAA","BBB"), TableBody(Seq(bodyRow("|","|"))))
      ))
    }

    "parse a table that ends on a blank line" in {
      val input =
        """|| AAA | BBB |
           || --- | --- |
           |CCC
           |
           |DDD
        """.stripMargin
      Parsing (input) should produce (root(
        Table(headerRow("AAA","BBB"), TableBody(Seq(paddedBodyRow(2, "CCC")))),
        p("DDD")
      ))
    }

    "parse a table that ends when a new block item starts" in {
      val input =
        """|| AAA | BBB |
           || --- | --- |
           |CCC
           |* DDD
           |* EEE
        """.stripMargin
      Parsing (input) should produce (root(
        Table(headerRow("AAA","BBB"), TableBody(Seq(paddedBodyRow(2, "CCC")))),
        bulletList() + "DDD" + "EEE"
      ))
    }

    "parse a table with alignments" in {
      val input =
        """|| AAA | BBB | CCC |
           || :--- | ---: | :--: |
           || DDD | EEE | FFF |
           || GGG | HHH | III |
        """.stripMargin
      val options = Seq(Styles("align-left"), Styles("align-right"), Styles("align-center"))
      def applyOptions (rows: Seq[Row]): Seq[Row] = rows map { row =>
        Row(row.content.zip(options).map {
          case (cell, opt) => cell.copy(options = opt)
        })
      }
      Parsing (input) should produce (root(Table(
        TableHead(applyOptions(headerRow("AAA","BBB","CCC").content)),
        TableBody(applyOptions(Seq(bodyRow("DDD","EEE","FFF"), bodyRow("GGG","HHH","III")))))
      ))
    }

    "parse a table head without body" in {
      val input =
        """|| AAA | BBB |
           || --- | --- |
        """.stripMargin
      Parsing (input) should produce (root(Table(headerRow("AAA","BBB"), TableBody(Nil))))
    }

    "parse a table head without body without leading '|' in the separator row" in {
      val input =
        """|| AAA | BBB |
           |  --- | --- |
        """.stripMargin
      Parsing (input) should produce (root(Table(headerRow("AAA","BBB"), TableBody(Nil))))
    }

    "parse a table head without body without trailing '|' in the separator row" in {
      val input =
        """|| AAA | BBB |
           || --- | ---
        """.stripMargin
      Parsing (input) should produce (root(Table(headerRow("AAA","BBB"), TableBody(Nil))))
    }

    "not recognize a table head where the number of cells in the separator row does not match the header row" in {
      val input =
        """|| AAA | BBB |
           |  --- |
           |  CCC |""".stripMargin
      Parsing (input) should produce (root(p(input)))
    }

  }

}
