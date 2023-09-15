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

import laika.api.bundle.ParserBundle
import laika.ast.*
import laika.ast.sample.ParagraphCompanionShortcuts
import laika.format.ReStructuredText
import laika.parse.Parser
import laika.parse.markup.RootParser
import munit.FunSuite

class TableParsersSpec extends FunSuite with ParagraphCompanionShortcuts {

  val rootParser = new RootParser(ReStructuredText, new ParserBundle().markupExtensions)
  val defaultParser: Parser[RootElement] = rootParser.rootElement

  def textRow(cells: String*): Row = Row(cells.map(BodyCell(_)))

  def cell(content: String, colspan: Int, rowspan: Int): Cell =
    Cell(BodyCell, List(p(Text(content))), colspan, rowspan)

  def run(input: String, blocks: Block*)(implicit loc: munit.Location): Unit =
    assertEquals(defaultParser.parse(input).toEither, Right(RootElement(blocks)))

  test("grid table - 2 rows and 2 cells") {
    val input = """+---+---+
                  || a | b |
                  |+---+---+
                  || c | d |
                  |+---+---+""".stripMargin
    run(input, Table(textRow("a", "b"), textRow("c", "d")))
  }

  test("grid table - horizontally merged cells in the first row") {
    val input = """+---+---+
                  ||  a b  |
                  |+---+---+
                  || c | d |
                  |+---+---+""".stripMargin
    run(input, Table(Row(cell("a b", 2, 1)), textRow("c", "d")))
  }

  test("grid table - horizontally merged cells in the second row") {
    val input = """+---+---+
                  || a | b |
                  |+---+---+
                  ||  c d  |
                  |+---+---+""".stripMargin
    run(input, Table(textRow("a", "b"), Row(cell("c d", 2, 1))))
  }

  test("grid table - vertically merged cells in the left column") {
    val input = """+---+---+
                  || a | d |
                  |+ b +---+
                  || c | e |
                  |+---+---+""".stripMargin
    run(input, Table(Row(cell("a\nb\nc", 1, 2), BodyCell("d")), textRow("e")))
  }

  test("grid table - vertically merged cells in the right column") {
    val input = """+---+---+
                  || a | b |
                  |+---+ c +
                  || e | d |
                  |+---+---+""".stripMargin
    run(input, Table(Row(BodyCell("a"), cell("b\nc\nd", 1, 2)), textRow("e")))
  }

  test("grid table - vertically and horizontally merged cells") {
    val input = """+---+---+---+
                  || a | b | c |
                  |+---+---+---+
                  ||  1-1  | d |
                  ||  2-2  +---+
                  ||  3-3  | e |
                  |+---+---+---+""".stripMargin
    run(
      input,
      Table(
        textRow("a", "b", "c"),
        Row(cell("1-1\n2-2\n3-3", 2, 2), BodyCell("d")),
        textRow("e")
      )
    )
  }

  test("grid table - empty cells") {
    val input = """+---+---+
                  ||   |   |
                  |+---+---+
                  ||   |   |
                  |+---+---+""".stripMargin
    run(
      input,
      Table(
        Row(BodyCell.empty, BodyCell.empty),
        Row(BodyCell.empty, BodyCell.empty)
      )
    )
  }

  test("grid table - fail in case of illegal merging of cells (variant 1)") {
    val input = """+---+---+
                  || a | b |
                  |+   +---+
                  ||  c d  |
                  |+---+---+""".stripMargin
    run(input, p(input))
  }

  test("grid table - fail in case of illegal merging of cells (variant 2)") {
    val input = """+---+---+
                  || a | b |
                  |+---+   +
                  ||  c d  |
                  |+---+---+""".stripMargin
    run(input, p(input))
  }

  test("grid table - fail in case of illegal merging of cells (variant 3)") {
    val input = """+---+---+
                  ||  a b  |
                  |+   +---+
                  || c | d |
                  |+---+---+""".stripMargin
    run(input, p(input))
  }

  test("grid table - fail in case of illegal merging of cells (variant 4)") {
    val input = """+---+---+
                  ||  a b  |
                  |+---+   +
                  || c | d |
                  |+---+---+""".stripMargin
    run(input, p(input))
  }

  test("grid table - parse cells containing multiple block elements") {
    val input = """+---+---------+
                  || a | Text    |
                  ||   |         |
                  ||   | * Line1 |
                  ||   |   Line2 |
                  ||   |         |
                  ||   | * Line3 |
                  |+---+---------+
                  || c | d       |
                  |+---+---------+""".stripMargin
    run(
      input,
      Table(
        Row(BodyCell("a"), BodyCell(p("Text"), BulletList("Line1\nLine2", "Line3"))),
        textRow("c", "d")
      )
    )
  }

  test("grid table - parse tables with header cells") {
    val input = """+---+---+
                  || a | b |
                  |+===+===+
                  || c | d |
                  |+---+---+""".stripMargin
    run(
      input,
      Table(
        TableHead(List(Row(HeadCell("a"), HeadCell("b")))),
        TableBody(List(textRow("c", "d")))
      )
    )
  }

  test("simple table - 2 rows and 2 cells") {
    val input = """===  ===
                  | a    b
                  | c    d
                  |===  ===""".stripMargin
    run(input, Table(textRow("a", "b"), textRow("c", "d")))
  }

  test("simple table - horizontally merged cells in the first row") {
    val input = """===  ===
                  | a    b
                  |--------
                  | c    d
                  |===  ===""".stripMargin
    run(input, Table(Row(cell("a    b", 2, 1)), textRow("c", "d")))
  }

  test("simple table - horizontally merged cells in the second row") {
    val input = """===  ===
                  | a    b
                  | c    d
                  |========""".stripMargin
    run(input, Table(textRow("a", "b"), Row(cell("c    d", 2, 1))))
  }

  test("simple table - empty cells") {
    val input = """===  ===
                  | a     
                  | c     
                  |===  ===""".stripMargin
    run(
      input,
      Table(
        Row(BodyCell("a"), BodyCell.empty),
        Row(BodyCell("c"), BodyCell.empty)
      )
    )
  }

  test("simple table - cells containing multiple block elements") {
    val input = """===  ===
                  | a    Text
                  |
                  |      * Line1
                  |        Line2
                  |
                  |      * Line3
                  |
                  | c    d
                  |===  ===""".stripMargin
    run(
      input,
      Table(
        Row(BodyCell("a"), BodyCell(p("Text"), BulletList("Line1\nLine2", "Line3"))),
        textRow("c", "d")
      )
    )
  }

  test("simple table - header cells") {
    val input = """===  ===
                  | a    b
                  |===  ===
                  | c    d
                  |===  ===""".stripMargin
    run(
      input,
      Table(
        TableHead(List(Row(HeadCell("a"), HeadCell("b")))),
        TableBody(List(textRow("c", "d")))
      )
    )
  }

}
