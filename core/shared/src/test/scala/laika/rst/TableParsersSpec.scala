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

import laika.ast._
import laika.ast.sample.ParagraphCompanionShortcuts
import laika.bundle.ParserBundle
import laika.format.ReStructuredText
import laika.parse.Parser
import laika.parse.helper.MigrationFlatSpec
import laika.parse.markup.RootParser
import org.scalatest.Assertion
     
class TableParsersSpec extends MigrationFlatSpec with ParagraphCompanionShortcuts {


  val rootParser = new RootParser(ReStructuredText, ParserBundle().markupExtensions)
  val defaultParser: Parser[RootElement] = rootParser.rootElement

  def textRow (cells: String*): Row = Row(cells.map(BodyCell(_)))
  def cell (content: String, colspan: Int, rowspan: Int): Cell = 
    Cell(BodyCell, List(p(Text(content))), colspan, rowspan)

  def run (input: String, blocks: Block*): Assertion =
    assertEquals(defaultParser.parse(input).toEither, Right(RootElement(blocks)))
  
  
  "The grid table parser" should "parse a small table with 2 rows and 2 cells" in {
    val input = """+---+---+
      || a | b |
      |+---+---+
      || c | d |
      |+---+---+""".stripMargin
    run(input, Table(textRow("a","b"), textRow("c","d")))
  }
  
  it should "parse a table with horizontally merged cells in the first row" in {
    val input = """+---+---+
      ||  a b  |
      |+---+---+
      || c | d |
      |+---+---+""".stripMargin
    run(input, Table(Row(cell("a b", 2, 1)), textRow("c","d")))
  }
  
  it should "parse a table with horizontally merged cells in the second row" in {
    val input = """+---+---+
      || a | b |
      |+---+---+
      ||  c d  |
      |+---+---+""".stripMargin
    run(input, Table(textRow("a","b"), Row(cell("c d", 2, 1))))
  }
  
  it should "parse a table with vertically merged cells in the left column" in {
    val input = """+---+---+
      || a | d |
      |+ b +---+
      || c | e |
      |+---+---+""".stripMargin
    run(input, Table(Row(cell("a\nb\nc", 1, 2), BodyCell("d")), textRow("e")))
  }
  
  it should "parse a table with vertically merged cells in the right column" in {
    val input = """+---+---+
      || a | b |
      |+---+ c +
      || e | d |
      |+---+---+""".stripMargin
    run(input, Table(Row(BodyCell("a"), cell("b\nc\nd", 1, 2)), textRow("e")))
  }
  
  it should "parse a table with vertically and horizontally merged cells" in {
    val input = """+---+---+---+
      || a | b | c |
      |+---+---+---+
      ||  1-1  | d |
      ||  2-2  +---+
      ||  3-3  | e |
      |+---+---+---+""".stripMargin
    run(input, Table(
      textRow("a","b","c"),
      Row(cell("1-1\n2-2\n3-3", 2, 2), BodyCell("d")),
      textRow("e")
    ))
  }
  
  it should "parse tables with empty cells" in {
    val input = """+---+---+
      ||   |   |
      |+---+---+
      ||   |   |
      |+---+---+""".stripMargin
    run(input, Table(
      Row(BodyCell.empty,BodyCell.empty),
      Row(BodyCell.empty,BodyCell.empty)
    ))
  }
  
  it should "fail in case of illegal merging of cells (variant 1)" in {
    val input = """+---+---+
      || a | b |
      |+   +---+
      ||  c d  |
      |+---+---+""".stripMargin
    run(input, p(input))
  }
  
  it should "fail in case of illegal merging of cells (variant 2)" in {
    val input = """+---+---+
      || a | b |
      |+---+   +
      ||  c d  |
      |+---+---+""".stripMargin
    run(input, p(input))
  }
  
  it should "fail in case of illegal merging of cells (variant 3)" in {
    val input = """+---+---+
      ||  a b  |
      |+   +---+
      || c | d |
      |+---+---+""".stripMargin
    run(input, p(input))
  }
  
  it should "fail in case of illegal merging of cells (variant 4)" in {
    val input = """+---+---+
      ||  a b  |
      |+---+   +
      || c | d |
      |+---+---+""".stripMargin
    run(input, p(input))
  }
  
  it should "parse cells containing multiple block elements" in {
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
    run(input, Table(
      Row(BodyCell("a"), BodyCell(p("Text"), BulletList("Line1\nLine2", "Line3"))),
      textRow("c","d")
    ))
  }
  
  it should "parse tables with header cells" in {
    val input = """+---+---+
      || a | b |
      |+===+===+
      || c | d |
      |+---+---+""".stripMargin
    run(input, Table(
      TableHead(List(Row(HeadCell("a"), HeadCell("b")))),
      TableBody(List(textRow("c","d")))
    ))
  }
  
  
  
  "The simple table parser" should "parse a small table with 2 rows and 2 cells" in {
    val input = """===  ===
      | a    b
      | c    d
      |===  ===""".stripMargin
    run(input, Table(textRow("a","b"), textRow("c","d")))
  }
  
  it should "parse a table with horizontally merged cells in the first row" in {
    val input = """===  ===
      | a    b
      |--------
      | c    d
      |===  ===""".stripMargin
    run(input, Table(Row(cell("a    b", 2, 1)), textRow("c","d")))
  }
  
  it should "parse a table with horizontally merged cells in the second row" in {
    val input = """===  ===
      | a    b
      | c    d
      |========""".stripMargin
    run(input, Table(textRow("a","b"), Row(cell("c    d", 2, 1))))
  }
  
  it should "parse tables with empty cells" in {
    val input = """===  ===
      | a     
      | c     
      |===  ===""".stripMargin
    run(input, Table(
      Row(BodyCell("a"), BodyCell.empty), 
      Row(BodyCell("c"), BodyCell.empty)
    ))
  }
  
  it should "parse cells containing multiple block elements" in {
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
    run(input, Table(
      Row(BodyCell("a"), BodyCell(p("Text"), BulletList("Line1\nLine2", "Line3"))), 
      textRow("c","d")
    ))
  }
  
  it should "parse tables with header cells" in {
    val input = """===  ===
      | a    b
      |===  ===
      | c    d
      |===  ===""".stripMargin
    run(input, Table(
      TableHead(List(Row(HeadCell("a"), HeadCell("b")))),
      TableBody(List(textRow("c","d")))
    ))
  }
  
  
}
