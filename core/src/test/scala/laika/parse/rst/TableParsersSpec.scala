/*
 * Copyright 2013-2016 the original author or authors.
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

import laika.parse.core.Parser
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers
import laika.parse.helper.DefaultParserHelpers
import laika.parse.helper.ParseResultHelpers
import laika.tree.Elements.Span
import laika.tree.helper.ModelBuilder
import laika.parse.rst.Elements._
import laika.tree.Elements._
import laika.parse.rst.ext.TextRoles.RoleDirectivePart
import laika.parse.rst.ext.Directives.DirectivePart
     
class TableParsersSpec extends FlatSpec 
                        with Matchers 
                        with ParseResultHelpers
                        with DefaultParserHelpers[RootElement] 
                        with ModelBuilder {


  val rootParser = new RootParser
  val defaultParser: Parser[RootElement] = rootParser.rootElement

  
  def blockDirective (name: String): Option[DirectivePart[Block]] = None
  def spanDirective (name: String): Option[DirectivePart[Span]] = None
  def textRole (name: String): Option[RoleDirectivePart[String => Span]] = None
  
  
  
  "The grid table parser" should "parse a small table with 2 rows and 2 cells" in {
    val input = """+---+---+
      || a | b |
      |+---+---+
      || c | d |
      |+---+---+""".stripMargin
    Parsing (input) should produce (root( table(strrow("a","b"), strrow("c","d"))))
  }
  
  it should "parse a table with horizontally merged cells in the first row" in {
    val input = """+---+---+
      ||  a b  |
      |+---+---+
      || c | d |
      |+---+---+""".stripMargin
    Parsing (input) should produce (root( table(row(cell("a b", 2, 1)), strrow("c","d"))))
  }
  
  it should "parse a table with horizontally merged cells in the second row" in {
    val input = """+---+---+
      || a | b |
      |+---+---+
      ||  c d  |
      |+---+---+""".stripMargin
    Parsing (input) should produce (root( table(strrow("a","b"), row(cell("c d", 2, 1)))))
  }
  
  it should "parse a table with vertically merged cells in the left column" in {
    val input = """+---+---+
      || a | d |
      |+ b +---+
      || c | e |
      |+---+---+""".stripMargin
    Parsing (input) should produce (root( table(row(cell("a\nb\nc", 1, 2), cell("d")), strrow("e"))))
  }
  
  it should "parse a table with vertically merged cells in the right column" in {
    val input = """+---+---+
      || a | b |
      |+---+ c +
      || e | d |
      |+---+---+""".stripMargin
    Parsing (input) should produce (root( table(row(cell("a"), cell("b\nc\nd", 1, 2)), strrow("e"))))
  }
  
  it should "parse a table with vertically and horizontally merged cells" in {
    val input = """+---+---+---+
      || a | b | c |
      |+---+---+---+
      ||  1-1  | d |
      ||  2-2  +---+
      ||  3-3  | e |
      |+---+---+---+""".stripMargin
    Parsing (input) should produce (root( table(strrow("a","b","c"), row(cell("1-1\n2-2\n3-3", 2, 2), cell("d")), strrow("e"))))
  }
  
  it should "parse tables with empty cells" in {
    val input = """+---+---+
      ||   |   |
      |+---+---+
      ||   |   |
      |+---+---+""".stripMargin
    Parsing (input) should produce (root( table( row(cell(),cell()), row(cell(),cell()))))
  }
  
  it should "fail in case of illegal merging of cells (variant 1)" in {
    val input = """+---+---+
      || a | b |
      |+   +---+
      ||  c d  |
      |+---+---+""".stripMargin
    Parsing (input) should produce (root( p(input)))
  }
  
  it should "fail in case of illegal merging of cells (variant 2)" in {
    val input = """+---+---+
      || a | b |
      |+---+   +
      ||  c d  |
      |+---+---+""".stripMargin
    Parsing (input) should produce (root( p(input)))
  }
  
  it should "fail in case of illegal merging of cells (variant 3)" in {
    val input = """+---+---+
      ||  a b  |
      |+   +---+
      || c | d |
      |+---+---+""".stripMargin
    Parsing (input) should produce (root( p(input)))
  }
  
  it should "fail in case of illegal merging of cells (variant 4)" in {
    val input = """+---+---+
      ||  a b  |
      |+---+   +
      || c | d |
      |+---+---+""".stripMargin
    Parsing (input) should produce (root( p(input)))
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
    Parsing (input) should produce (root( table(row(cell("a"), cell(p("Text"), bulletList() + "Line1\nLine2" + "Line3")), strrow("c","d"))))
  }
  
  it should "parse tables with header cells" in {
    val input = """+---+---+
      || a | b |
      |+===+===+
      || c | d |
      |+---+---+""".stripMargin
    Parsing (input) should produce (root( Table(TableHead(List(row(Cell(HeadCell,List(p("a"))), Cell(HeadCell,List(p("b")))))), 
                                               TableBody(List(strrow("c","d"))))))
  }
  
  
  
  "The simple table parser" should "parse a small table with 2 rows and 2 cells" in {
    val input = """===  ===
      | a    b
      | c    d
      |===  ===""".stripMargin
    Parsing (input) should produce (root( table(strrow("a","b"), strrow("c","d"))))
  }
  
  it should "parse a table with horizontally merged cells in the first row" in {
    val input = """===  ===
      | a    b
      |--------
      | c    d
      |===  ===""".stripMargin
    Parsing (input) should produce (root( table(row(cell("a    b", 2, 1)), strrow("c","d"))))
  }
  
  it should "parse a table with horizontally merged cells in the second row" in {
    val input = """===  ===
      | a    b
      | c    d
      |========""".stripMargin
    Parsing (input) should produce (root( table(strrow("a","b"), row(cell("c    d", 2, 1)))))
  }
  
  it should "parse tables with empty cells" in {
    val input = """===  ===
      | a     
      | c     
      |===  ===""".stripMargin
    Parsing (input) should produce (root( table(row(cell("a"),cell()), row(cell("c"),cell()))))
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
    Parsing (input) should produce (root( table(row(cell("a"), cell(p("Text"), bulletList() + "Line1\nLine2" + "Line3")), strrow("c","d"))))
  }
  
  it should "parse tables with header cells" in {
    val input = """===  ===
      | a    b
      |===  ===
      | c    d
      |===  ===""".stripMargin
    Parsing (input) should produce (root( Table(TableHead(List(row(Cell(HeadCell,List(p("a"))), Cell(HeadCell,List(p("b")))))), 
                                               TableBody(List(strrow("c","d"))))))
  }
  
  
}
