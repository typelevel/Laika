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

import laika.ast.EnumType.Arabic
import laika.ast.sample.{ ParagraphCompanionShortcuts, TestSourceBuilders }
import laika.ast._
import laika.parse.Parser
import laika.rst.ast._
import laika.rst.ext.{ ExtensionProvider, RootParserProvider }
import munit.{ Assertions, FunSuite }

trait ListParserRunner extends Assertions with ParagraphCompanionShortcuts {

  private val defaultParser: Parser[RootElement] =
    RootParserProvider.forBundle(ExtensionProvider.forExtensions()).rootElement

  def fp(content: String): ForcedParagraph = ForcedParagraph(List(Text(content)))

  def run(input: String, blocks: Block*)(implicit loc: munit.Location): Unit =
    assertEquals(defaultParser.parse(input).toEither, Right(RootElement(blocks)))

}

class BulletListSpec extends FunSuite with ListParserRunner {

  test("list items not separated by blank lines") {
    val input =
      """* aaa
        |* bbb
        |* ccc""".stripMargin
    run(input, BulletList("aaa", "bbb", "ccc"))
  }

  test("list items separated by blank lines") {
    val input =
      """* aaa
        |
        |* bbb
        |
        |* ccc""".stripMargin
    run(input, BulletList("aaa", "bbb", "ccc"))
  }

  test("list items starting with '+' treated the same way as those starting with a '*'") {
    val input =
      """+ aaa
        |+ bbb
        |+ ccc""".stripMargin
    run(input, BulletList(StringBullet("+"))("aaa", "bbb", "ccc"))
  }

  test("list items starting with '-' treated the same way as those starting with a '*'") {
    val input =
      """- aaa
        |- bbb
        |- ccc""".stripMargin
    run(input, BulletList(StringBullet("-"))("aaa", "bbb", "ccc"))
  }

  test("list items containing multiple paragraphs in a single item") {
    val input    =
      """* aaa
        |   
        |  bbb
        |  bbb
        |
        |* ccc
        |
        |* ddd""".stripMargin
    val expected = BulletList(
      Seq(p("aaa"), p("bbb\nbbb")),
      Seq(fp("ccc")),
      Seq(fp("ddd"))
    )
    run(input, expected)
  }

  test("nested items indented by spaces") {
    val input =
      """* aaa
        |
        |  * bbb
        |
        |    * ccc""".stripMargin
    val list3 = BulletList("ccc")
    val list2 = BulletList(Seq(SpanSequence("bbb"), list3))
    val list1 = BulletList(Seq(SpanSequence("aaa"), list2))
    run(input, list1)
  }

  test("ignore items when the second line is not indented") {
    val input =
      """* aaa
        |bbb""".stripMargin
    run(input, p("* aaa\nbbb"))
  }

  test("literal block after the first line of a list item") {
    val input    =
      """* aaa::
        |   
        |   bbb
        |   bbb
        |
        |* ccc
        |
        |* ddd""".stripMargin
    val expected = BulletList(
      Seq(p("aaa:"), LiteralBlock("bbb\nbbb")),
      Seq(fp("ccc")),
      Seq(fp("ddd"))
    )
    run(input, expected)
  }

}

class EnumListSpec extends FunSuite with ListParserRunner {

  import EnumType._

  test("arabic enumeration style") {
    val input =
      """1. aaa
        |2. bbb
        |3. ccc""".stripMargin
    run(input, EnumList(EnumFormat(Arabic))("aaa", "bbb", "ccc"))
  }

  test("lowercase alphabetic enumeration style") {
    val input =
      """a. aaa
        |b. bbb
        |c. ccc""".stripMargin
    run(input, EnumList(EnumFormat(LowerAlpha))("aaa", "bbb", "ccc"))
  }

  test("uppercase alphabetic enumeration style") {
    val input =
      """A. aaa
        |B. bbb
        |C. ccc""".stripMargin
    run(input, EnumList(EnumFormat(UpperAlpha))("aaa", "bbb", "ccc"))
  }

  test("lowercase Roman enumeration style") {
    val input =
      """i. aaa
        |ii. bbb
        |iii. ccc""".stripMargin
    run(input, EnumList(EnumFormat(LowerRoman))("aaa", "bbb", "ccc"))
  }

  test("uppercase Roman enumeration style") {
    val input =
      """I. aaa
        |II. bbb
        |III. ccc""".stripMargin
    run(input, EnumList(EnumFormat(UpperRoman))("aaa", "bbb", "ccc"))
  }

  test("keep the right start value for arabic enumeration style") {
    val input =
      """4. aaa
        |5. bbb""".stripMargin
    run(input, EnumList(EnumFormat(Arabic), 4)("aaa", "bbb"))
  }

  test("keep the right start value for lowercase alphabetic enumeration style") {
    val input =
      """d. aaa
        |e. bbb""".stripMargin
    run(input, EnumList(EnumFormat(LowerAlpha), 4)("aaa", "bbb"))
  }

  test("keep the right start value for uppercase alphabetic enumeration style") {
    val input =
      """D. aaa
        |E. bbb""".stripMargin
    run(input, EnumList(EnumFormat(UpperAlpha), 4)("aaa", "bbb"))
  }

  test("keep the right start value for lowercase Roman enumeration style") {
    val input =
      """iv. aaa
        |v. bbb""".stripMargin
    run(input, EnumList(EnumFormat(LowerRoman), 4)("aaa", "bbb"))
  }

  test("keep the right start value for uppercase Roman enumeration style") {
    val input =
      """IV. aaa
        |V. bbb""".stripMargin
    run(input, EnumList(EnumFormat(UpperRoman), 4)("aaa", "bbb"))
  }

  test("do not try to parse a Roman Numeral in a normal paragraph (issue #19)") {
    val input = "imp"
    run(input, p("imp"))
  }

  test("item label suffixed by right-parenthesis") {
    val input =
      """1) aaa
        |2) bbb
        |3) ccc""".stripMargin
    run(input, EnumList(EnumFormat(Arabic, "", ")"))("aaa", "bbb", "ccc"))
  }

  test("item label surrounded by parenthesis") {
    val input =
      """(1) aaa
        |(2) bbb
        |(3) ccc""".stripMargin
    run(input, EnumList(EnumFormat(Arabic, "(", ")"))("aaa", "bbb", "ccc"))
  }

  test("items separated by blank lines") {
    val input =
      """1. aaa
        |
        |2. bbb
        |
        |3. ccc""".stripMargin
    run(input, EnumList(EnumFormat(Arabic))("aaa", "bbb", "ccc"))
  }

  test("items containing multiple paragraphs") {
    val input    =
      """1. aaa
        |   
        |   bbb
        |   bbb
        |
        |2. ccc
        |
        |3. ddd""".stripMargin
    val expected = EnumList(
      Seq(p("aaa"), p("bbb\nbbb")),
      Seq(fp("ccc")),
      Seq(fp("ddd"))
    )
    run(input, expected)
  }

  test("nested items indented by spaces") {
    val input =
      """1. aaa
        |
        |   1. bbb
        |
        |      1. ccc""".stripMargin
    val list3 = EnumList("ccc")
    val list2 = EnumList(Seq(SpanSequence("bbb"), list3))
    val list1 = EnumList(Seq(SpanSequence("aaa"), list2))
    run(input, list1)
  }

  test("different enumeration patterns kept in separate lists") {
    val input =
      """1. aaa
        |
        |2. bbb
        |
        |1) ccc
        |
        |2) ddd""".stripMargin
    val f     = EnumFormat(Arabic, "", ")")
    run(input, EnumList("aaa", "bbb"), EnumList(f)("ccc", "ddd"))
  }

}

class DefinitionListSpec extends FunSuite with ListParserRunner with TestSourceBuilders {

  test("items not separated by blank lines") {
    val input =
      """term 1
        | aaa
        |term 2
        | bbb""".stripMargin
    val list  = DefinitionList(
      DefinitionListItem("term 1", p("aaa")),
      DefinitionListItem("term 2", p("bbb"))
    )
    run(input, list)
  }

  test("items separated by blank lines") {
    val input =
      """term 1
        | aaa
        |
        |term 2
        | bbb""".stripMargin
    val list  = DefinitionList(
      DefinitionListItem("term 1", p("aaa")),
      DefinitionListItem("term 2", p("bbb"))
    )
    run(input, list)
  }

  test("term with a classifier") {
    val input =
      """term 1
        | aaa
        |
        |term 2 : classifier
        | bbb""".stripMargin
    val list  = DefinitionList(
      DefinitionListItem("term 1", p("aaa")),
      DefinitionListItem(List(Text("term 2"), Classifier(List(Text("classifier")))), List(p("bbb")))
    )
    run(input, list)
  }

  test("items containing multiple paragraphs") {
    val input =
      """term 1
        |  aaa
        |  aaa
        |
        |  bbb
        |
        |term 2
        |  ccc""".stripMargin
    val list  = DefinitionList(
      DefinitionListItem("term 1", p("aaa\naaa"), p("bbb")),
      DefinitionListItem("term 2", p("ccc"))
    )
    run(input, list)
  }

  test("items containing multiple paragraphs with different indentation") {
    val input =
      """term 1
        |   aaa
        |   aaa
        |
        |  bbb
        |
        |term 2
        |  ccc""".stripMargin
    val list  = DefinitionList(
      DefinitionListItem("term 1", QuotedBlock("aaa\naaa"), p("bbb")),
      DefinitionListItem("term 2", p("ccc"))
    )
    run(input, list)
  }

  test("inline markup in the term") {
    val input =
      """term *em*
        | aaa
        |
        |term 2
        | bbb""".stripMargin
    val list  = DefinitionList(
      DefinitionListItem(List(Text("term "), Emphasized("em")), List(p("aaa"))),
      DefinitionListItem("term 2", p("bbb"))
    )
    run(input, list)
  }

  test("ignore subsequent tables") {
    val input =
      """term 1
        | aaa
        |
        |term 2
        | bbb
        |
        |=== ===
        | a   b 
        |=== ===""".stripMargin
    val list  = DefinitionList(
      DefinitionListItem("term 1", p("aaa")),
      DefinitionListItem("term 2", p("bbb"))
    )
    run(input, list, Table(Row(BodyCell("a"), BodyCell("b"))))
  }

  test("ignore subsequent directives") {
    val directive =
      """.. foo::
        | :name: value""".stripMargin
    val input     =
      s"""term 1
         | aaa
         |
         |term 2
         | bbb
         |
         |$directive""".stripMargin
    val list      = DefinitionList(
      DefinitionListItem("term 1", p("aaa")),
      DefinitionListItem("term 2", p("bbb"))
    )
    run(input, list, InvalidBlock("unknown directive: foo", source(directive, input)))
  }

  test("ignore subsequent bullet lists") {
    val input =
      """term 1
        | aaa
        |
        |term 2
        | bbb
        |
        |* list
        |  list""".stripMargin
    val list  = DefinitionList(
      DefinitionListItem("term 1", p("aaa")),
      DefinitionListItem("term 2", p("bbb"))
    )
    run(input, list, BulletList(p("list\nlist")))
  }

  test("ignore subsequent enum lists") {
    val input =
      """term 1
        | aaa
        |
        |term 2
        | bbb
        |
        |1. list
        |   list""".stripMargin
    val list  = DefinitionList(
      DefinitionListItem("term 1", p("aaa")),
      DefinitionListItem("term 2", p("bbb"))
    )
    run(input, list, EnumList(EnumFormat(Arabic))("list\nlist"))
  }

  test("ignore subsequent headers with overline") {
    val header =
      """########
        | Header
        |########""".stripMargin
    val input  =
      s"""term 1
         | aaa
         |
         |term 2
         | bbb
         |
         |$header""".stripMargin
    val list   = DefinitionList(
      DefinitionListItem("term 1", p("aaa")),
      DefinitionListItem("term 2", p("bbb"))
    )
    run(
      input,
      list,
      DecoratedHeader(OverlineAndUnderline('#'), List(Text("Header")), source(header, input))
    )
  }

}

class FieldListSpec extends FunSuite with ListParserRunner {

  def fl(fields: Field*): FieldList = FieldList(fields.toList)

  def field(name: String, blocks: Block*): Field = Field(List(Text(name)), blocks.toList)

  test("list with all bodies on the same line as the name") {
    val input =
      """:name1: value1
        |:name2: value2
        |:name3: value3""".stripMargin
    run(
      input,
      fl(field("name1", p("value1")), field("name2", p("value2")), field("name3", p("value3")))
    )
  }

  test("list with bodies spanning multiple lines") {
    val input =
      """:name1: line1a
        |  line1b
        |:name2: line2a
        |  line2b""".stripMargin
    run(input, fl(field("name1", p("line1a\nline1b")), field("name2", p("line2a\nline2b"))))
  }

  test("list with bodies spanning multiple blocks") {
    val input =
      """:name1: line1a
        |  line1b
        |
        |  line1c
        |  line1d
        |:name2: line2a
        |  line2b""".stripMargin
    run(
      input,
      fl(
        field("name1", p("line1a\nline1b"), p("line1c\nline1d")),
        field("name2", p("line2a\nline2b"))
      )
    )
  }

}

class OptionListSpec extends FunSuite with ListParserRunner {

  def optL(items: OptionListItem*): OptionList = OptionList(items.toList)

  def oli(name: String, value: Block*): OptionListItem =
    OptionListItem(List(ProgramOption(name, None)), value.toList)

  def oli(name: String, value: String): OptionListItem =
    OptionListItem(List(ProgramOption(name, None)), List(p(value)))

  def oli(name: String, argDelim: String, arg: String, value: String): OptionListItem =
    OptionListItem(List(ProgramOption(name, Some(OptionArgument(arg, argDelim)))), List(p(value)))

  test("list with short posix options") {
    val input =
      """-a  Option1
        |-b  Option2""".stripMargin
    run(input, optL(oli("-a", "Option1"), oli("-b", "Option2")))
  }

  test("list with long posix options") {
    val input =
      """--aaaa  Option1
        |--bbbb  Option2""".stripMargin
    run(input, optL(oli("--aaaa", "Option1"), oli("--bbbb", "Option2")))
  }

  test("list with short GNU-style options") {
    val input =
      """+a  Option1
        |+b  Option2""".stripMargin
    run(input, optL(oli("+a", "Option1"), oli("+b", "Option2")))
  }

  test("list with short DOS-style options") {
    val input =
      """/a  Option1
        |/b  Option2""".stripMargin
    run(input, optL(oli("/a", "Option1"), oli("/b", "Option2")))
  }

  test("option argument separated by a space") {
    val input =
      """-a FILE  Option1
        |-b  Option2""".stripMargin
    run(input, optL(oli("-a", " ", "FILE", "Option1"), oli("-b", "Option2")))
  }

  test("option argument separated by '='") {
    val input =
      """-a=FILE  Option1
        |-b  Option2""".stripMargin
    run(input, optL(oli("-a", "=", "FILE", "Option1"), oli("-b", "Option2")))
  }

  test("option argument enclosed in angle brackets") {
    val input =
      """-a <in=out>  Option1
        |-b  Option2""".stripMargin
    run(input, optL(oli("-a", " ", "<in=out>", "Option1"), oli("-b", "Option2")))
  }

  test("description starting on the next line") {
    val input =
      """-a
        |    Option1
        |-b  Option2""".stripMargin
    run(input, optL(oli("-a", "Option1"), oli("-b", "Option2")))
  }

  test("block of options with blank lines between them") {
    val input =
      """-a  Option1
        |
        |-b  Option2""".stripMargin
    run(input, optL(oli("-a", "Option1"), oli("-b", "Option2")))
  }

  test("description containing multiple paragraphs") {
    val input =
      """-a  Line1
        |                Line2
        |
        |                Line3
        |
        |-b  Option2""".stripMargin
    run(input, optL(oli("-a", p("Line1\nLine2"), p("Line3")), oli("-b", "Option2")))
  }

  test("option separated by more than 2 spaces") {
    val input = """-a   Option""".stripMargin
    run(input, optL(oli("-a", "Option")))
  }

}

class LineBlockSpec extends FunSuite with ListParserRunner {

  test("block with out continuation or indentation") {
    val input =
      """|| Line1
         || Line2
         || Line3""".stripMargin
    run(input, LineBlock(Line("Line1"), Line("Line2"), Line("Line3")))
  }

  test("block with a continuation line") {
    val input =
      """|| Line1
         |  Line2
         || Line3
         || Line4""".stripMargin
    run(input, LineBlock(Line("Line1\nLine2"), Line("Line3"), Line("Line4")))
  }

  test("nested structure (pointing right)") {
    val input =
      """|| Line1
         ||   Line2
         ||     Line3
         ||   Line4
         || Line5""".stripMargin
    run(
      input,
      LineBlock(
        Line("Line1"),
        LineBlock(Line("Line2"), LineBlock(Line("Line3")), Line("Line4")),
        Line("Line5")
      )
    )
  }

  test("nested structure (pointing left)") {
    val input =
      """||     Line1
         ||   Line2
         || Line3
         ||   Line4
         ||     Line5""".stripMargin
    run(
      input,
      LineBlock(
        LineBlock(LineBlock(Line("Line1")), Line("Line2")),
        Line("Line3"),
        LineBlock(Line("Line4"), LineBlock(Line("Line5")))
      )
    )
  }

}
