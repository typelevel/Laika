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

import laika.ast.sample.{ParagraphCompanionShortcuts, TestSourceBuilders}
import laika.ast._
import laika.parse.Parser
import laika.parse.helper.MigrationFlatSpec
import laika.rst.ast._
import laika.rst.ext.Directives.DirectivePart
import laika.rst.ext.TextRoles.RoleDirectivePart
import laika.rst.ext.{ExtensionProvider, RootParserProvider}
import org.scalatest.Assertion
     
class ListParsersSpec extends MigrationFlatSpec
  with ParagraphCompanionShortcuts
  with TestSourceBuilders {

  import EnumType._

  val defaultParser: Parser[RootElement] = RootParserProvider.forBundle(ExtensionProvider.forExtensions()).rootElement

  
  def blockDirective (name: String): Option[DirectivePart[Block]] = None
  def spanDirective (name: String): Option[DirectivePart[Span]] = None
  def textRole (name: String): Option[RoleDirectivePart[String => Span]] = None
  
  
  def fp (content: String) = ForcedParagraph(List(Text(content)))
  
  
  def fl (fields: Field*) = FieldList(fields.toList)
  
  def field (name: String, blocks: Block*) = Field(List(Text(name)), blocks.toList)
  
  
  def oli (name: String, value: Block*) = OptionListItem(List(ProgramOption(name, None)), value.toList)

  def oli (name: String, value: String) = OptionListItem(List(ProgramOption(name, None)), List(p(value)))

  def oli (name: String, argDelim: String, arg: String, value: String) = 
    OptionListItem(List(ProgramOption(name, Some(OptionArgument(arg,argDelim)))), List(p(value)))
  
  def optL (items: OptionListItem*) = OptionList(items.toList)


  def run (input: String, blocks: Block*): Assertion =
    assertEquals(defaultParser.parse(input).toEither, Right(RootElement(blocks)))
  
  
  "The bullet list parser" should "parse items that are not separated by blank lines" in {
    val input = """* aaa
      |* bbb
      |* ccc""".stripMargin
    run(input, BulletList("aaa", "bbb", "ccc"))
  }
  
  it should "parse items that are separated by blank lines" in {
    val input = """* aaa
      |
      |* bbb
      |
      |* ccc""".stripMargin
    run(input, BulletList("aaa", "bbb", "ccc"))
  }
  
  it should "parse items starting with a '+' the same way as those starting with a '*'" in {
    val input = """+ aaa
      |+ bbb
      |+ ccc""".stripMargin
    run(input, BulletList(StringBullet("+"))("aaa", "bbb", "ccc"))
  }
  
  it should "parse items starting with a '-' the same way as those starting with a '*'" in {
    val input = """- aaa
      |- bbb
      |- ccc""".stripMargin
    run(input, BulletList(StringBullet("-"))("aaa", "bbb", "ccc"))
  }
  
  it should "parse items containing multiple paragraphs in a single item" in {
    val input = """* aaa
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
  
  it should "parse nested items indented by spaces" in {
    val input = """* aaa
                  |
                  |  * bbb
                  |
                  |    * ccc""".stripMargin
    val list3 = BulletList("ccc")
    val list2 = BulletList(Seq(SpanSequence("bbb"), list3))
    val list1 = BulletList(Seq(SpanSequence("aaa"), list2))
    run(input, list1)
  }
  
  it should "ignore items when the second line is not indented" in {
    val input = """* aaa
      |bbb""".stripMargin
    run(input, p("* aaa\nbbb"))
  }
  
  it should "parse a literal block after the first line of a list item" in {
    val input = """* aaa::
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
  
  
  "The enumerated list parser" should "parse items with arabic enumeration style" in {
    val input = """1. aaa
      |2. bbb
      |3. ccc""".stripMargin
    run(input, EnumList(EnumFormat(Arabic, "", "."))("aaa", "bbb", "ccc"))
  }
  
  it should "parse items with lowercase alphabetic enumeration style" in {
    val input = """a. aaa
      |b. bbb
      |c. ccc""".stripMargin
    run(input, EnumList(EnumFormat(LowerAlpha, "", "."))("aaa", "bbb", "ccc"))
  }
  
  it should "parse items with uppercase alphabetic enumeration style" in {
    val input = """A. aaa
      |B. bbb
      |C. ccc""".stripMargin
    run(input, EnumList(EnumFormat(UpperAlpha, "", "."))("aaa", "bbb", "ccc"))
  }
  
  it should "parse items with lowercase Roman enumeration style" in {
    val input = """i. aaa
      |ii. bbb
      |iii. ccc""".stripMargin
    run(input, EnumList(EnumFormat(LowerRoman, "", "."))("aaa", "bbb", "ccc"))
  }
  
  it should "parse items with uppercase Roman enumeration style" in {
    val input = """I. aaa
      |II. bbb
      |III. ccc""".stripMargin
    run(input, EnumList(EnumFormat(UpperRoman, "", "."))("aaa", "bbb", "ccc"))
  }
  
  it should "keep the right start value for arabic enumeration style" in {
    val input = """4. aaa
      |5. bbb""".stripMargin
    run(input, EnumList(EnumFormat(Arabic, "", "."), 4)("aaa", "bbb"))
  }
  
  it should "keep the right start value for lowercase alphabetic enumeration style" in {
    val input = """d. aaa
      |e. bbb""".stripMargin
    run(input, EnumList(EnumFormat(LowerAlpha, "", "."), 4)("aaa", "bbb"))
  }
  
  it should "keep the right start value for uppercase alphabetic enumeration style" in {
    val input = """D. aaa
      |E. bbb""".stripMargin
    run(input, EnumList(EnumFormat(UpperAlpha, "", "."), 4)("aaa", "bbb"))
  }
  
  it should "keep the right start value for lowercase Roman enumeration style" in {
    val input = """iv. aaa
      |v. bbb""".stripMargin
    run(input, EnumList(EnumFormat(LowerRoman, "", "."), 4)("aaa", "bbb"))
  }
  
  it should "keep the right start value for uppercase Roman enumeration style" in {
    val input = """IV. aaa
      |V. bbb""".stripMargin
    run(input, EnumList(EnumFormat(UpperRoman, "", "."), 4)("aaa", "bbb"))
  }
  
  it should "not try to parse a Roman Numeral in a normal paragraph (issue #19)" in {
    val input = "imp"
    run(input, p("imp"))
  }
  
  it should "parse items suffixed by right-parenthesis" in {
    val input = """1) aaa
      |2) bbb
      |3) ccc""".stripMargin
    run(input, EnumList(EnumFormat(Arabic, "", ")"))("aaa", "bbb", "ccc"))
  }
  
  it should "parse items surrounded by parenthesis" in {
    val input = """(1) aaa
      |(2) bbb
      |(3) ccc""".stripMargin
    run(input, EnumList(EnumFormat(Arabic, "(", ")"))("aaa", "bbb", "ccc"))
  }
  
  it should "parse items that are separated by blank lines" in {
    val input = """1. aaa
      |
      |2. bbb
      |
      |3. ccc""".stripMargin
    run(input, EnumList(EnumFormat(Arabic))("aaa", "bbb", "ccc"))
  }
  
  it should "parse items containing multiple paragraphs in a single item" in {
    val input = """1. aaa
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
  
  it should "parse nested items indented by spaces" in {
    val input = """1. aaa
                  |
                  |   1. bbb
                  |
                  |      1. ccc""".stripMargin
    val list3 = EnumList("ccc")
    val list2 = EnumList(Seq(SpanSequence("bbb"), list3))
    val list1 = EnumList(Seq(SpanSequence("aaa"), list2))
    run(input, list1)
  }
  
  it should "parse items with different enumeration patterns into separate lists" in {
    val input = """1. aaa
      |
      |2. bbb
      |
      |1) ccc
      |
      |2) ddd""".stripMargin
    val f = EnumFormat(Arabic,"",")")
    run(input, EnumList("aaa","bbb"), EnumList(f)("ccc","ddd"))
  }
  
  
  
  "The definition list parser" should "parse items that are not separated by blank lines" in {
    val input = """term 1
      | aaa
      |term 2
      | bbb""".stripMargin
    val list = DefinitionList(
      DefinitionListItem("term 1", p("aaa")),
      DefinitionListItem("term 2", p("bbb"))
    )
    run(input, list)
  }
  
  it should "parse items that are separated by blank lines" in {
    val input = """term 1
      | aaa
      |
      |term 2
      | bbb""".stripMargin
    val list = DefinitionList(
      DefinitionListItem("term 1", p("aaa")),
      DefinitionListItem("term 2", p("bbb"))
    )
    run(input, list)
  }
  
  it should "parse a term with a classifier" in {
    val input = """term 1
      | aaa
      |
      |term 2 : classifier
      | bbb""".stripMargin
    val list = DefinitionList(
      DefinitionListItem("term 1", p("aaa")),
      DefinitionListItem(List(Text("term 2"), Classifier(List(Text("classifier")))), List(p("bbb")))
    )
    run(input, list)
  }
  
  it should "parse items containing multiple paragraphs in a single item" in {
    val input = """term 1
      |  aaa
      |  aaa
      |
      |  bbb
      |
      |term 2
      |  ccc""".stripMargin
    val list = DefinitionList(
      DefinitionListItem("term 1", p("aaa\naaa"), p("bbb")),
      DefinitionListItem("term 2", p("ccc"))
    )
    run(input, list)
  }
  
  it should "parse items containing multiple paragraphs with different identation in a single item" in {
    val input = """term 1
      |   aaa
      |   aaa
      |
      |  bbb
      |
      |term 2
      |  ccc""".stripMargin
    val list = DefinitionList(
      DefinitionListItem("term 1", QuotedBlock("aaa\naaa"), p("bbb")),
      DefinitionListItem("term 2", p("ccc"))
    )
    run(input, list)
  }
  
  it should "support inline markup in the term" in {
    val input = """term *em*
      | aaa
      |
      |term 2
      | bbb""".stripMargin
    val list = DefinitionList(
      DefinitionListItem(List(Text("term "), Emphasized("em")), List(p("aaa"))),
      DefinitionListItem("term 2", p("bbb"))
    )
    run(input, list)
  }
  
  it should "ignore subsequent tables" in {
    val input = """term 1
      | aaa
      |
      |term 2
      | bbb
      |
      |=== ===
      | a   b 
      |=== ===""".stripMargin
    val list = DefinitionList(
      DefinitionListItem("term 1", p("aaa")),
      DefinitionListItem("term 2", p("bbb"))
    )
    run(input, list, Table(Row(BodyCell("a"),BodyCell("b"))))
  }
  
  it should "ignore subsequent directives" in {
    val directive = """.. foo::
                      | :name: value""".stripMargin
    val input = s"""term 1
      | aaa
      |
      |term 2
      | bbb
      |
      |$directive""".stripMargin
    val list = DefinitionList(
      DefinitionListItem("term 1", p("aaa")),
      DefinitionListItem("term 2", p("bbb"))
    )
    run(input, list, InvalidBlock("unknown directive: foo", source(directive, input)))
  }
  
  it should "ignore subsequent bullet lists" in {
    val input = """term 1
      | aaa
      |
      |term 2
      | bbb
      |
      |* list
      |  list""".stripMargin
    val list = DefinitionList(
      DefinitionListItem("term 1", p("aaa")),
      DefinitionListItem("term 2", p("bbb"))
    )
    run(input, list, BulletList(p("list\nlist")))
  }
  
  it should "ignore subsequent enum lists" in {
    val input = """term 1
      | aaa
      |
      |term 2
      | bbb
      |
      |1. list
      |   list""".stripMargin
    val list = DefinitionList(
      DefinitionListItem("term 1", p("aaa")),
      DefinitionListItem("term 2", p("bbb"))
    )
    run(input, list, EnumList(EnumFormat(Arabic))("list\nlist"))
  }
  
  it should "ignore subsequent headers with overline" in {
    val header = """########
                   | Header
                   |########""".stripMargin
    val input = s"""term 1
      | aaa
      |
      |term 2
      | bbb
      |
      |$header""".stripMargin
    val list = DefinitionList(
      DefinitionListItem("term 1", p("aaa")),
      DefinitionListItem("term 2", p("bbb"))
    )
    run(input, list, DecoratedHeader(OverlineAndUnderline('#'), List(Text("Header")), source(header, input)))
  }
  
  
  
  "The field list parser" should "parse a list with all bodies on the same line as the name" in {
    val input = """:name1: value1
      |:name2: value2
      |:name3: value3""".stripMargin
    run(input, fl( field("name1", p("value1")), field("name2", p("value2")), field("name3", p("value3"))))
  }
  
  it should "parse a list with bodies spanning multiple lines" in {
    val input = """:name1: line1a
      |  line1b
      |:name2: line2a
      |  line2b""".stripMargin
    run(input, fl( field("name1", p("line1a\nline1b")), field("name2", p("line2a\nline2b"))))
  }
  
  it should "parse a list with bodies spanning multiple blocks" in {
    val input = """:name1: line1a
      |  line1b
      |
      |  line1c
      |  line1d
      |:name2: line2a
      |  line2b""".stripMargin
    run(input, fl( field("name1", p("line1a\nline1b"), p("line1c\nline1d")), field("name2", p("line2a\nline2b"))))
  }
  
  
  "The option list parser" should "parse a list with short posix options" in {
    val input = """-a  Option1
      |-b  Option2""".stripMargin
    run(input, optL( oli("-a", "Option1"), oli("-b", "Option2")))
  }
  
  it should "parse a list with long posix options" in {
    val input = """--aaaa  Option1
      |--bbbb  Option2""".stripMargin
    run(input, optL( oli("--aaaa", "Option1"), oli("--bbbb", "Option2")))
  }
  
  it should "parse a list with short GNU-style options" in {
    val input = """+a  Option1
      |+b  Option2""".stripMargin
    run(input, optL( oli("+a", "Option1"), oli("+b", "Option2")))
  }
  
  it should "parse a list with short DOS-style options" in {
    val input = """/a  Option1
      |/b  Option2""".stripMargin
    run(input, optL( oli("/a", "Option1"), oli("/b", "Option2")))
  }
  
  it should "parse an option argument separated by a space" in {
    val input = """-a FILE  Option1
      |-b  Option2""".stripMargin
    run(input, optL( oli("-a", " ", "FILE", "Option1"), oli("-b", "Option2")))
  }
  
  it should "parse an option argument separated by '='" in {
    val input = """-a=FILE  Option1
      |-b  Option2""".stripMargin
    run(input, optL( oli("-a", "=", "FILE", "Option1"), oli("-b", "Option2")))
  }

  it should "parse an option argument enclosed in angle brackets" in {
    val input = """-a <in=out>  Option1
                  |-b  Option2""".stripMargin
    run(input, optL( oli("-a", " ", "<in=out>", "Option1"), oli("-b", "Option2")))
  }
  
  it should "parse a description starting on the next line" in {
    val input = """-a
      |    Option1
      |-b  Option2""".stripMargin
    run(input, optL( oli("-a", "Option1"), oli("-b", "Option2")))
  }
  
  it should "parse a block of options with blank lines between them" in {
    val input = """-a  Option1
      |
      |-b  Option2""".stripMargin
    run(input, optL( oli("-a", "Option1"), oli("-b", "Option2")))
  }
  
  it should "parse a description containing multiple paragraphs" in {
    val input = """-a  Line1
      |                Line2
      |
      |                Line3
      |
      |-b  Option2""".stripMargin
    run(input, optL( oli("-a", p("Line1\nLine2"), p("Line3")), oli("-b", "Option2")))
  }

  it should "parse an option separated by more than 2 spaces" in {
    val input = """-a   Option""".stripMargin
    run(input, optL(oli("-a", "Option")))
  }



  "The line block parser" should "parse a block with out continuation or indentation" in {
    val input = """|| Line1
      || Line2
      || Line3""".stripMargin
    run(input, LineBlock( Line("Line1"), Line("Line2"), Line("Line3")))
  }
  
  it should "parse a block with a continuation line" in {
    val input = """|| Line1
      |  Line2
      || Line3
      || Line4""".stripMargin
    run(input, LineBlock( Line("Line1\nLine2"), Line("Line3"), Line("Line4")))
  }
  
  it should "parse a nested structure (pointing right)" in {
    val input = """|| Line1
      ||   Line2
      ||     Line3
      ||   Line4
      || Line5""".stripMargin
    run(input, LineBlock( Line("Line1"), LineBlock(Line("Line2"), LineBlock(Line("Line3")), Line("Line4")), Line("Line5")))
  }
  
  it should "parse a nested structure (pointing left)" in {
    val input = """||     Line1
      ||   Line2
      || Line3
      ||   Line4
      ||     Line5""".stripMargin
    run(input, LineBlock( LineBlock( LineBlock(Line("Line1")), Line("Line2")), Line("Line3"), LineBlock(Line("Line4"), LineBlock(Line("Line5")))))
  }
  
  
}
