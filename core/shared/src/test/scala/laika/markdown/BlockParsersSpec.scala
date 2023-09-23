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
import laika.internal.parse.markup.RootParser
import laika.parse.Parser
import munit.FunSuite

class BlockParsersSpec extends FunSuite with ParagraphCompanionShortcuts {

  val rootParser =
    new RootParser(
      Markdown,
      new OperationConfig(Markdown.extensions).forStrictMode.markupExtensions
    )

  val defaultParser: Parser[RootElement] = rootParser.rootElement

  def run(input: String, blocks: Block*)(implicit loc: munit.Location): Unit =
    assertEquals(defaultParser.parse(input).toEither, Right(RootElement(blocks)))

  def fp(content: String): ForcedParagraph = ForcedParagraph(List(Text(content)))

  test("paragraphs - blocks without block-level markup parsed as normal paragraphs") {
    val input = """aaa
                  |bbb
                  |ccc
                  |
                  |ddd
                  |eee""".stripMargin
    run(input, p("aaa\nbbb\nccc"), p("ddd\neee"))
  }

  test("paragraphs - double space at a line end parsed as a hard line break") {
    run("some text  \nsome more", p(Text("some text"), LineBreak(), Text("\nsome more")))
  }

  test("bullet lists - items not separated by blank lines as list items with flow content") {
    val input = """* aaa
                  |* bbb
                  |* ccc""".stripMargin
    run(input, BulletList("aaa", "bbb", "ccc"))
  }

  test("bullet lists - items separated by blank lines as list items with 'forced' paragraphs") {
    val input = """* aaa
                  |
                  |* bbb
                  |
                  |* ccc""".stripMargin
    run(input, BulletList(fp("aaa"), fp("bbb"), fp("ccc")))
  }

  test(
    "bullet lists - items indented by a tab after the '*' treated the same way as items indented by a space"
  ) {
    val input = """*	aaa
                  |*	bbb
                  |*	ccc""".stripMargin
    run(input, BulletList("aaa", "bbb", "ccc"))
  }

  test(
    "bullet lists - items starting with a '+' treated the same way as those starting with a '*'"
  ) {
    val input = """+ aaa
                  |+ bbb
                  |+ ccc""".stripMargin
    run(input, BulletList(BulletFormat.StringBullet("+"))("aaa", "bbb", "ccc"))
  }

  test(
    "bullet lists - items starting with a '-' treated the same way as those starting with a '*'"
  ) {
    val input = """- aaa
                  |- bbb
                  |- ccc""".stripMargin
    run(input, BulletList(BulletFormat.StringBullet("-"))("aaa", "bbb", "ccc"))
  }

  test("bullet lists - items prefixed by numbers as items of an enumerated list") {
    val input = """1. aaa
                  |2. bbb
                  |3. ccc""".stripMargin
    run(input, EnumList("aaa", "bbb", "ccc"))
  }

  test(
    "enum lists - items prefixed by numbers and separated by blank lines as ordered list items with paragraph"
  ) {
    val input = """1. aaa
                  |
                  |2. bbb
                  |
                  |3. ccc""".stripMargin
    run(input, EnumList(fp("aaa"), fp("bbb"), fp("ccc")))
  }

  test("enum lists - items prefixed by numbers containing multiple paragraphs in a single item") {
    val input    = """1. aaa
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

  test("bullet lists - nested items indented by spaces") {
    val input = """*   aaa
                  |    *   bbb
                  |        * ccc""".stripMargin

    val list3 = BulletList("ccc")
    val list2 = BulletList(Seq(SpanSequence("bbb"), list3))
    val list1 = BulletList(Seq(SpanSequence("aaa"), list2))

    run(input, list1)
  }

  test("bullet lists - nested items indented by tabs") {
    val input = """*	aaa
                  |	* bbb
                  |		* ccc""".stripMargin

    val list3 = BulletList("ccc")
    val list2 = BulletList(Seq(SpanSequence("bbb"), list3))
    val list1 = BulletList(Seq(SpanSequence("aaa"), list2))

    run(input, list1)
  }

  test("lists - bullet list nested inside enumerated list") {
    val input = """1.  111
                  |2.  222
                  |    * aaa
                  |    * bbb
                  |    * ccc
                  |3. 333""".stripMargin

    val nestedList = BulletList("aaa", "bbb", "ccc")
    val expected   = EnumList(
      Seq(p("111")),
      Seq(SpanSequence("222"), nestedList),
      Seq(p("333"))
    )
    run(input, expected)
  }

  test("lists - bullet list nested inside enumerated list with blank lines between the items") {
    val input = """1.  111
                  |
                  |2.  222
                  |    * aaa
                  |    * bbb
                  |    * ccc
                  |
                  |3. 333""".stripMargin

    val nestedList = BulletList("aaa", "bbb", "ccc")
    val expected   = EnumList(
      Seq(fp("111")),
      Seq(p("222"), nestedList),
      Seq(fp("333"))
    )
    run(input, expected)
  }

  test("enum lists - list nested between two paragraphs inside a list item") {
    val input = """*	aaa
                  |
                  |	*	bbb
                  |
                  |	ccc""".stripMargin

    val nestedList = BulletList("bbb")
    val list       = BulletList(List(p("aaa"), nestedList, p("ccc")))
    run(input, list)
  }

  test("blockquotes - paragraph decorated with '>' at the beginning of each line") {
    val input = """>aaa
                  |>bbb
                  |>ccc""".stripMargin
    run(input, QuotedBlock("aaa\nbbb\nccc"))
  }

  test("blockquotes - paragraph decorated with '>' only at the beginning of the first line") {
    val input = """>aaa
                  |bbb
                  |ccc""".stripMargin
    run(input, QuotedBlock("aaa\nbbb\nccc"))
  }

  test(
    "blockquotes - two adjacent paragraphs decorated with '>' as a single blockquote with two paragraphs"
  ) {
    val input = """>aaa
                  |bbb
                  |ccc
                  |
                  |>ddd
                  |>eee
                  |>fff""".stripMargin
    run(input, QuotedBlock(p("aaa\nbbb\nccc"), p("ddd\neee\nfff")))
  }

  test("blockquotes - nested blockquote decorated with a second '>'") {
    val input = """>aaa
                  |>
                  |>>bbb
                  |>
                  |>ccc""".stripMargin
    run(input, QuotedBlock(p("aaa"), QuotedBlock("bbb"), p("ccc")))
  }

  test(
    "blockquotes - prevent endless recursion and stop after the configured maximum of 12 nest levels"
  ) {
    val input = ">>>>>>>>>>>>>>>>aaa\n\nbbb"
    // 3 '>' chars left (16 minus root level minus 12 nest levels)
    val res   = defaultParser.parse(input).toOption
    assert(res.exists(_.select(_ == p(">>>aaa")).nonEmpty))
  }

  test("literal blocks - paragraphs indented with 4 or more spaces as a code block") {
    val input = """    code
                  |
                  |text
                  |
                  |    code
                  |
                  |text
                  |
                  |    code""".stripMargin
    run(
      input,
      LiteralBlock("code"),
      p("text"),
      LiteralBlock("code"),
      p("text"),
      LiteralBlock("code")
    )
  }

  test("literal blocks - paragraphs indented with 1 or more tabs as a code block") {
    val input = """	code
                  |
                  |text
                  |
                  |	code
                  |
                  |text
                  |
                  |	code""".stripMargin
    run(
      input,
      LiteralBlock("code"),
      p("text"),
      LiteralBlock("code"),
      p("text"),
      LiteralBlock("code")
    )
  }

  test("literal blocks - indented lines separated by blank lines as a single code block") {
    val input = """    code 1
                  |
                  |    code 2
                  |
                  |    code 3""".stripMargin
    run(input, LiteralBlock("code 1\n\ncode 2\n\ncode 3"))
  }

  test(
    "setext header - title decorated by one or more '=' on the following line as a level 1 header"
  ) {
    val input = """aaa
                  |bbb
                  |
                  |CCC
                  |==""".stripMargin
    run(input, p("aaa\nbbb"), Header(1, "CCC"))
  }

  test(
    "setext header - title decorated by one or more '-' on the following line as a level 2 header"
  ) {
    val input = """aaa
                  |bbb
                  |
                  |CCC
                  |---""".stripMargin
    run(input, p("aaa\nbbb"), Header(2, "CCC"))
  }

  test("setext header - title that includes markup") {
    val input = """aaa
                  |bbb
                  |
                  |CCC *DDD* EEE
                  |---""".stripMargin
    run(input, p("aaa\nbbb"), Header(2, Text("CCC "), Emphasized("DDD"), Text(" EEE")))
  }

  test("atx header - title decorated by one '#' on the beginning of the line as a level 1 header") {
    val input = """aaa
                  |bbb
                  |
                  |# CCC""".stripMargin
    run(input, p("aaa\nbbb"), Header(1, "CCC"))
  }

  test(
    "atx header - title decorated by three '#' on the beginning of the line as a level 3 header"
  ) {
    val input = """aaa
                  |bbb
                  |
                  |### CCC""".stripMargin
    run(input, p("aaa\nbbb"), Header(3, "CCC"))
  }

  test("atx header - title decorated by six '#' on the beginning of the line as a level 3 header") {
    val input = """aaa
                  |bbb
                  |
                  |###### CCC""".stripMargin
    run(input, p("aaa\nbbb"), Header(6, "CCC"))
  }

  test("atx header - title that includes markup") {
    val input = """aaa
                  |bbb
                  |
                  |##### CCC `DDD` EEE""".stripMargin
    run(input, p("aaa\nbbb"), Header(5, Text("CCC "), Literal("DDD"), Text(" EEE")))
  }

  test("atx header - strip all trailing '#' from the header") {
    val input = """aaa
                  |bbb
                  |
                  |#### CCC DDD EEE ###########""".stripMargin
    run(input, p("aaa\nbbb"), Header(4, "CCC DDD EEE"))
  }

  test("atx header - ignore title lines without title text") {
    val input = """aaa
                  |bbb
                  |
                  |###""".stripMargin
    run(input, p("aaa\nbbb"), p("###"))
  }

  test("atx header - recognize header without preceding blank line") {
    val input = """aaa
                  |bbb
                  |### CCC""".stripMargin
    run(input, BlockSequence(p("aaa\nbbb"), Header(3, "CCC")))
  }

  test("rules - line decorated by '-' and space characters ending on a '-'") {
    val input = """aaa
                  |bbb
                  |
                  |- - - -
                  |
                  |ccc""".stripMargin
    run(input, p("aaa\nbbb"), Rule(), p("ccc"))
  }

  test("rules - line decorated by '-' and space characters ending on several spaces") {
    val input = """aaa
                  |bbb
                  |
                  |- - - -    
                  |
                  |ccc""".stripMargin
    run(input, p("aaa\nbbb"), Rule(), p("ccc"))
  }

  test("rules - line decorated by '-' and space characters even if the number of spaces varies") {
    val input = """aaa
                  |bbb
                  |
                  |- -    - -    
                  |
                  |ccc""".stripMargin
    run(input, p("aaa\nbbb"), Rule(), p("ccc"))
  }

  test(
    "rules - treat a line decorated by '_' and space characters as normal text in case it is followed by other characters"
  ) {
    val input = """aaa
                  |bbb
                  |
                  |_ _ _ _ abc
                  |
                  |ccc""".stripMargin
    run(input, p("aaa\nbbb"), p("_ _ _ _ abc"), p("ccc"))
  }

  test(
    "rules - treat a line decorated by '_' and space characters as normal text in case the pattern is repeated less than 3 times"
  ) {
    val input = """aaa
                  |bbb
                  |
                  |_ _
                  |
                  |ccc""".stripMargin
    run(input, p("aaa\nbbb"), p("_ _"), p("ccc"))
  }

  test("rules - line decorated by '-' and space characters indented up to 3 spaces") {
    val input = """aaa
                  |bbb
                  |
                  |   _ _ _ _    
                  |
                  |ccc""".stripMargin
    run(input, p("aaa\nbbb"), Rule(), p("ccc"))
  }

  test("rules - line decorated by '-' without space characters") {
    val input = """aaa
                  |bbb
                  |
                  |-----    
                  |
                  |ccc""".stripMargin
    run(input, p("aaa\nbbb"), Rule(), p("ccc"))
  }

  test("rules - line decorated by '_' and space characters") {
    val input = """aaa
                  |bbb
                  |
                  |_ _ _ _    
                  |
                  |ccc""".stripMargin
    run(input, p("aaa\nbbb"), Rule(), p("ccc"))
  }

  test("rules - line decorated by '*' and space characters") {
    val input = """aaa
                  |bbb
                  |
                  | *  *  *    
                  |
                  |ccc""".stripMargin
    run(input, p("aaa\nbbb"), Rule(), p("ccc"))
  }

  test("link target without title and url without angle brackets") {
    val input = """[def]: http://foo/""".stripMargin
    run(input, LinkDefinition("def", ExternalTarget("http://foo/"), None))
  }

  test("link target without title and url in angle brackets") {
    val input = """[def]: <http://foo/>""".stripMargin
    run(input, LinkDefinition("def", ExternalTarget("http://foo/"), None))
  }

  test("link target with title enclosed in double quotes") {
    val input = """[def]: <http://foo/> "Some Title"   """.stripMargin
    run(input, LinkDefinition("def", ExternalTarget("http://foo/"), Some("Some Title")))
  }

  test("link target with title enclosed in single quotes") {
    val input = """[def]: <http://foo/> 'Some Title'   """.stripMargin
    run(input, LinkDefinition("def", ExternalTarget("http://foo/"), Some("Some Title")))
  }

  test("link target with title enclosed in parentheses") {
    val input = """[def]: <http://foo/> (Some Title)""".stripMargin
    run(input, LinkDefinition("def", ExternalTarget("http://foo/"), Some("Some Title")))
  }

  test("link target with the title indented on the following line") {
    val input = """[def]: <http://foo/> 
                  |       (Some Title)""".stripMargin
    run(input, LinkDefinition("def", ExternalTarget("http://foo/"), Some("Some Title")))
  }

  test("link target ignoring the title when it is following after a blank line") {
    val input = """[def]: <http://foo/> 
                  |
                  |       (Some Title)""".stripMargin
    run(
      input,
      LinkDefinition("def", ExternalTarget("http://foo/"), None),
      LiteralBlock("   (Some Title)")
    )
  }

  test("internal target") {
    val input = """[def]: ../foo/bar.md#xy""".stripMargin
    run(input, LinkDefinition("def", InternalTarget(RelativePath.parse("../foo/bar.md#xy"))))
  }

  test("internal target with title enclosed in parentheses") {
    val input = """[def]: foo/bar.md#xy (Some Title)""".stripMargin
    run(
      input,
      LinkDefinition("def", InternalTarget(RelativePath.parse("foo/bar.md#xy")), Some("Some Title"))
    )
  }

  test("code block nested inside a list") {
    val input    = """* aaa
                  |* bbb
                  |
                  |        code
                  |* ccc""".stripMargin
    val expected = BulletList(
      Seq(p("aaa")),
      Seq(p("bbb"), LiteralBlock("code")),
      Seq(p("ccc"))
    )
    run(input, expected)
  }

  test("blockquote nested inside a list") {
    val input    = """* aaa
                  |* bbb
                  |
                  |  >quote
                  |* ccc""".stripMargin
    val expected = BulletList(
      Seq(p("aaa")),
      Seq(p("bbb"), QuotedBlock("quote")),
      Seq(p("ccc"))
    )
    run(input, expected)
  }

  test("list nested inside a blockquote") {
    val input = """>aaa
                  |>bbb
                  |>
                  |>* ccc
                  |>* ddd""".stripMargin
    run(input, QuotedBlock(p("aaa\nbbb"), BulletList("ccc", "ddd")))
  }

  test("code block nested inside a blockquote") {
    val input = """>aaa
                  |>bbb
                  |>
                  |>     code
                  |>
                  |>ccc""".stripMargin
    run(input, QuotedBlock(p("aaa\nbbb"), LiteralBlock("code"), p("ccc")))
  }

}
