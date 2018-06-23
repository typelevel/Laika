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

package laika.parse.markdown

import laika.api.ext.ParserDefinitionBuilders
import laika.parse.core.Parser
import laika.parse.helper.{DefaultParserHelpers, ParseResultHelpers}
import laika.tree.Elements._
import laika.tree.helper.ModelBuilder
import org.scalatest.{FlatSpec, Matchers}
    
class BlockParsersSpec extends FlatSpec 
                       with Matchers 
                       with ParseResultHelpers
                       with DefaultParserHelpers[RootElement] 
                       with ModelBuilder {


  val rootParser = new RootParser(Map(), Map(), ParserDefinitionBuilders(), verbatimHTML = false, isStrict = true)

  val defaultParser: Parser[RootElement] = rootParser.rootElement
  
  
  def fp (content: String) = ForcedParagraph(List(Text(content)))
  
  def ss (content: String) = SpanSequence(List(Text(content)))
  
  
  "The paragraph parser" should "parse blocks without block-level markup as normal paragraphs" in {
    val input = """aaa
      |bbb
      |ccc
      |
      |ddd
      |eee""".stripMargin
    Parsing (input) should produce (root( p("aaa\nbbb\nccc"), p("ddd\neee")))
  }
  
  it should "parse a double space at a line end as a hard line break" in {
    Parsing ("some text  \nsome more") should produce (root( p(txt("some text"), LineBreak(), txt("\nsome more"))))
  }
  
  
  
  "The list parser" should "parse items that are not separated by blank lines as list items with flow content" in {
    val input = """* aaa
      |* bbb
      |* ccc""".stripMargin
    Parsing (input) should produce (root(bulletList() + "aaa" + "bbb" + "ccc"))
  }
  
  it should "parse items that are separated by blank lines as list items with paragraph" in {
    val input = """* aaa
      |
      |* bbb
      |
      |* ccc""".stripMargin
    Parsing (input) should produce (root(bulletList() + fp("aaa") + fp("bbb") + fp("ccc")))
  }
  
  it should "parse items indented by a tab after the '*' in the same way as items indented by a space" in {
    val input = """*	aaa
      |*	bbb
      |*	ccc""".stripMargin
    Parsing (input) should produce (root(bulletList() + "aaa" + "bbb" + "ccc"))
  }
  
  it should "parse items starting with a '+' the same way as those starting with a '*'" in {
    val input = """+ aaa
      |+ bbb
      |+ ccc""".stripMargin
    Parsing (input) should produce (root(bulletList("+") + "aaa" + "bbb" + "ccc"))
  }
  
  it should "parse items starting with a '-' the same way as those starting with a '*'" in {
    val input = """- aaa
      |- bbb
      |- ccc""".stripMargin
    Parsing (input) should produce (root(bulletList("-") + "aaa" + "bbb" + "ccc"))
  }
  
  it should "parse items prefixed by numbers as items of an enumerated list" in {
    val input = """1. aaa
      |2. bbb
      |3. ccc""".stripMargin
    Parsing (input) should produce (root(enumList() + "aaa" + "bbb" + "ccc"))
  }
  
  it should "parse items prefixed by numbers and separated by blank lines as ordered list items with paragraph" in {
    val input = """1. aaa
      |
      |2. bbb
      |
      |3. ccc""".stripMargin
    Parsing (input) should produce (root(enumList() + fp("aaa") + fp("bbb") + fp("ccc")))
  }
  
  it should "parse items prefixed by numbers containing multiple paragraphs in a single item" in {
    val input = """1. aaa
      |   
      |   bbb
      |   bbb
      |
      |2. ccc
      |
      |3. ddd""".stripMargin
    Parsing (input) should produce (root( enumList() + (p("aaa"), p("bbb\nbbb")) + fp("ccc") + fp("ddd")))
  }
  
  it should "parse nested items indented by spaces" in {
    val input = """*   aaa
                  |    *   bbb
                  |        * ccc""".stripMargin

    val list3 = bulletList() + "ccc"
    val list2 = bulletList() + (ss("bbb"), list3)
    val list1 = bulletList() + (ss("aaa"), list2)

    Parsing (input) should produce (root(list1))
  }
  
  it should "parse nested items indented by tabs" in {
    val input = """*	aaa
      |	* bbb
      |		* ccc""".stripMargin

    val list3 = bulletList() + "ccc"
    val list2 = bulletList() + (ss("bbb"), list3)
    val list1 = bulletList() + (ss("aaa"), list2)
    
    Parsing (input) should produce (root(list1))
  }
  
  it should "parse a bullet list nested inside an enumerated list" in {
    val input = """1.  111
      |2.  222
      |    * aaa
      |    * bbb
      |    * ccc
      |3. 333""".stripMargin
      
    val nestedList = bulletList() + "aaa" + "bbb" + "ccc"
    
    Parsing (input) should produce (root(enumList() + "111" + (ss("222"), nestedList) + "333"))
  }
  
  it should "parse a bullet list nested inside an enumerated list with blank lines between the items" in {
    val input = """1.  111
      |
      |2.  222
      |    * aaa
      |    * bbb
      |    * ccc
      |
      |3. 333""".stripMargin
      
    val nestedList = bulletList() + "aaa" + "bbb" + "ccc"
    
    Parsing (input) should produce (root(enumList() + fp("111") + (p("222"), nestedList) + fp("333")))
  }
  
  it should "parse a list nested between two paragraphs inside a list item" in {
    val input = """*	aaa
      |
      |	*	bbb
      |
      |	ccc""".stripMargin
      
    val nestedList = bulletList() + "bbb"
    
    Parsing (input) should produce (root( bulletList() + (p("aaa"), nestedList, p("ccc"))))
  }
  
  
  
  "The blockquote parser" should "parse a paragraph decorated with '>' at the beginning of each line" in {
    val input = """>aaa
      |>bbb
      |>ccc""".stripMargin
    Parsing (input) should produce (root( quote("aaa\nbbb\nccc")))
  }
  
  it should "parse a paragraph decorated with '>' only at the beginning of the first line" in {
    val input = """>aaa
      |bbb
      |ccc""".stripMargin
    Parsing (input) should produce (root( quote("aaa\nbbb\nccc")))
  }
  
  it should "parse two adjacent paragraphs decorated with '>' as a single blockquote with two paragraphs" in {
    val input = """>aaa
      |bbb
      |ccc
      |
      |>ddd
      |>eee
      |>fff""".stripMargin
    Parsing (input) should produce (root( quote(p("aaa\nbbb\nccc"),p("ddd\neee\nfff"))))
  }
  
  it should "parse a nested blockquote decorated with a second '>'" in {
    val input = """>aaa
      |>
      |>>bbb
      |>
      |>ccc""".stripMargin
    Parsing (input) should produce (root( quote(p("aaa"), quote("bbb"), p("ccc"))))
  }
  
  
  
  "The literal block parser" should "parse paragraphs indented with 4 or more spaces as a code block" in {
    val input = """    code
      |
      |text
      |
      |    code
      |
      |text
      |
      |    code""".stripMargin
    Parsing (input) should produce (root( litBlock("code"), p("text"), litBlock("code"), p("text"), litBlock("code")))
  }
  
  it should "parse paragraphs indented with 1 or more tabs as a code block" in {
    val input = """	code
      |
      |text
      |
      |	code
      |
      |text
      |
      |	code""".stripMargin
    Parsing (input) should produce (root( litBlock("code"), p("text"), litBlock("code"), p("text"), litBlock("code")))
  }
  
  it should "parse indented lines separated by blank lines into a single code block" in {
    val input = """    code 1
      |
      |    code 2
      |
      |    code 3""".stripMargin
    Parsing (input) should produce (root( litBlock("code 1\n\ncode 2\n\ncode 3")))
  }
  
  
  
  "The setext header parser" should "parse a title decorated by one or more '=' on the following line as a level 1 header" in {
    val input = """aaa
      |bbb
      |
      |CCC
      |==""".stripMargin
    Parsing (input) should produce (root( p("aaa\nbbb"), h(1, "CCC")))
  }
  
  it should "parse a title decorated by one or more '-' on the following line as a level 2 header" in {
    val input = """aaa
      |bbb
      |
      |CCC
      |---""".stripMargin
    Parsing (input) should produce (root( p("aaa\nbbb"), h(2, "CCC")))
  }
  
  it should "parse a title that includes markup" in {
    val input = """aaa
      |bbb
      |
      |CCC *DDD* EEE
      |---""".stripMargin
    Parsing (input) should produce (root( p("aaa\nbbb"), h(2, txt("CCC "), em("DDD"), txt(" EEE"))))
  }
  
  
  
  "The atx header parser" should "parse a title decorated by one '#' on the beginning of the line as a level 1 header" in {
    val input = """aaa
      |bbb
      |
      |# CCC""".stripMargin
    Parsing (input) should produce (root( p("aaa\nbbb"), h(1, "CCC")))
  }
  
  it should "parse a title decorated by three '#' on the beginning of the line as a level 3 header" in {
    val input = """aaa
      |bbb
      |
      |### CCC""".stripMargin
    Parsing (input) should produce (root( p("aaa\nbbb"), h(3, "CCC")))
  }

  it should "parse a title decorated by six '#' on the beginning of the line as a level 3 header" in {
    val input = """aaa
      |bbb
      |
      |###### CCC""".stripMargin
    Parsing (input) should produce (root( p("aaa\nbbb"), h(6, "CCC")))
  }
  
  it should "parse a title that includes markup" in {
    val input = """aaa
      |bbb
      |
      |##### CCC `DDD` EEE""".stripMargin
    Parsing (input) should produce (root( p("aaa\nbbb"), h(5, txt("CCC "), lit("DDD"), txt(" EEE"))))
  }
  
  it should "strip all trailing '#' from the header" in {
    val input = """aaa
      |bbb
      |
      |#### CCC DDD EEE ###########""".stripMargin
    Parsing (input) should produce (root( p("aaa\nbbb"), h(4, txt("CCC DDD EEE"))))
  }
  
  it should "ignore title lines without title text" in {
    val input = """aaa
      |bbb
      |
      |###""".stripMargin
    Parsing (input) should produce (root( p("aaa\nbbb"), p("###")))
  }
  
  
  
  "The rule parser" should "parse a line decorated by '-' and space characters ending on a '-' as a horizontal rule" in {
    val input = """aaa
      |bbb
      |
      |- - - -
      |
      |ccc""".stripMargin
    Parsing (input) should produce (root( p("aaa\nbbb"), Rule(), p("ccc")))
  }
  
  it should "parse a line decorated by '-' and space characters ending on several spaces as a horizontal rule" in {
    val input = """aaa
      |bbb
      |
      |- - - -    
      |
      |ccc""".stripMargin
    Parsing (input) should produce (root( p("aaa\nbbb"), Rule(), p("ccc")))
  }
  
  it should "parse a line decorated by '-' and space characters even if the number of spaces varies" in {
    val input = """aaa
      |bbb
      |
      |- -    - -    
      |
      |ccc""".stripMargin
    Parsing (input) should produce (root( p("aaa\nbbb"), Rule(), p("ccc")))
  }
  
  it should "treat a line decorated by '_' and space characters as normal text in case it is followed by other characters" in {
    val input = """aaa
      |bbb
      |
      |_ _ _ _ abc
      |
      |ccc""".stripMargin
    Parsing (input) should produce (root( p("aaa\nbbb"), p("_ _ _ _ abc"), p("ccc")))
  }
  
  it should "treat a line decorated by '_' and space characters as normal text in case the pattern is repeated less than 3 times" in {
    val input = """aaa
      |bbb
      |
      |_ _
      |
      |ccc""".stripMargin
    Parsing (input) should produce (root( p("aaa\nbbb"), p("_ _"), p("ccc")))
  }
  
  it should "parse a line decorated by '-' and space characters indented up to 3 spaces as a horizontal rule" in {
    val input = """aaa
      |bbb
      |
      |   _ _ _ _    
      |
      |ccc""".stripMargin
    Parsing (input) should produce (root( p("aaa\nbbb"), Rule(), p("ccc")))
  }
  
  it should "parse a line decorated by '-' without space characters as a horizontal rule" in {
    val input = """aaa
      |bbb
      |
      |-----    
      |
      |ccc""".stripMargin
    Parsing (input) should produce (root( p("aaa\nbbb"), Rule(), p("ccc")))
  }
  
  it should "parse a line decorated by '_' and space characters" in {
    val input = """aaa
      |bbb
      |
      |_ _ _ _    
      |
      |ccc""".stripMargin
    Parsing (input) should produce (root( p("aaa\nbbb"), Rule(), p("ccc")))
  }
  
  it should "parse a line decorated by '*' and space characters" in {
    val input = """aaa
      |bbb
      |
      | *  *  *    
      |
      |ccc""".stripMargin
    Parsing (input) should produce (root( p("aaa\nbbb"), Rule(), p("ccc")))
  }
  
  
  
  "The link target parser" should "parse a link target without title and url without angle brackets" in {
    val input = """[def]: http://foo/""".stripMargin
    Parsing (input) should produce (root( ExternalLinkDefinition("def", "http://foo/", None)))
  }
  
  it should "parse a link target without title and url in angle brackets" in {
    val input = """[def]: <http://foo/>""".stripMargin
    Parsing (input) should produce (root( ExternalLinkDefinition("def", "http://foo/", None)))
  }
  
  it should "parse a link target with title enclosed in double quotes" in {
    val input = """[def]: <http://foo/> "Some Title"   """.stripMargin
    Parsing (input) should produce (root( ExternalLinkDefinition("def", "http://foo/", Some("Some Title"))))
  }
  
  it should "parse a link target with title enclosed in single quotes" in {
    val input = """[def]: <http://foo/> 'Some Title'   """.stripMargin
    Parsing (input) should produce (root( ExternalLinkDefinition("def", "http://foo/", Some("Some Title"))))
  }
  
  it should "parse a link target with title enclosed in parentheses" in {
    val input = """[def]: <http://foo/> (Some Title)""".stripMargin
    Parsing (input) should produce (root( ExternalLinkDefinition("def", "http://foo/", Some("Some Title"))))
  }
  
  it should "parse a link target with the title indented on the following line" in {
    val input = """[def]: <http://foo/> 
                  |       (Some Title)""".stripMargin
    Parsing (input) should produce (root( ExternalLinkDefinition("def", "http://foo/", Some("Some Title"))))
  }
  
  it should "parse a link target ignoring the title when it is following atfer a blank line" in {
    val input = """[def]: <http://foo/> 
                  |
                  |       (Some Title)""".stripMargin
    Parsing (input) should produce (root( ExternalLinkDefinition("def", "http://foo/", None), litBlock("   (Some Title)")))
  }
  
  
  
  "The block parser" should "parse a code block nested inside a list" in {
    val input = """* aaa
      |* bbb
      |
      |        code
      |* ccc""".stripMargin
    Parsing (input) should produce (root( bulletList() + "aaa" + (p("bbb"), litBlock("code")) + "ccc"))
  }
  
  it should "parse a blockquote nested inside a list" in {
    val input = """* aaa
      |* bbb
      |
      |  >quote
      |* ccc""".stripMargin
    Parsing (input) should produce (root( bulletList() + "aaa" + (p("bbb"), quote("quote")) + "ccc"))
  }
  
  it should "parse a list nested inside a blockquote" in {
    val input = """>aaa
      |>bbb
      |>
      |>* ccc
      |>* ddd""".stripMargin
    Parsing (input) should produce (root( quote( p("aaa\nbbb"), bulletList() + "ccc" + "ddd")))
  }
  
  it should "parse a code block nested inside a blockquote" in {
    val input = """>aaa
      |>bbb
      |>
      |>     code
      |>
      |>ccc""".stripMargin
    Parsing (input) should produce (root( quote( p("aaa\nbbb"), litBlock("code"), p("ccc"))))
  }


}
