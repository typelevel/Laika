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

package laika.directive

import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.bundle.{BlockParser, BlockParserBuilder, ParserBundle}
import laika.parse.Parser
import laika.parse.combinator.Parsers
import laika.parse.markup.RootParserProvider
import laika.parse.directive.BlockDirectiveParsers
import laika.parse.helper.{DefaultParserHelpers, ParseResultHelpers}
import laika.parse.text.TextParsers._
import org.scalatest.{FlatSpec, Matchers}


class BlockDirectiveAPISpec extends FlatSpec
                          with Matchers
                          with ModelBuilder {

  
  object DirectiveSetup {
    import Blocks.dsl._

    trait RequiredDefaultAttribute {
      val directive = Blocks.create("dir") { attribute(Default) map (p(_)) }
    }
    
    trait OptionalDefaultAttribute {
      val directive = Blocks.create("dir") { 
        attribute(Default, positiveInt).optional map (num => p(num.map(_.toString).getOrElse("<>"))) 
      }
    }
    
    trait RequiredNamedAttribute {
      val directive = Blocks.create("dir") { attribute("name") map (p(_)) }
    }
    
    trait OptionalNamedAttribute {
      val directive = Blocks.create("dir") { 
        attribute("name", positiveInt).optional map (num => p(num.map(_.toString).getOrElse("<>"))) 
      }
    }
    
    trait RequiredDefaultBody {
      val directive = Blocks.create("dir") { body(Default) map (BlockSequence(_)) }
    }
    
    trait OptionalDefaultBody {
      val directive = Blocks.create("dir") { 
        body(Default).optional map (blocks => BlockSequence(blocks.getOrElse(Nil))) 
      }
    }
    
    trait RequiredNamedBody {
      val directive = Blocks.create("dir") { body("name") map (BlockSequence(_)) }
    }
    
    trait OptionalNamedBody {
      val directive = Blocks.create("dir") { 
        body("name").optional map (blocks => BlockSequence(blocks.getOrElse(Nil))) 
      }
    }
    
    trait FullDirectiveSpec {
      val directive = Blocks.create("dir") {
        (attribute(Default) ~ attribute("strAttr").optional ~ attribute("intAttr", positiveInt).optional ~
        body(Default) ~ body("blockBody").optional ~ body("intBody", positiveInt).optional) {
          (defAttr, strAttr, intAttr, defBody, blockBody, intBody) => 
            val sum = intAttr.getOrElse(0) + intBody.getOrElse(0)
            val str = defAttr + ":" + strAttr.getOrElse("..") + ":" + sum
            BlockSequence(p(str) +: (defBody ++ blockBody.getOrElse(Nil)))
        }
      }
    }
    
    trait DirectiveWithParserAccess {
      val directive = Blocks.create("dir") { 
        (body(Default, string) ~ parser) {
          (body, parser) => BlockSequence(parser(body.drop(3)))
        }
      }
    }
    
    trait DirectiveWithContextAccess {
      val directive = Blocks.create("dir") { 
        (body(Default, string) ~ cursor) {
          (body, cursor) => p(body + cursor.target.path)
        }
      }
    }
    
  }
  
  trait TemplateParser extends ParseResultHelpers
                          with DefaultParserHelpers[RootElement] {

    def directive: Blocks.Directive

    lazy val directiveSupport: ParserBundle = DirectiveSupport.withDirectives(Seq(directive), Seq(), Seq()).parsers

    lazy val paragraphParser: BlockParserBuilder = BlockParser.withoutStartChar.recursive { recParser =>
      recParser.recursiveSpans(((Parsers.not(blankLine) ~> restOfLine) +) ^^ (_.mkString("\n"))) ^^ { Paragraph(_) }
    }

    lazy val defaultParser: Parser[RootElement] = RootParserProvider.forParsers(
      blockParsers = Seq(paragraphParser),
      markupExtensions = directiveSupport.markupExtensions
    ).rootElement

    def invalid (input: String, error: String): InvalidBlock = InvalidElement(error, input).asBlock

  }


  import DirectiveSetup._

  "The directive parser" should "parse a directive with one required default string attribute" in {
    new TemplateParser with RequiredDefaultAttribute {
      val input = """aa
        |
        |@:dir foo.
        |
        |bb""".stripMargin
      Parsing (input) should produce (root(p("aa"), p("foo"), p("bb")))
    }
  }

  it should "detect a directive with a missing required default attribute" in {
    new TemplateParser with RequiredDefaultAttribute {
      val input = """aa
        |
        |@:dir.
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'dir': required default attribute is missing"
      Parsing (input) should produce (root(p("aa"), invalid("@:dir.",msg), p("bb")))
    }
  }

  it should "parse a directive with an optional default int attribute" in {
    new TemplateParser with OptionalDefaultAttribute {
      val input = """aa
        |
        |@:dir 5.
        |
        |bb""".stripMargin
      Parsing (input) should produce (root(p("aa"), p("5"), p("bb")))
    }
  }

  it should "detect a directive with an optional invalid default int attribute" in {
    new TemplateParser with OptionalDefaultAttribute {
      val input = """aa
        |
        |@:dir foo.
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'dir': error converting default attribute: not an integer: foo"
      Parsing (input) should produce (root(p("aa"), invalid("@:dir foo.",msg), p("bb")))
    }
  }

  it should "parse a directive with a missing optional default int attribute" in {
    new TemplateParser with OptionalDefaultAttribute {
      val input = """aa
        |
        |@:dir.
        |
        |bb""".stripMargin
      Parsing (input) should produce (root(p("aa"), p("<>"), p("bb")))
    }
  }

  it should "parse a directive with one required named string attribute" in {
    val input = """aa
        |
        |@:dir name=foo.
        |
        |bb""".stripMargin
    new TemplateParser with RequiredNamedAttribute {
      Parsing (input) should produce (root(p("aa"), p("foo"), p("bb")))
    }
  }

  it should "parse a directive with a named string attribute value in quotes" in {
    val input = """aa
        |
        |@:dir name="foo bar".
        |
        |bb""".stripMargin
    new TemplateParser with RequiredNamedAttribute {
      Parsing (input) should produce (root(p("aa"), p("foo bar"), p("bb")))
    }
  }

  it should "detect a directive with a missing required named attribute" in {
    new TemplateParser with RequiredNamedAttribute {
      val input = """aa
        |
        |@:dir.
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'dir': required attribute with name 'name' is missing"
      Parsing (input) should produce (root(p("aa"), invalid("@:dir.",msg), p("bb")))
    }
  }

  it should "parse a directive with an optional named int attribute" in {
    new TemplateParser with OptionalNamedAttribute {
      val input = """aa
        |
        |@:dir name=5.
        |
        |bb""".stripMargin
      Parsing (input) should produce (root(p("aa"), p("5"), p("bb")))
    }
  }

  it should "detect a directive with an optional invalid named int attribute" in {
    new TemplateParser with OptionalNamedAttribute {
      val input = """aa
        |
        |@:dir name=foo.
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'dir': error converting attribute with name 'name': not an integer: foo"
      Parsing (input) should produce (root(p("aa"), invalid("@:dir name=foo.",msg), p("bb")))
    }
  }

  it should "parse a directive with a missing optional named int attribute" in {
    new TemplateParser with OptionalNamedAttribute {
      val input = """aa
        |
        |@:dir.
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'dir': required default attribute is missing"
      Parsing (input) should produce (root(p("aa"), p("<>"), p("bb")))
    }
  }

  it should "parse a directive with a required default body on the same line" in {
    new TemplateParser with RequiredDefaultBody {
      val input = """aa
        |
        |@:dir: some {{ref}} text
        |
        |bb""".stripMargin
      val body = BlockSequence(List(p(txt("some "), MarkupContextReference("ref"), txt(" text"))))
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }

  it should "parse a directive with a required default body on the following lines" in {
    new TemplateParser with RequiredDefaultBody {
      val input = """aa
        |
        |@:dir:
        |  some
        |  {{ref}}
        |  text
        |
        |bb""".stripMargin
      val body = BlockSequence(List(p(txt("some\n"), MarkupContextReference("ref"), txt("\ntext"))))
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }

  it should "detect a directive with a missing required default body" in {
    new TemplateParser with RequiredDefaultBody {
      val input = """aa
        |
        |@:dir.
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'dir': required default body is missing"
      Parsing (input) should produce (root(p("aa"), invalid("@:dir.",msg), p("bb")))
    }
  }

  it should "parse a directive with an optional default body" in {
    new TemplateParser with OptionalDefaultBody {
      val input = """aa
        |
        |@:dir: some {{ref}} text
        |
        |bb""".stripMargin
      val body = BlockSequence(List(p(txt("some "), MarkupContextReference("ref"), txt(" text"))))
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }

  it should "parse a directive with a missing optional default body" in {
    new TemplateParser with OptionalDefaultBody {
      val input = """aa
        |
        |@:dir.
        |
        |bb""".stripMargin
      Parsing (input) should produce (root(p("aa"), BlockSequence(Nil), p("bb")))
    }
  }

  it should "parse a directive with a required named body" in {
    new TemplateParser with RequiredNamedBody {
      val input = """aa
        |
        |@:dir: ~name: some {{ref}} text
        |
        |bb""".stripMargin
      val body = BlockSequence(List(p(txt("some "), MarkupContextReference("ref"), txt(" text"))))
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }

  it should "detect a directive with a missing required named body" in {
    new TemplateParser with RequiredNamedBody {
      val input = """aa
        |
        |@:dir.
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'dir': required body with name 'name' is missing"
      Parsing (input) should produce (root(p("aa"), invalid("@:dir.",msg), p("bb")))
    }
  }

  it should "parse a directive with an optional named body" in {
    new TemplateParser with OptionalNamedBody {
      val input = """aa
        |
        |@:dir: ~name: some {{ref}} text
        |
        |bb""".stripMargin
      val body = BlockSequence(List(p(txt("some "), MarkupContextReference("ref"), txt(" text"))))
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }

  it should "parse a directive with a missing optional named body" in {
    new TemplateParser with OptionalNamedBody {
      val input = """aa
        |
        |@:dir.
        |
        |bb""".stripMargin
      Parsing (input) should produce (root(p("aa"), BlockSequence(Nil), p("bb")))
    }
  }

  it should "parse a full directive spec with all elements present" in {
    new FullDirectiveSpec with TemplateParser {
      val input = """aa
        |
        |@:dir foo strAttr=str intAttr=7:
        |  1 {{ref1}} 2
        |~blockBody:
        |  3 {{ref3}} 4
        |~intBody: 9
        |
        |bb""".stripMargin
      val body = BlockSequence(List(
        p("foo:str:16"),
        p(txt("1 "), MarkupContextReference("ref1"), txt(" 2")),
        p(txt("3 "), MarkupContextReference("ref3"), txt(" 4"))
      ))
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }

  it should "parse a full directive spec with all elements present and blank lines between blocks" in {
    new FullDirectiveSpec with TemplateParser {
      val input = """aa
        |
        |@:dir foo strAttr=str intAttr=7:
        |  1 {{ref1}} 2
        |
        |~blockBody:
        |  3 {{ref3}} 4
        |
        |~intBody: 9
        |
        |bb""".stripMargin
      val body = BlockSequence(List(
        p("foo:str:16"),
        p(txt("1 "), MarkupContextReference("ref1"), txt(" 2")),
        p(txt("3 "), MarkupContextReference("ref3"), txt(" 4"))
      ))
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }

  it should "parse a full directive spec with all optional elements missing" in {
    new FullDirectiveSpec with TemplateParser {
      val input = """aa
        |
        |@:dir foo:
        |  1 {{ref1}} 2
        |
        |bb""".stripMargin
      val body = BlockSequence(List(
        p("foo:..:0"),
        p(txt("1 "), MarkupContextReference("ref1"), txt(" 2"))
      ))
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }

  it should "detect a full directive spec with one required attribute and one required body missing" in {
    new FullDirectiveSpec with TemplateParser {
      val input = """aa
        |
        |@:dir strAttr=str.
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'dir': required default attribute is missing, required default body is missing"
      Parsing (input) should produce (root(p("aa"), invalid("@:dir strAttr=str.",msg), p("bb")))
    }
  }

  it should "parse a directive with a required default body and parser access" in {
    new TemplateParser with DirectiveWithParserAccess {
      val input = """aa
        |
        |@:dir: some {{ref}} text
        |
        |bb""".stripMargin
      val body = BlockSequence(List(p(txt("e "), MarkupContextReference("ref"), txt(" text"))))
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }

  it should "parse a directive with a required default body and cursor access" in {
    new TemplateParser with DirectiveWithContextAccess {
      val input = """aa
        |
        |@:dir: text
        |
        |bb""".stripMargin
      def translate (result: RootElement) = result rewrite {
        case _: BlockDirectiveParsers.DirectiveBlock => Some(p("ok")) // cannot compare DirectiveSpans
      }
      Parsing (input) map translate should produce (root(p("aa"), p("ok"), p("bb")))
    }
  }
  
  it should "detect a directive with an unknown name" in {
    new TemplateParser with OptionalNamedAttribute {
      val input = """aa
        |
        |@:foo name=foo.
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'foo': No block directive registered with name: foo"
      Parsing (input) should produce (root(p("aa"), invalid("@:foo name=foo.",msg), p("bb")))
    }
  }
  
  
}
