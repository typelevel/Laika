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

package laika.directive

import cats.implicits._
import laika.config.ConfigBuilder
import laika.ast.Path.Root
import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.bundle.{BlockParser, BlockParserBuilder, ParserBundle}
import laika.parse.Parser
import laika.parse.combinator.Parsers
import laika.parse.helper.{DefaultParserHelpers, ParseResultHelpers}
import laika.parse.markup.RootParserProvider
import laika.parse.text.TextParsers._
import laika.rewrite.TemplateRewriter
import org.scalatest.{FlatSpec, Matchers}


class LegacyBlockDirectiveAPISpec extends FlatSpec
                          with Matchers
                          with ModelBuilder {

  
  object DirectiveSetup {
    import Blocks.dsl._

    trait RequiredDefaultAttribute {
      val directive = Blocks.create("dir") { defaultAttribute.as[String] map p }
    }
    
    trait OptionalDefaultAttribute {
      val directive = Blocks.create("dir") {
        defaultAttribute.as[Int].optional map (num => p(num.map(_.toString).getOrElse("<>"))) 
      }
    }
    
    trait RequiredNamedAttribute {
      val directive = Blocks.create("dir") { attribute("name").as[String] map p }
    }
    
    trait OptionalNamedAttribute {
      val directive = Blocks.create("dir") { 
        attribute("name").as[Int].optional map (num => p(num.map(_.toString).getOrElse("<>"))) 
      }
    }
    
    trait RequiredDefaultBody {
      val directive = Blocks.create("dir") { parsedBody map (BlockSequence(_)) }
    }
    
    trait FullDirectiveSpec {
      val directive = Blocks.create("dir") {
        (defaultAttribute.as[String], attribute("strAttr").as[String].optional, attribute("intAttr").as[Int].optional, parsedBody).mapN {
          (defAttr, strAttr, intAttr, defBody) =>
            val sum = intAttr.getOrElse(0)
            val str = defAttr + ":" + strAttr.getOrElse("..") + ":" + sum
            BlockSequence(p(str) +: defBody)
        }
      }
    }
    
    trait DirectiveWithParserAccess {
      val directive = Blocks.create("dir") { 
        (rawBody, parser).mapN { (body, parser) =>
          BlockSequence(parser(body.drop(3)))
        }
      }
    }
    
    trait DirectiveWithContextAccess {
      val directive = Blocks.create("dir") { 
        (rawBody, cursor).mapN { (body, cursor) =>
          p(body + cursor.target.path)
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
    ).rootElement ^^ { root =>
      TemplateRewriter.rewriteRules(DocumentCursor(
        Document(Root, root, config = ConfigBuilder.empty.withValue("ref", "value").build)
      )).rewriteBlock(root).asInstanceOf[RootElement]
    }

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
      val msg = "One or more errors processing directive 'dir': required attribute 'name' is missing"
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
      val msg = "One or more errors processing directive 'dir': error converting attribute 'name': not an integer: foo"
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
        |@:dir: some {{config.ref}} text
        |
        |bb""".stripMargin
      val body = BlockSequence(List(p(txt("some value text"))))
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }

  it should "parse a directive with a required default body on the following lines" in {
    new TemplateParser with RequiredDefaultBody {
      val input = """aa
        |
        |@:dir:
        |  some
        |  {{config.ref}}
        |  text
        |
        |bb""".stripMargin
      val body = BlockSequence(List(p(txt("some\nvalue\ntext"))))
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
      val msg = "One or more errors processing directive 'dir': required body is missing"
      Parsing (input) should produce (root(p("aa"), invalid("@:dir.",msg), p("bb")))
    }
  }

  it should "parse a full directive spec with all elements present" in {
    new FullDirectiveSpec with TemplateParser {
      val input = """aa
        |
        |@:dir foo strAttr=str intAttr=7:
        |  1 {{config.ref}} 2
        |
        |bb""".stripMargin
      val body = BlockSequence(List(
        p("foo:str:7"),
        p(txt("1 value 2"))
      ))
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }

  it should "parse a full directive spec with all optional elements missing" in {
    new FullDirectiveSpec with TemplateParser {
      val input = """aa
        |
        |@:dir foo:
        |  1 {{config.ref}} 2
        |
        |bb""".stripMargin
      val body = BlockSequence(List(
        p("foo:..:0"),
        p(txt("1 value 2"))
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
      val msg = "One or more errors processing directive 'dir': required default attribute is missing, required body is missing"
      Parsing (input) should produce (root(p("aa"), invalid("@:dir strAttr=str.",msg), p("bb")))
    }
  }

  it should "parse a directive with a required default body and parser access" in {
    new TemplateParser with DirectiveWithParserAccess {
      val input = """aa
        |
        |@:dir: some {{config.ref}} text
        |
        |bb""".stripMargin
      val body = BlockSequence(List(p(txt("e value text"))))
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
      Parsing (input) should produce (root(p("aa"), p("text/"), p("bb")))
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
