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
import laika.parse.builders._
import laika.parse.implicits._
import laika.rewrite.TemplateRewriter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class BlockDirectiveAPISpec extends AnyFlatSpec
                          with Matchers
                          with ModelBuilder {

  
  object DirectiveSetup {
    import Blocks.dsl._

    trait Empty {
      val directive = Blocks.create("dir")(Blocks.dsl.empty(p("foo")))
    }
    
    trait RequiredPositionalAttribute {
      val directive = Blocks.create("dir") { attribute(0).as[String] map p }
    }
    
    trait OptionalPositionalAttribute {
      val directive = Blocks.create("dir") {
        attribute(0).as[Int].optional map (num => p(num.map(_.toString).getOrElse("<>"))) 
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

    trait AllAttributes {
      val directive = Blocks.create("dir") {
        allAttributes.map { attrs =>
          val foo = attrs.get[String]("foo").toOption.get
          val bar = attrs.get[Int]("bar").toOption.get
          p(s"$foo $bar")
        }
      }
    }
    
    trait Body {
      val directive = Blocks.create("dir") { parsedBody map (BlockSequence(_)) }
    }

    trait SeparatedBody {
      
      sealed trait Child extends Product with Serializable
      case class Foo (content: Seq[Block]) extends Child
      case class Bar (content: Seq[Block], attr: String) extends Child
      
      val sep1 = Blocks.separator("foo", min = 1) {
        parsedBody.map(Foo)
      }
      val sep2 = Blocks.separator("bar", max = 1) {
        (parsedBody, attribute(0).as[String]).mapN(Bar)
      }
      
      val directive = Blocks.create("dir") { separatedBody[Child](Seq(sep1, sep2)) map { multipart =>
        val seps = multipart.children.flatMap {
          case Foo(content) => p("foo") +: content
          case Bar(content, attr) => p(attr) +: content
        } 
        BlockSequence(multipart.mainBody ++ seps)
      }}
    }
    
    trait FullDirectiveSpec {
      val directive = Blocks.create("dir") {
        (attribute(0).as[String], attribute(1).as[Int], attribute("strAttr").as[String].optional, attribute("intAttr").as[Int].optional, parsedBody).mapN {
          (posStr, posInt, strAttr, intAttr, defBody) =>
            val sum = intAttr.getOrElse(0) + posInt
            val str = posStr + ":" + strAttr.getOrElse("..") + ":" + sum
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
  
  trait BlockParser extends ParseResultHelpers
                          with DefaultParserHelpers[RootElement] {

    def directive: Blocks.Directive

    lazy val directiveSupport: ParserBundle = DirectiveSupport.withDirectives(Seq(directive, StandardDirectives.blockStyle), Nil, Nil, Nil).parsers

    lazy val paragraphParser: BlockParserBuilder = BlockParser.recursive { recParser =>
      recParser.recursiveSpans((Parsers.not(blankLine) ~> restOfLine).rep.min(1).mkLines) ^^ { Paragraph(_) }
    }

    lazy val defaultParser: Parser[RootElement] = RootParserProvider.forParsers(
      blockParsers = Seq(paragraphParser),
      markupExtensions = directiveSupport.markupExtensions
    ).rootElement.map { root =>
      TemplateRewriter.rewriteRules(DocumentCursor(
        Document(Root, root, config = ConfigBuilder.empty.withValue("ref", "value").build)
      )).rewriteBlock(root).asInstanceOf[RootElement]
    }

    def invalid (input: String, error: String): InvalidBlock = InvalidElement(error, input).asBlock

  }


  import DirectiveSetup._

  "The directive parser" should "parse an empty directive" in {
    new BlockParser with Empty {
      val input = """aa
        |
        |@:dir
        |
        |bb""".stripMargin
      Parsing (input) should produce (root(p("aa"), p("foo"), p("bb")))
    }
  }
  
  it should "parse a directive with one required default string attribute" in {
    new BlockParser with RequiredPositionalAttribute {
      val input = """aa
        |
        |@:dir(foo)
        |
        |bb""".stripMargin
      Parsing (input) should produce (root(p("aa"), p("foo"), p("bb")))
    }
  }

  it should "detect a directive with a missing positional default attribute" in {
    new BlockParser with RequiredPositionalAttribute {
      val input = """aa
        |
        |@:dir
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'dir': required positional attribute at index 0 is missing"
      Parsing (input) should produce (root(p("aa"), invalid("@:dir",msg), p("bb")))
    }
  }

  it should "parse a directive with an optional default int attribute" in {
    new BlockParser with OptionalPositionalAttribute {
      val input = """aa
        |
        |@:dir(5)
        |
        |bb""".stripMargin
      Parsing (input) should produce (root(p("aa"), p("5"), p("bb")))
    }
  }

  it should "detect a directive with an optional invalid default int attribute" in {
    new BlockParser with OptionalPositionalAttribute {
      val input = """aa
        |
        |@:dir(foo)
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'dir': error converting positional attribute at index 0: not an integer: foo"
      Parsing (input) should produce (root(p("aa"), invalid("@:dir(foo)",msg), p("bb")))
    }
  }

  it should "parse a directive with a missing optional default int attribute" in {
    new BlockParser with OptionalPositionalAttribute {
      val input = """aa
        |
        |@:dir
        |
        |bb""".stripMargin
      Parsing (input) should produce (root(p("aa"), p("<>"), p("bb")))
    }
  }

  it should "parse a directive with one required named string attribute" in {
    val input = """aa
        |
        |@:dir { name=foo }
        |
        |bb""".stripMargin
    new BlockParser with RequiredNamedAttribute {
      Parsing (input) should produce (root(p("aa"), p("foo"), p("bb")))
    }
  }

  it should "parse a directive with a named string attribute value in quotes" in {
    val input = """aa
        |
        |@:dir { name="foo bar" }
        |
        |bb""".stripMargin
    new BlockParser with RequiredNamedAttribute {
      Parsing (input) should produce (root(p("aa"), p("foo bar"), p("bb")))
    }
  }

  it should "detect a directive with a missing required named attribute" in {
    new BlockParser with RequiredNamedAttribute {
      val input = """aa
        |
        |@:dir
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'dir': required attribute 'name' is missing"
      Parsing (input) should produce (root(p("aa"), invalid("@:dir",msg), p("bb")))
    }
  }

  it should "parse a directive with an optional named int attribute" in {
    new BlockParser with OptionalNamedAttribute {
      val input = """aa
        |
        |@:dir { name=5 }
        |
        |bb""".stripMargin
      Parsing (input) should produce (root(p("aa"), p("5"), p("bb")))
    }
  }

  it should "detect a directive with an optional invalid named int attribute" in {
    new BlockParser with OptionalNamedAttribute {
      val input = """aa
        |
        |@:dir { name=foo }
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'dir': error converting attribute 'name': not an integer: foo"
      Parsing (input) should produce (root(p("aa"), invalid("@:dir { name=foo }",msg), p("bb")))
    }
  }

  it should "parse a directive with a missing optional named int attribute" in {
    new BlockParser with OptionalNamedAttribute {
      val input = """aa
        |
        |@:dir
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'dir': required positional attribute at index 0 is missing"
      Parsing (input) should produce (root(p("aa"), p("<>"), p("bb")))
    }
  }

  it should "parse a directive with the allAttributes combinator" in {
    new BlockParser with AllAttributes {
      val input = """aa
        |
        |@:dir { foo=Planet, bar=42 }
        |
        |bb""".stripMargin
      Parsing (input) should produce (root(p("aa"), p("Planet 42"), p("bb")))
    }
  }

  it should "parse a directive with the allAttributes combinator and an additional nested config object" in {
    new BlockParser with AllAttributes {
      val input = """aa
        |
        |@:dir { 
        |  foo=Planet 
        |  bar=42
        |  nested { xx = 9 } 
        |}
        |
        |bb""".stripMargin
      Parsing (input) should produce (root(p("aa"), p("Planet 42"), p("bb")))
    }
  }

  it should "parse a directive with a body" in {
    new BlockParser with Body {
      val input = """aa
        |
        |@:dir
        |some
        |${ref}
        |text
        |@:@
        |
        |bb""".stripMargin
      val body = BlockSequence(p(Text("some\nvalue\ntext")))
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }

  it should "parse a directive with a body and trim empty lines at the start and end" in {
    new BlockParser with Body {
      val input = """aa
        |
        |@:dir
        |
        |some
        |${ref}
        |text
        |  
        |@:@
        |
        |bb""".stripMargin
      val body = BlockSequence(p(Text("some\nvalue\ntext")))
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }

  it should "parse a directive with a body and a custom fence" in {
    new BlockParser with Body {
      val input = """aa
        |
        |@:dir +++
        |
        |some
        |${ref}
        |text
        |  
        |+++
        |
        |bb""".stripMargin
      val body = BlockSequence(p(Text("some\nvalue\ntext")))
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }

  it should "detect a directive with a missing body" in {
    new BlockParser with Body {
      val input = """aa
        |
        |@:dir
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'dir': required body is missing"
      Parsing (input) should produce (root(p("aa"), invalid("@:dir",msg), p("bb")))
    }
  }

  it should "parse a directive with a separated body" in {
    new BlockParser with SeparatedBody {
      val input = """aa
        |
        |@:dir
        |aaa
        |
        |@:foo
        |bbb
        |
        |@:bar(baz)
        |ccc
        |@:@
        |
        |bb""".stripMargin
      val body = BlockSequence(p("aaa"),p("foo"),p("bbb"),p("baz"),p("ccc"))
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }

  it should "detect a directive with an invalid separator" in new BlockParser with SeparatedBody {
    val input = """aa
      |
      |@:dir
      |aaa
      |
      |@:foo
      |bbb
      |
      |@:bar
      |ccc
      |@:@
      |
      |bb""".stripMargin
    val msg = "One or more errors processing directive 'dir': One or more errors processing separator directive 'bar': required positional attribute at index 0 is missing"
    val src = input.split("\n").toSeq.slice(2, 11).mkString("\n")
    Parsing (input) should produce (root(p("aa"), invalid(src,msg), p("bb")))
  }

  it should "detect a directive with a separator not meeting the min count requirements" in {
    new BlockParser with SeparatedBody {
      val input = """aa
        |
        |@:dir
        |aaa
        |
        |@:bar(baz)
        |ccc
        |@:@
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'dir': too few occurrences of separator directive 'foo': expected min: 1, actual: 0"
      val src = input.split("\n").toSeq.drop(2).dropRight(2).mkString("\n")
      Parsing (input) should produce (root(p("aa"), invalid(src,msg), p("bb")))
    }
  }

  it should "detect a directive with a separator exceeding the max count constraint" in {
    new BlockParser with SeparatedBody {
      val input = """aa
        |
        |@:dir
        |aaa
        |
        |@:foo
        |bbb
        |
        |@:bar(baz)
        |ccc
        |
        |@:bar(baz)
        |ddd
        |
        |@:@
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'dir': too many occurrences of separator directive 'bar': expected max: 1, actual: 2"
      val src = input.split("\n").toSeq.drop(2).dropRight(2).mkString("\n")
      Parsing (input) should produce (root(p("aa"), invalid(src,msg), p("bb")))
    }
  }
  
  it should "detect an orphaned separator directive" in new BlockParser with SeparatedBody {
    val input = """aa
      |
      |@:foo
      |
      |bb""".stripMargin
    val msg = "Orphaned separator directive with name 'foo'"
    Parsing (input) should produce (root(p("aa"), invalid("@:foo",msg), p("bb")))
  }

  it should "parse a full directive spec with all elements present" in {
    new FullDirectiveSpec with BlockParser {
      val input = """aa
        |
        |@:dir(foo, 4) { strAttr=str, intAttr=7 }
        |
        |1 ${ref} 2
        |
        |@:@
        |
        |bb""".stripMargin
      val body = BlockSequence(
        p("foo:str:11"),
        p(Text("1 value 2"))
      )
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }

  it should "parse a full directive spec with all elements present and attributes on multiple lines" in {
    new FullDirectiveSpec with BlockParser {
      val input = """aa
        |
        |@:dir(foo, 4) {
        |  strAttr=str 
        |  intAttr=7 
        |}
        |
        |1 ${ref} 2
        |
        |@:@
        |
        |bb""".stripMargin
      val body = BlockSequence(
        p("foo:str:11"),
        p(Text("1 value 2"))
      )
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }

  it should "parse a full directive spec with all optional elements missing" in {
    new FullDirectiveSpec with BlockParser {
      val input = """aa
        |
        |@:dir(foo, 4)
        | 
        |1 ${ref} 2
        |
        |@:@
        |
        |bb""".stripMargin
      val body = BlockSequence(
        p("foo:..:4"),
        p(Text("1 value 2"))
      )
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }

  it should "parse a full directive spec with a custom fence" in {
    new FullDirectiveSpec with BlockParser {
      val input = """aa
        |
        |@:dir(foo, 4) +++
        |
        |1 ${ref} 2
        |
        |+++
        |
        |bb""".stripMargin
      val body = BlockSequence(
        p("foo:..:4"),
        p(Text("1 value 2"))
      )
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }

  it should "detect a full directive spec with positional attributes and the body missing" in {
    new FullDirectiveSpec with BlockParser {
      val input = """aa
        |
        |@:dir { strAttr=str }
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'dir': required positional attribute at index 0 is missing, required positional attribute at index 1 is missing, required body is missing"
      Parsing (input) should produce (root(p("aa"), invalid("@:dir { strAttr=str }",msg), p("bb")))
    }
  }

  it should "parse a directive with a body and parser access" in {
    new BlockParser with DirectiveWithParserAccess {
      val input = """aa
        |
        |@:dir
        |some ${ref} text
        |@:@
        |
        |bb""".stripMargin
      val body = BlockSequence("e value text")
      Parsing (input) should produce (root(p("aa"), body, p("bb")))
    }
  }

  it should "parse a directive with a required default body and cursor access" in {
    new BlockParser with DirectiveWithContextAccess {
      val input = """aa
        |
        |@:dir
        |text
        |@:@
        |
        |bb""".stripMargin
      Parsing (input) should produce (root(p("aa"), p("text/"), p("bb")))
    }
  }
  
  it should "detect a directive with an unknown name" in {
    new BlockParser with OptionalNamedAttribute {
      val input = """aa
        |
        |@:foo { name=foo }
        |
        |bb""".stripMargin
      val msg = "One or more errors processing directive 'foo': No block directive registered with name: foo"
      Parsing (input) should produce (root(p("aa"), invalid("@:foo { name=foo }",msg), p("bb")))
    }
  }

  it should "merge options from two nested directives" in {
    new BlockParser with RequiredPositionalAttribute {
      val input = """aa
                    |
                    |@:style(bar)
                    |
                    |@:dir(foo)
                    |
                    |@:@
                    |
                    |bb""".stripMargin
      Parsing (input) should produce (root(p("aa"), p("foo").mergeOptions(Styles("bar")), p("bb")))
    }
  }
  
}
