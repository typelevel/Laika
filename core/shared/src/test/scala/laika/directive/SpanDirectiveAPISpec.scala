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
import laika.api.MarkupParser
import laika.config.ConfigBuilder
import laika.ast.Path.Root
import laika.ast._
import laika.ast.helper.{ModelBuilder, TestSourceBuilders}
import laika.bundle.ParserBundle
import laika.format.Markdown
import laika.parse.Parser
import laika.parse.helper.{DefaultParserHelpers, ParseResultHelpers}
import laika.parse.markup.DocumentParser.ParserError
import laika.parse.markup.RootParserProvider
import laika.rewrite.TemplateRewriter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Try

class SpanDirectiveAPISpec extends AnyFlatSpec
                          with Matchers
                          with TestSourceBuilders {

  
  object DirectiveSetup {
    import Spans.dsl._
    
    trait Empty {
      val directive = Spans.create("dir")(Spans.dsl.empty(Text("foo")))
    }

    trait RequiredPositionalAttribute {
      val directive = Spans.create("dir") { attribute(0).as[String] map (Text(_)) }
    }
    
    trait OptionalPositionalAttribute {
      val directive = Spans.create("dir") {
        attribute(0).as[Int].optional map (num => Text(num.map(_.toString).getOrElse("<>"))) 
      }
    }
    
    trait RequiredNamedAttribute {
      val directive = Spans.create("dir") { attribute("name").as[String] map (Text(_)) }
    }
    
    trait OptionalNamedAttribute {
      val directive = Spans.create("dir") { 
        attribute("name").as[Int].optional map (num => Text(num.map(_.toString).getOrElse("<>"))) 
      }
    }

    trait AllAttributes {
      val directive = Spans.create("dir") {
        allAttributes.map { attrs =>
          val foo = attrs.get[String]("foo").toOption.get
          val bar = attrs.get[Int]("bar").toOption.get
          Text(s"$foo $bar")
        }
      }
    }
    
    trait RequiredBody {
      val directive = Spans.create("dir") { parsedBody map (SpanSequence(_)) }
    }

    trait SeparatedBody {
      
      sealed trait Child extends Product with Serializable
      case class Foo (content: Seq[Span]) extends Child
      case class Bar (content: Seq[Span], attr: String) extends Child
      
      val sep1 = Spans.separator("foo", min = 1) { 
        parsedBody.map(Foo) 
      }
      val sep2 = Spans.separator("bar", max = 1) { 
        (parsedBody, attribute(0).as[String]).mapN(Bar) 
      }
      val directive = Spans.create("dir") { separatedBody[Child](Seq(sep1, sep2)) map { multipart =>
        val seps = multipart.children.flatMap {
          case Foo(content) => Text("foo") +: content
          case Bar(content, attr) => Text(attr) +: content
        }
        SpanSequence(multipart.mainBody ++ seps)
      }}
    }
    
    trait FullDirectiveSpec {
      val directive = Spans.create("dir") {
        (attribute(0).as[String], attribute(1).as[Int], attribute("strAttr").as[String].optional, attribute("intAttr").as[Int].optional, parsedBody).mapN {
          (posStr, posInt, strAttr, intAttr, defBody) => 
            val sum = intAttr.getOrElse(0) + posInt
            val str = posStr + ":" + strAttr.getOrElse("..") + ":" + sum
            SpanSequence(Text(str) +: defBody)
        }
      }
    }
    
    trait DirectiveWithCustomBodyParser {
      import laika.parse.builders._
      import laika.parse.implicits._
      val directive = Spans.create("dir") { 
        parsedBody(recParsers => anyChars.take(3) ~> recParsers.recursiveSpans(anyChars.line))
          .map(SpanSequence(_))
      }
    }
    
    trait DirectiveWithContextAccess {
      val directive = Spans.create("dir") { 
        (rawBody, cursor).mapN { (body, cursor) =>
          Text(body + cursor.target.path)
        }
      }
    }
    
    trait LinkDirectiveSetup {
      val directive = Links.eval("rfc") { (linkId, _) =>
        Try(Integer.parseInt(linkId))
          .toEither
          .fold(
            _ => Left(s"Not a valid RFC id: $linkId"),
            id => Right(SpanLink.external(s"http://tools.ietf.org/html/rfc$linkId")(s"RFC $id"))
          )
      }
      object bundle extends DirectiveRegistry {
        override def spanDirectives: Seq[Spans.Directive] = Nil
        override def blockDirectives: Seq[Blocks.Directive] = Nil
        override def templateDirectives: Seq[Templates.Directive] = Nil
        override def linkDirectives: Seq[Links.Directive] = Seq(directive)
      }

      def parseAsMarkdown (input: String): Either[ParserError, Block] = MarkupParser
        .of(Markdown)
        .using(bundle)
        .failOnMessages(MessageFilter.None)
        .build
        .parse(input)
        .map(_.content.content.head)
    }
    
    
  }
  
  trait BaseParser extends ParseResultHelpers with DefaultParserHelpers[Span] {

    def directiveSupport: ParserBundle
    
    def input: String
    
    lazy val defaultParser: Parser[Span] = RootParserProvider.forParsers(
      markupExtensions = directiveSupport.markupExtensions
    ).standaloneSpanParser.map { spans =>
      val seq = SpanSequence(spans)
      TemplateRewriter.rewriteRules(DocumentCursor(
        Document(Root, RootElement(seq), config = ConfigBuilder.empty.withValue("ref","value").build)
      )).rewriteSpan(seq)
    }

    def invalid (fragment: String, error: String): InvalidSpan = InvalidSpan(error, source(fragment, input))

    def ss (spans: Span*): Span = SpanSequence(spans)
    
  }
  
  trait SpanParser extends BaseParser {
    def directive: Spans.Directive
    lazy val directiveSupport: ParserBundle = DirectiveSupport.withDirectives(Seq(), Seq(directive), Nil, Nil).parsers
  }

  trait LinkParser extends BaseParser {
    def directive: Links.Directive
    lazy val directiveSupport: ParserBundle = DirectiveSupport.withDirectives(Seq(), Nil, Nil, Seq(directive)).parsers
  }
  

  import DirectiveSetup._
  
  "The span directive parser" should "parse an empty directive" in {
    new SpanParser with Empty {
      val input = "aa @:dir bb"
      Parsing (input) should produce (ss(Text("aa foo bb")))
    }
  }

  it should "parse a directive with one required default string attribute" in {
    new SpanParser with RequiredPositionalAttribute {
      val input = "aa @:dir(foo) bb"
      Parsing (input) should produce (ss(Text("aa foo bb")))
    }
  }

  it should "detect a directive with a missing required positional attribute" in {
    new SpanParser with RequiredPositionalAttribute {
      val input = "aa @:dir bb"
      val msg = "One or more errors processing directive 'dir': required positional attribute at index 0 is missing"
      Parsing (input) should produce (ss(Text("aa "), invalid("@:dir",msg), Text(" bb")))
    }
  }

  it should "parse a directive with an optional default int attribute" in {
    new SpanParser with OptionalPositionalAttribute {
      val input = "aa @:dir(5) bb"
      Parsing (input) should produce (ss(Text("aa 5 bb")))
    }
  }
  
  it should "detect a directive with an optional invalid default int attribute" in {
    new SpanParser with OptionalPositionalAttribute {
      val input = "aa @:dir(foo) bb"
      val msg = "One or more errors processing directive 'dir': error converting positional attribute at index 0: not an integer: foo"
      Parsing (input) should produce (ss(Text("aa "), invalid("@:dir(foo)",msg), Text(" bb")))
    }
  }

  it should "parse a directive with a missing optional default int attribute" in {
    new SpanParser with OptionalPositionalAttribute {
      val input = "aa @:dir bb"
        Parsing (input) should produce (ss(Text("aa <> bb")))
    }
  }
  
  it should "parse a directive with one required named string attribute" in {
    new SpanParser with RequiredNamedAttribute {
      val input = "aa @:dir { name=foo } bb"
      Parsing (input) should produce (ss(Text("aa foo bb")))
    }
  }
  
  it should "parse a directive with a named string attribute value in quotes" in {
    new SpanParser with RequiredNamedAttribute {
      val input = """aa @:dir { name="foo bar" } bb"""
      Parsing (input) should produce (ss(Text("aa foo bar bb")))
    }
  }
  
  it should "detect a directive with a missing required named attribute" in {
    new SpanParser with RequiredNamedAttribute {
      val input = "aa @:dir bb"
      val msg = "One or more errors processing directive 'dir': required attribute 'name' is missing"
      Parsing (input) should produce (ss(Text("aa "), invalid("@:dir",msg), Text(" bb")))
    }
  }

  it should "detect a directive with an invalid HOCON string attribute (missing closing quote)" in {
    new SpanParser with RequiredNamedAttribute {
      val input = """aa @:dir { name="foo bar } bb"""
      val msg = s"""One or more errors processing directive 'dir': Multiple errors parsing HOCON: [1.30] failure: Expected closing '"'
       |
       |$input
       |                             ^""".stripMargin
      Parsing (input) should produce (ss(Text("aa "), invalid("@:dir { name=\"foo bar } bb", msg)))
    }
  }

  it should "detect a directive with an invalid HOCON string attribute (invalid character in unquoted string)" in {
    new SpanParser with RequiredNamedAttribute {
      val input = """aa @:dir { name = foo ? bar } bb"""
      val msg = s"""One or more errors processing directive 'dir': Multiple errors parsing HOCON: [1.23] failure: Illegal character in unquoted string, expected delimiters are one of '#', ',', '\\n', '}'
                  |
                  |$input
                  |                      ^""".stripMargin
      Parsing (input) should produce (ss(Text("aa "), invalid("@:dir { name = foo ? bar }", msg), Text(" bb")))
    }
  }
  
  it should "parse a directive with an optional named int attribute" in {
    new SpanParser with OptionalNamedAttribute {
      val input = "aa @:dir { name=5 } bb"
      Parsing (input) should produce (ss(Text("aa 5 bb")))
    }
  }
  
  it should "detect a directive with an optional invalid named int attribute" in {
    new SpanParser with OptionalNamedAttribute {
      val input = "aa @:dir { name=foo } bb"
      val msg = "One or more errors processing directive 'dir': error converting attribute 'name': not an integer: foo"
      Parsing (input) should produce (ss(Text("aa "), invalid("@:dir { name=foo }",msg), Text(" bb")))
    }
  }
  
  it should "parse a directive with a missing optional named int attribute" in {
    new SpanParser with OptionalNamedAttribute {
      val input = "aa @:dir bb"
      val msg = "One or more errors processing directive 'dir': required positional attribute at index 0 is missing"
      Parsing (input) should produce (ss(Text("aa <> bb")))
    }
  }

  it should "parse a directive with the allAttributes combinator" in {
    new SpanParser with AllAttributes {
      val input = "aa @:dir { foo=Planet, bar=42 } bb"
        Parsing (input) should produce (ss(Text("aa Planet 42 bb")))
    }
  }
  
  it should "parse a directive with a body" in {
    new SpanParser with RequiredBody {
      val input = "aa @:dir some ${ref} text @:@ bb"
      val body = ss(Text(" some value text "))
      Parsing (input) should produce (ss(Text("aa "), body, Text(" bb")))
    }
  }
  
  it should "support a directive with a nested pair of braces" in {
    new SpanParser with RequiredBody {
      val input = "aa @:dir some {ref} text @:@ bb"
      val body = ss(Text(" some {ref} text "))
      Parsing (input) should produce (ss(Text("aa "), body, Text(" bb")))
    }
  }
  
  it should "detect a directive with a missing body" in {
    new SpanParser with RequiredBody {
      val input = "aa @:dir bb"
      val msg = "One or more errors processing directive 'dir': required body is missing"
      Parsing (input) should produce (ss(Text("aa "), invalid("@:dir",msg), Text(" bb")))
    }
  }

  it should "parse a directive with a separated body" in {
    new SpanParser with SeparatedBody {
      val input = """aa @:dir aaa @:foo bbb @:bar(baz) ccc @:@ bb"""
      val body = SpanSequence(Text(" aaa foo bbb baz ccc "))
      Parsing (input) should produce (ss(Text("aa "), body, Text(" bb")))
    }
  }

  it should "detect a directive with an invalid separator" in {
    new SpanParser with SeparatedBody {
      val input = """aa @:dir aaa @:foo bbb @:bar ccc @:@ bb"""
      val msg = "One or more errors processing directive 'dir': One or more errors processing separator directive 'bar': required positional attribute at index 0 is missing"
      val src = input.slice(3, 36)
      Parsing (input) should produce (ss(Text("aa "), invalid(src,msg), Text(" bb")))
    }
  }

  it should "detect a directive with a separator not meeting the min count requirements" in {
    new SpanParser with SeparatedBody {
      val input = """aa @:dir aaa @:bar(baz) ccc @:@ bb"""
      val msg = "One or more errors processing directive 'dir': too few occurrences of separator directive 'foo': expected min: 1, actual: 0"
      val src = input.slice(3, 31)
      Parsing (input) should produce (ss(Text("aa "), invalid(src,msg), Text(" bb")))
    }
  }

  it should "detect a directive with a separator exceeding the max count constraint" in {
    new SpanParser with SeparatedBody {
      val input = """aa @:dir aaa @:foo bbb @:bar(baz) ccc @:bar(baz) ddd @:@ bb"""
      val msg = "One or more errors processing directive 'dir': too many occurrences of separator directive 'bar': expected max: 1, actual: 2"
      val src = input.drop(3).dropRight(3)
      Parsing (input) should produce (ss(Text("aa "), invalid(src,msg), Text(" bb")))
    }
  }

  it should "detect an orphaned separator directive" in new SpanParser with SeparatedBody {
    val input = "aa @:foo bb"
    val msg = "Orphaned separator directive with name 'foo'"
    Parsing (input) should produce (ss(Text("aa "), invalid("@:foo",msg), Text(" bb")))
  }
  
  it should "parse a full directive spec with all elements present" in {
    new FullDirectiveSpec with SpanParser {
      val input = "aa @:dir(foo, 4) { strAttr=str, intAttr=7 } 1 ${ref} 2 @:@ bb"
      val body = ss(
        Text("foo:str:11 1 value 2 ")
      )
      Parsing (input) should produce (ss(Text("aa "), body, Text(" bb")))
    }
  }

  it should "parse a full directive spec with all elements present with attributes spanning two lines" in {
    new FullDirectiveSpec with SpanParser {
      val input = "aa @:dir(foo,4) {\n strAttr=str\nintAttr=7 \n} 1 ${ref} 2 @:@ bb"
      val body = ss(
        Text("foo:str:11 1 value 2 ")
      )
      Parsing (input) should produce (ss(Text("aa "), body, Text(" bb")))
    }
  }

  it should "parse a full directive spec with all optional elements missing" in {
    new FullDirectiveSpec with SpanParser {
      val input = "aa @:dir(foo,4) 1 ${ref} 2 @:@ bb"
      val body = ss(
        Text("foo:..:4 1 value 2 ")
      )
      Parsing (input) should produce (ss(Text("aa "), body, Text(" bb")))
    }
  }
  
  it should "detect a full directive spec with positional attributes and the body missing" in {
    new FullDirectiveSpec with SpanParser {
      val input = "aa @:dir { strAttr=str } bb"
      val msg = "One or more errors processing directive 'dir': required positional attribute at index 0 is missing, required positional attribute at index 1 is missing, required body is missing"
      Parsing (input) should produce (ss(Text("aa "), invalid("@:dir { strAttr=str }",msg), Text(" bb")))
    }
  }
  
  it should "parse a directive with a custom body parser" in {
    new DirectiveWithCustomBodyParser with SpanParser {
      val input = "aa @:dir some ${ref} text @:@ bb"
      val body = ss(Text("me value text "))
      Parsing (input) should produce (ss(Text("aa "), body, Text(" bb")))
    }
  }
  
  it should "parse a directive with a body and cursor access" in {
    new DirectiveWithContextAccess with SpanParser {
      val input = "aa @:dir text @:@ bb"
      Parsing (input) should produce (ss(Text("aa  text / bb")))
    }
  }
  
  it should "detect a directive with an unknown name" in {
    new SpanParser with OptionalNamedAttribute {
      val input = "aa @:foo { name=foo } bb"
      val msg = "One or more errors processing directive 'foo': No span directive registered with name: foo"
      Parsing (input) should produce (ss(Text("aa "), invalid("@:foo { name=foo }",msg), Text(" bb")))
    }
  }
  
  it should "parse a link directive" in new LinkParser with LinkDirectiveSetup {
    val input = "aa @:rfc(222) bb"
    Parsing (input) should produce (ss(
      Text("aa "),
      SpanLink.external("http://tools.ietf.org/html/rfc222")("RFC 222"),
      Text(" bb")
    ))
  }

  it should "parse a link directive inside a native link expression" in new LinkParser with LinkDirectiveSetup {
    val input = "aa [RFC-222][@:rfc(222)] bb"
    parseAsMarkdown(input) shouldBe Right(Paragraph(
      Text("aa "),
      SpanLink.external("http://tools.ietf.org/html/rfc222")("RFC-222"),
      Text(" bb")
    ))
  }

  it should "detect an unknown link directive" in new LinkParser with LinkDirectiveSetup {
    val input = "aa [RFC-222][@:rfx(222)] bb"
    parseAsMarkdown(input) shouldBe Right(Paragraph(
      Text("aa "),
      invalid("[RFC-222][@:rfx(222)]", "Unknown link directive: rfx"),
      Text(" bb")
    ))
  }

  it should "detect an invalid link directive" in new LinkParser with LinkDirectiveSetup {
    val input = "aa [RFC-222][@:rfc(foo)] bb"
    parseAsMarkdown(input) shouldBe Right(Paragraph(
      Text("aa "),
      invalid("[RFC-222][@:rfc(foo)]", "Invalid link directive: Not a valid RFC id: foo"),
      Text(" bb")
    ))
  }

  it should "detect an invalid link directive syntax" in new LinkParser with LinkDirectiveSetup {
    val input = "aa [RFC-222][@:rfc foo] bb"
    parseAsMarkdown(input) shouldBe Right(Paragraph(
      Text("aa "),
      invalid("[RFC-222][@:rfc foo]", "Invalid link directive: `(' expected but `f` found"),
      Text(" bb")
    ))
  }
  
}
