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
import laika.bundle.ParserBundle
import laika.parse.Parser
import laika.parse.helper.{DefaultParserHelpers, ParseResultHelpers}
import laika.parse.markup.RootParserProvider
import laika.rewrite.TemplateRewriter
import org.scalatest.{FlatSpec, Matchers}

class SpanDirectiveAPISpec extends FlatSpec
                          with Matchers
                          with ModelBuilder {

  
  object DirectiveSetup {
    import Spans.dsl._
    
    trait Empty {
      val directive = Spans.create("dir")(Spans.dsl.empty(Text("foo")))
    }

    trait RequiredDefaultAttribute {
      val directive = Spans.create("dir") { defaultAttribute.as[String] map (Text(_)) }
    }
    
    trait OptionalDefaultAttribute {
      val directive = Spans.create("dir") { 
        defaultAttribute.as[Int].optional map (num => Text(num.map(_.toString).getOrElse("<>"))) 
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
        (parsedBody, defaultAttribute.as[String]).mapN(Bar) 
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
        (defaultAttribute.as[String], attribute("strAttr").as[String].optional, attribute("intAttr").as[Int].optional, parsedBody).mapN {
          (defAttr, strAttr, intAttr, defBody) => 
            val sum = intAttr.getOrElse(0)
            val str = defAttr + ":" + strAttr.getOrElse("..") + ":" + sum
            SpanSequence(Text(str) +: defBody)
        }
      }
    }
    
    trait DirectiveWithParserAccess {
      val directive = Spans.create("dir") { 
        (rawBody, parser).mapN { (body, parser) =>
          SpanSequence(parser(body.drop(3)))
        }
      }
    }
    
    trait DirectiveWithContextAccess {
      val directive = Spans.create("dir") { 
        (rawBody, cursor).mapN { (body, cursor) =>
          Text(body + cursor.target.path)
        }
      }
    }
    
  }
  
  trait SpanParser extends ParseResultHelpers
                   with DefaultParserHelpers[Span] {
    
    def directive: Spans.Directive

    lazy val directiveSupport: ParserBundle = DirectiveSupport.withDirectives(Seq(), Seq(directive), Seq()).parsers

    lazy val defaultParser: Parser[Span] = RootParserProvider.forParsers(
      markupExtensions = directiveSupport.markupExtensions
    ).recursiveSpans.map { spans =>
      val seq = SpanSequence(spans)
      TemplateRewriter.rewriteRules(DocumentCursor(
        Document(Root, RootElement(seq), config = ConfigBuilder.empty.withValue("ref","value").build)
      )).rewriteSpan(seq)
    }
    
    def invalid (input: String, error: String): InvalidSpan = InvalidElement(error, input).asSpan
        
    def ss (spans: Span*): Span = SpanSequence(spans)

  }
  

  import DirectiveSetup._
  
  "The span directive parser" should "parse an empty directive" in {
    new SpanParser with Empty {
      Parsing ("aa @:dir bb") should produce (ss(Text("aa foo bb")))
    }
  }

  it should "parse a directive with one required default string attribute" in {
    new SpanParser with RequiredDefaultAttribute {
      Parsing ("aa @:dir(foo) bb") should produce (ss(Text("aa foo bb")))
    }
  }

  it should "parse a directive with one required legacy default string attribute" in {
    new SpanParser with RequiredDefaultAttribute {
      Parsing ("aa @:dir { foo } bb") should produce (ss(Text("aa foo bb")))
    }
  }
  
  it should "detect a directive with a missing required default attribute" in {
    new SpanParser with RequiredDefaultAttribute {
      val msg = "One or more errors processing directive 'dir': required default attribute is missing"
      Parsing ("aa @:dir bb") should produce (ss(Text("aa "), invalid("@:dir",msg), Text(" bb")))
    }
  }

  it should "detect a directive with an invalid legacy default attribute" in {
    new SpanParser with RequiredDefaultAttribute {
      val msg = """One or more errors processing directive 'dir': Multiple errors parsing HOCON: [1.16] failure: Expected separator after key ('=', '+=', ':' or '{')
                  |
                  |aa @:dir { foo ? bar } bb
                  |               ^""".stripMargin
      Parsing ("aa @:dir { foo ? bar } bb") should produce (ss(Text("aa "), invalid("@:dir { foo ? bar } bb",msg)))
    }
  }
  
  it should "parse a directive with an optional default int attribute" in {
    new SpanParser with OptionalDefaultAttribute {
      Parsing ("aa @:dir(5) bb") should produce (ss(Text("aa 5 bb")))
    }
  }
  
  it should "detect a directive with an optional invalid default int attribute" in {
    new SpanParser with OptionalDefaultAttribute {
      val msg = "One or more errors processing directive 'dir': error converting default attribute: not an integer: foo"
      Parsing ("aa @:dir(foo) bb") should produce (ss(Text("aa "), invalid("@:dir(foo)",msg), Text(" bb")))
    }
  }

  it should "parse a directive with an optional legacy default int attribute" in {
    new SpanParser with OptionalDefaultAttribute {
      Parsing ("aa @:dir { 5 } bb") should produce (ss(Text("aa 5 bb")))
    }
  }
  
  it should "parse a directive with a missing optional default int attribute" in {
    new SpanParser with OptionalDefaultAttribute {
      Parsing ("aa @:dir bb") should produce (ss(Text("aa <> bb")))
    }
  }
  
  it should "parse a directive with one required named string attribute" in {
    new SpanParser with RequiredNamedAttribute {
      Parsing ("aa @:dir { name=foo } bb") should produce (ss(Text("aa foo bb")))
    }
  }
  
  it should "parse a directive with a named string attribute value in quotes" in {
    new SpanParser with RequiredNamedAttribute {
      Parsing ("""aa @:dir { name="foo bar" } bb""") should produce (ss(Text("aa foo bar bb")))
    }
  }
  
  it should "detect a directive with a missing required named attribute" in {
    new SpanParser with RequiredNamedAttribute {
      val msg = "One or more errors processing directive 'dir': required attribute 'name' is missing"
      Parsing ("aa @:dir bb") should produce (ss(Text("aa "), invalid("@:dir",msg), Text(" bb")))
    }
  }

  it should "detect a directive with an invalid HOCON string attribute (missing closing quote)" in {
    new SpanParser with RequiredNamedAttribute {
      val msg = """One or more errors processing directive 'dir': Multiple errors parsing HOCON: [1.30] failure: Expected closing '"'
       |
       |aa @:dir { name="foo bar } bb
       |                             ^""".stripMargin
      Parsing ("""aa @:dir { name="foo bar } bb""") should produce (ss(Text("aa "), invalid("@:dir { name=\"foo bar } bb",msg)))
    }
  }

  it should "detect a directive with an invalid HOCON string attribute (invalid character in unquoted string)" in {
    new SpanParser with RequiredNamedAttribute {
      val msg = """One or more errors processing directive 'dir': Multiple errors parsing HOCON: [1.23] failure: Illegal character in unquoted string, expected delimiters are one of '#', ',', '\n', '}'
                  |
                  |aa @:dir { name = foo ? bar } bb
                  |                      ^""".stripMargin
      Parsing ("""aa @:dir { name = foo ? bar } bb""") should produce (ss(Text("aa "), invalid("@:dir { name = foo ? bar }",msg), Text(" bb")))
    }
  }
  
  it should "parse a directive with an optional named int attribute" in {
    new SpanParser with OptionalNamedAttribute {
      Parsing ("aa @:dir { name=5 } bb") should produce (ss(Text("aa 5 bb")))
    }
  }
  
  it should "detect a directive with an optional invalid named int attribute" in {
    new SpanParser with OptionalNamedAttribute {
      val msg = "One or more errors processing directive 'dir': error converting attribute 'name': not an integer: foo"
      Parsing ("aa @:dir { name=foo } bb") should produce (ss(Text("aa "), invalid("@:dir { name=foo }",msg), Text(" bb")))
    }
  }
  
  it should "parse a directive with a missing optional named int attribute" in {
    new SpanParser with OptionalNamedAttribute {
      val msg = "One or more errors processing directive 'dir': required default attribute is missing"
      Parsing ("aa @:dir bb") should produce (ss(Text("aa <> bb")))
    }
  }

  it should "parse a directive with the allAttributes combinator" in {
    new SpanParser with AllAttributes {
      Parsing ("aa @:dir { foo=Planet, bar=42 } bb") should produce (ss(Text("aa Planet 42 bb")))
    }
  }
  
  it should "parse a directive with a body" in {
    new SpanParser with RequiredBody {
      val body = ss(Text(" some value text "))
      Parsing ("aa @:dir some ${ref} text @:@ bb") should produce (ss(Text("aa "), body, Text(" bb")))
    }
  }
  
  it should "support a directive with a nested pair of braces" in {
    new SpanParser with RequiredBody {
      val body = ss(Text(" some {ref} text "))
      Parsing ("aa @:dir some {ref} text @:@ bb") should produce (ss(Text("aa "), body, Text(" bb")))
    }
  }
  
  it should "detect a directive with a missing body" in {
    new SpanParser with RequiredBody {
      val msg = "One or more errors processing directive 'dir': required body is missing"
      Parsing ("aa @:dir bb") should produce (ss(Text("aa "), invalid("@:dir",msg), Text(" bb")))
    }
  }

  it should "parse a directive with a separated body" in {
    new SpanParser with SeparatedBody {
      val input = """aa @:dir aaa @:foo bbb @:bar { baz } ccc @:@ bb"""
      val body = SpanSequence(Text(" aaa foo bbb baz ccc "))
      Parsing (input) should produce (ss(Text("aa "), body, Text(" bb")))
    }
  }

  it should "detect a directive with an invalid separator" in {
    new SpanParser with SeparatedBody {
      val input = """aa @:dir aaa @:foo bbb @:bar ccc @:@ bb"""
      val msg = "One or more errors processing directive 'dir': One or more errors processing separator directive 'bar': required default attribute is missing"
      val src = input.slice(3, 36)
      Parsing (input) should produce (ss(Text("aa "), invalid(src,msg), Text(" bb")))
    }
  }

  it should "detect a directive with a separator not meeting the min count requirements" in {
    new SpanParser with SeparatedBody {
      val input = """aa @:dir aaa @:bar { baz } ccc @:@ bb"""
      val msg = "One or more errors processing directive 'dir': too few occurrences of separator directive 'foo': expected min: 1, actual: 0"
      val src = input.slice(3, 34)
      Parsing (input) should produce (ss(Text("aa "), invalid(src,msg), Text(" bb")))
    }
  }

  it should "detect a directive with a separator exceeding the max count constraint" in {
    new SpanParser with SeparatedBody {
      val input = """aa @:dir aaa @:foo bbb @:bar { baz } ccc @:bar { baz } ddd @:@ bb"""
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
      val body = ss(
        Text("foo:str:7 1 value 2 ")
      )
      Parsing ("aa @:dir(foo) { strAttr=str, intAttr=7 } 1 ${ref} 2 @:@ bb") should produce (ss(Text("aa "), body, Text(" bb")))
    }
  }

  it should "parse a full directive spec with all elements present with attributes spanning two lines" in {
    new FullDirectiveSpec with SpanParser {
      val body = ss(
        Text("foo:str:7 1 value 2 ")
      )
      Parsing ("aa @:dir(foo) {\n strAttr=str\nintAttr=7 \n} 1 ${ref} 2 @:@ bb") should produce (ss(Text("aa "), body, Text(" bb")))
    }
  }

  it should "parse a full directive spec with all elements present, including a legacy default attribute" in {
    new FullDirectiveSpec with SpanParser {
      val body = ss(
        Text("foo:str:7 1 value 2 ")
      )
      Parsing ("aa @:dir { foo, strAttr=str, intAttr=7 } 1 ${ref} 2 @:@ bb") should produce (ss(Text("aa "), body, Text(" bb")))
    }
  }
  
  it should "parse a full directive spec with all optional elements missing" in {
    new FullDirectiveSpec with SpanParser {
      val body = ss(
        Text("foo:..:0 1 value 2 ")
      )
      Parsing ("aa @:dir(foo) 1 ${ref} 2 @:@ bb") should produce (ss(Text("aa "), body, Text(" bb")))
    }
  }
  
  it should "detect a full directive spec with one required attribute and the body missing" in {
    new FullDirectiveSpec with SpanParser {
      val msg = "One or more errors processing directive 'dir': required default attribute is missing, required body is missing"
      Parsing ("aa @:dir { strAttr=str } bb") should produce (ss(Text("aa "), invalid("@:dir { strAttr=str }",msg), Text(" bb")))
    }
  }
  
  it should "parse a directive with a body and parser access" in {
    new DirectiveWithParserAccess with SpanParser {
      val body = ss(Text("me value text "))
      Parsing ("aa @:dir some ${ref} text @:@ bb") should produce (ss(Text("aa "), body, Text(" bb")))
    }
  }
  
  it should "parse a directive with a body and cursor access" in {
    new DirectiveWithContextAccess with SpanParser {
      Parsing ("aa @:dir text @:@ bb") should produce (ss(Text("aa  text / bb")))
    }
  }
  
  it should "detect a directive with an unknown name" in {
    new SpanParser with OptionalNamedAttribute {
      val msg = "One or more errors processing directive 'foo': No span directive registered with name: foo"
      Parsing ("aa @:foo { name=foo } bb") should produce (ss(Text("aa "), invalid("@:foo { name=foo }",msg), Text(" bb")))
    }
  }
  
  
}
