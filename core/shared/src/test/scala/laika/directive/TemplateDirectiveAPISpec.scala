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
import laika.ast.sample.TestSourceBuilders
import laika.parse.Parser
import laika.parse.directive.TemplateParsers
import laika.parse.helper.{DefaultParserHelpers, ParseResultHelpers}
import laika.rewrite.TemplateRewriter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TemplateDirectiveAPISpec extends AnyFlatSpec
                          with Matchers
                          with TestSourceBuilders {

  def result (span: TemplateSpan): TemplateRoot = TemplateRoot(
    TemplateString("aa "),
    span,
    TemplateString(" bb")
  )
  
  object DirectiveSetup {
    import Templates.dsl._

    trait Empty {
      val directive = Templates.create("dir")(Templates.dsl.empty(TemplateString("foo")))
    }
    
    trait RequiredPositionalAttribute {
      val directive = Templates.create("dir") { attribute(0).as[String] map (TemplateString(_)) }
    }
    
    trait OptionalPositionalAttribute {
      val directive = Templates.create("dir") {
        attribute(0).as[Int].optional map (num => TemplateString(num.map(_.toString).getOrElse("<>"))) 
      }
    }
    
    trait RequiredNamedAttribute {
      val directive = Templates.create("dir") { attribute("name").as[String] map (TemplateString(_)) }
    }
    
    trait OptionalNamedAttribute {
      val directive = Templates.create("dir") { 
        attribute("name").as[Int].optional map (num => TemplateString(num.map(_.toString).getOrElse("<>"))) 
      }
    }

    trait AllAttributes {
      val directive = Templates.create("dir") {
        allAttributes.map { attrs =>
          val foo = attrs.get[String]("foo").toOption.get
          val bar = attrs.get[Int]("bar").toOption.get
          TemplateString(s"$foo $bar")
        }
      }
    }
    
    trait RequiredBody {
      val directive = Templates.create("dir") { parsedBody map (TemplateSpanSequence(_)) }
    }

    trait SeparatedBody {
      
      sealed trait Child extends Product with Serializable
      case class Foo (content: Seq[TemplateSpan]) extends Child
      case class Bar (content: Seq[TemplateSpan], attr: String) extends Child
      
      val sep1 = Templates.separator("foo", min = 1) { 
        parsedBody.map(Foo) 
      }
      val sep2 = Templates.separator("bar", max = 1) { 
        (parsedBody, attribute(0).as[String]).mapN(Bar) 
      }
      val directive = Templates.create("dir") { separatedBody(Seq(sep1, sep2)).map { multipart =>
        val seps = multipart.children.flatMap {
          case Foo(content) => TemplateString("foo") +: content
          case Bar(content, attr) => TemplateString(attr) +: content
        }
        TemplateSpanSequence(multipart.mainBody ++ seps)
      }}
    }
    
    trait FullDirectiveSpec {
      val directive = Templates.create("dir") {
        (attribute(0).as[String], attribute(1).as[Int], attribute("strAttr").as[String].optional, attribute("intAttr").as[Int].optional, parsedBody).mapN {
          (posStr, posInt, strAttr, intAttr, defBody) => 
            val sum = intAttr.getOrElse(0) + posInt
            val str = posStr + ":" + strAttr.getOrElse("..") + ":" + sum
            TemplateSpanSequence(TemplateString(str) +: defBody)
        }
      }
    }

    trait DirectiveWithCustomBodyParser {
      import laika.parse.builders._
      import laika.parse.implicits._
      val directive = Templates.create("dir") {
        parsedBody(recParsers => anyChars.take(3) ~> recParsers.recursiveSpans(anyChars.line))
          .map {
            _.collect {
              case s: TemplateSpan => s
              case Text(s, opt) => TemplateString(s, opt) // TODO - might get extracted
            }
          }
          .map(TemplateSpanSequence(_))
      }
    }
    
    trait DirectiveWithContextAccess {
      val directive = Templates.create("dir") { 
        (rawBody, cursor).mapN { (body, cursor) =>
          TemplateString(body + cursor.target.path)
        }
      }
    }
    
  }
  
  trait TemplateParser extends ParseResultHelpers
                          with DefaultParserHelpers[TemplateRoot] {
    
    def directive: Templates.Directive

    val templateParsers = new TemplateParsers(Map(directive.name -> directive))
    
    val defaultParser: Parser[TemplateRoot] = templateParsers.templateSpans.map { spans =>
      val root = TemplateRoot(spans)
      TemplateRewriter.rewriteRules(DocumentCursor(
        Document(Root, RootElement(root), config = ConfigBuilder.empty.withValue("ref","value").build)
      )).rewriteBlock(root).asInstanceOf[TemplateRoot]
    }
    
  }
  
  trait InvalidTemplateParser extends TemplateParser {
    def input: String
    def invalid (fragment: String, error: String): InvalidSpan = InvalidSpan(error, source(fragment, input))
  }
  

  import DirectiveSetup._

  "The directive parser" should "parse an empty directive" in {
    new Empty with TemplateParser {
      Parsing ("aa @:dir bb") should produce (result(TemplateString("foo")))
    }
  }
  
  it should "parse a directive with one required default string attribute" in {
    new RequiredPositionalAttribute with TemplateParser {
      Parsing ("aa @:dir(foo) bb") should produce (result(TemplateString("foo")))
    }
  }

  it should "detect a directive with a missing required positional attribute" in {
    new RequiredPositionalAttribute with InvalidTemplateParser {
      val input = "aa @:dir bb"
      val msg = "One or more errors processing directive 'dir': required positional attribute at index 0 is missing"
      Parsing (input) should produce (result(TemplateElement(invalid("@:dir",msg))))
    }
  }
  
  it should "parse a directive with an optional default int attribute" in {
    new OptionalPositionalAttribute with TemplateParser {
      Parsing ("aa @:dir(5) bb") should produce (result(TemplateString("5")))
    }
  }
  
  it should "detect a directive with an optional invalid default int attribute" in {
    new OptionalPositionalAttribute with InvalidTemplateParser {
      val input = "aa @:dir(foo) bb"
      val msg = "One or more errors processing directive 'dir': error converting positional attribute at index 0: not an integer: foo"
      Parsing (input) should produce (result(TemplateElement(invalid("@:dir(foo)",msg))))
    }
  }
  
  it should "parse a directive with a missing optional default int attribute" in {
    new OptionalPositionalAttribute with TemplateParser {
      Parsing ("aa @:dir bb") should produce (result(TemplateString("<>")))
    }
  }
  
  it should "parse a directive with one required named string attribute" in {
    new RequiredNamedAttribute with TemplateParser {
      Parsing ("aa @:dir { name=foo } bb") should produce (result(TemplateString("foo")))
    }
  }
  
  it should "parse a directive with a named string attribute value in quotes" in {
    new RequiredNamedAttribute with TemplateParser {
      Parsing ("""aa @:dir { name="foo bar" } bb""") should produce (result(TemplateString("foo bar")))
    }
  }
  
  it should "detect a directive with a missing required named attribute" in {
    new RequiredNamedAttribute with InvalidTemplateParser {
      val input = "aa @:dir bb"
      val msg = "One or more errors processing directive 'dir': required attribute 'name' is missing"
      Parsing (input) should produce (result(TemplateElement(invalid("@:dir",msg))))
    }
  }
  
  it should "parse a directive with an optional named int attribute" in {
    new OptionalNamedAttribute with TemplateParser {
      Parsing ("aa @:dir { name=5 } bb") should produce (result(TemplateString("5")))
    }
  }
  
  it should "detect a directive with an optional invalid named int attribute" in {
    new OptionalNamedAttribute with InvalidTemplateParser {
      val input = "aa @:dir { name=foo } bb"
      val msg = "One or more errors processing directive 'dir': error converting attribute 'name': not an integer: foo"
      Parsing (input) should produce (result(TemplateElement(invalid("@:dir { name=foo }",msg))))
    }
  }
  
  it should "parse a directive with a missing optional named int attribute" in {
    new OptionalNamedAttribute with TemplateParser {
      val msg = "One or more errors processing directive 'dir': required positional attribute at index 0 is missing"
      Parsing ("aa @:dir bb") should produce (result(TemplateString("<>")))
    }
  }

  it should "parse a directive with the allAttributes combinator" in {
    new AllAttributes with TemplateParser {
      Parsing ("aa @:dir { foo=Planet, bar=42 } bb") should produce (result(TemplateString("Planet 42")))
    }
  }
  
  it should "parse a directive with a body" in {
    new RequiredBody with TemplateParser {
      val body = TemplateSpanSequence(TemplateString(" some "), TemplateString("value"), TemplateString(" text "))
      Parsing ("aa @:dir some ${ref} text @:@ bb") should produce (result(body))
    }
  }
  
  it should "support a directive with a nested pair of braces" in {
    new RequiredBody with TemplateParser {
      val body = TemplateSpanSequence(" some {ref} text ")
      Parsing ("aa @:dir some {ref} text @:@ bb") should produce (result(body))
    }
  }
  
  it should "detect a directive with a missing body" in {
    new RequiredBody with InvalidTemplateParser {
      val input = "aa @:dir bb"
      val msg = "One or more errors processing directive 'dir': required body is missing"
      Parsing (input) should produce (result(TemplateElement(invalid("@:dir",msg))))
    }
  }

  it should "parse a directive with a separated body" in {
    new SeparatedBody with TemplateParser {
      val input = """aa @:dir aaa @:foo bbb @:bar(baz) ccc @:@ bb"""
      val body = TemplateSpanSequence(" aaa ", "foo", " bbb ", "baz", " ccc ")
      Parsing (input) should produce (result(body))
    }
  }

  it should "detect a directive with an invalid separator" in {
    new SeparatedBody with InvalidTemplateParser {
      val input = """aa @:dir aaa @:foo bbb @:bar ccc @:@ bb"""
      val msg = "One or more errors processing directive 'dir': One or more errors processing separator directive 'bar': required positional attribute at index 0 is missing"
      val src = input.slice(3, 36)
      Parsing (input) should produce (result(TemplateElement(invalid(src,msg))))
    }
  }

  it should "detect a directive with a separator not meeting the min count requirements" in {
    new SeparatedBody with InvalidTemplateParser {
      val input = """aa @:dir aaa @:bar(baz) ccc @:@ bb"""
      val msg = "One or more errors processing directive 'dir': too few occurrences of separator directive 'foo': expected min: 1, actual: 0"
      val src = input.slice(3, 31)
      Parsing (input) should produce (result(TemplateElement(invalid(src,msg))))
    }
  }

  it should "detect a directive with a separator exceeding the max count constraint" in {
    new SeparatedBody with InvalidTemplateParser {
      val input = """aa @:dir aaa @:foo bbb @:bar(baz) ccc @:bar(baz) ddd @:@ bb"""
      val msg = "One or more errors processing directive 'dir': too many occurrences of separator directive 'bar': expected max: 1, actual: 2"
      val src = input.drop(3).dropRight(3)
      Parsing (input) should produce (result(TemplateElement(invalid(src,msg))))
    }
  }

  it should "detect an orphaned separator directive" in new SeparatedBody with InvalidTemplateParser {
    val input = "aa @:foo bb"
    val msg = "Orphaned separator directive with name 'foo'"
    Parsing (input) should produce (result(TemplateElement(invalid("@:foo",msg))))
  }
  
  it should "parse a full directive spec with all elements present" in {
    new FullDirectiveSpec with TemplateParser {
      val body = TemplateSpanSequence("foo:str:11", " 1 ", "value", " 2 ")
      Parsing ("aa @:dir(foo, 4) { strAttr=str, intAttr=7 } 1 ${ref} 2 @:@ bb") should produce (result(body))
    }
  }

  it should "parse a full directive spec with all elements present with attributes spanning three lines" in {
    new FullDirectiveSpec with TemplateParser {
      val body = TemplateSpanSequence("foo:str:11", " 1 ", "value", " 2 ")
      Parsing ("aa @:dir(foo,4) { \nstrAttr=str\nintAttr=7\n } 1 ${ref} 2 @:@ bb") should produce (result(body))
    }
  }
  
  it should "parse a full directive spec with all optional elements missing" in {
    new FullDirectiveSpec with TemplateParser {
      val body = TemplateSpanSequence("foo:..:4", " 1 ", "value", " 2 ")
      Parsing ("aa @:dir(foo,4) 1 ${ref} 2 @:@ bb") should produce (result(body))
    }
  }
  
  it should "detect a full directive spec with all positional attributes and the required body missing" in {
    new FullDirectiveSpec with InvalidTemplateParser {
      val input = "aa @:dir { strAttr=str } bb"
      val msg = "One or more errors processing directive 'dir': required positional attribute at index 0 is missing, required positional attribute at index 1 is missing, required body is missing"
      Parsing (input) should produce (result(TemplateElement(invalid("@:dir { strAttr=str }",msg))))
    }
  }
  
  it should "parse a directive with a custom body parser" in {
    new DirectiveWithCustomBodyParser with TemplateParser {
      val body = TemplateSpanSequence("me ", "value", " text ")
      Parsing ("aa @:dir some ${ref} text @:@ bb") should produce (result(body))
    }
  }
  
  it should "parse a directive with a required default body and cursor access" in {
    new DirectiveWithContextAccess with TemplateParser {
      Parsing ("aa @:dir text @:@ bb") should produce (result(TemplateString(" text /")))
    }
  }
  
  it should "detect a directive with an unknown name" in {
    new OptionalNamedAttribute with InvalidTemplateParser {
      val input = "aa @:foo {name=foo} bb"
      val msg = "One or more errors processing directive 'foo': No template directive registered with name: foo"
      Parsing (input) should produce (result(TemplateElement(invalid("@:foo {name=foo}",msg))))
    }
  }
  
  
}
