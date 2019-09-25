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

import com.typesafe.config.ConfigFactory
import laika.ast.Path.Root
import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.bundle.ParserBundle
import laika.parse.Parser
import laika.parse.helper.{DefaultParserHelpers, ParseResultHelpers}
import laika.parse.markup.RootParserProvider
import laika.rewrite.TemplateRewriter
import org.scalatest.{FlatSpec, Matchers}

class LegacySpanDirectiveAPISpec extends FlatSpec
                          with Matchers
                          with ModelBuilder {

  
  object DirectiveSetup {
    import Spans.dsl._

    trait RequiredDefaultAttribute {
      val directive = Spans.create("dir") { attribute(Default) map (Text(_)) }
    }
    
    trait OptionalDefaultAttribute {
      val directive = Spans.create("dir") { 
        attribute(Default, positiveInt).optional map (num => Text(num.map(_.toString).getOrElse("<>"))) 
      }
    }
    
    trait RequiredNamedAttribute {
      val directive = Spans.create("dir") { attribute("name") map (Text(_)) }
    }
    
    trait OptionalNamedAttribute {
      val directive = Spans.create("dir") { 
        attribute("name", positiveInt).optional map (num => Text(num.map(_.toString).getOrElse("<>"))) 
      }
    }
    
    trait RequiredDefaultBody {
      val directive = Spans.create("dir") { body map (SpanSequence(_)) }
    }
    
    trait FullDirectiveSpec {
      val directive = Spans.create("dir") {
        (attribute(Default) ~ attribute("strAttr").optional ~ attribute("intAttr", positiveInt).optional ~
        body).map {
          case defAttr ~ strAttr ~ intAttr ~ defBody => 
            val sum = intAttr.getOrElse(0)
            val str = defAttr + ":" + strAttr.getOrElse("..") + ":" + sum
            SpanSequence(Text(str) +: defBody)
        }
      }
    }
    
    trait DirectiveWithParserAccess {
      val directive = Spans.create("dir") { 
        (body(string) ~ parser).map {
          case body ~ parser => SpanSequence(parser(body.drop(3)))
        }
      }
    }
    
    trait DirectiveWithContextAccess {
      val directive = Spans.create("dir") { 
        (body(string) ~ cursor).map {
          case body ~ cursor => Text(body + cursor.target.path)
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
    ).recursiveSpans ^^ { spans =>
      val seq = SpanSequence(spans)
      TemplateRewriter.rewriteRules(DocumentCursor(
        Document(Root, RootElement(Seq(seq)), config = ConfigFactory.parseString("ref: value"))
      )).rewriteSpan(seq)
    }
    
    def invalid (input: String, error: String): InvalidSpan = InvalidElement(error, input).asSpan
        
    def ss (spans: Span*): Span = SpanSequence(spans)

  }
  

  import DirectiveSetup._
  
  "The span directive parser" should "parse a directive with one required default string attribute" in {
    new SpanParser with RequiredDefaultAttribute {
      Parsing ("aa @:dir foo. bb") should produce (ss(txt("aa foo bb")))
    }
  }
  
  it should "detect a directive with a missing required default attribute" in {
    new SpanParser with RequiredDefaultAttribute {
      val msg = "One or more errors processing directive 'dir': required default attribute is missing"
      Parsing ("aa @:dir. bb") should produce (ss(txt("aa "), invalid("@:dir.",msg), txt(" bb")))
    }
  }
  
  it should "parse a directive with an optional default int attribute" in {
    new SpanParser with OptionalDefaultAttribute {
      Parsing ("aa @:dir 5. bb") should produce (ss(txt("aa 5 bb")))
    }
  }
  
  it should "detect a directive with an optional invalid default int attribute" in {
    new SpanParser with OptionalDefaultAttribute {
      val msg = "One or more errors processing directive 'dir': error converting default attribute: not an integer: foo"
      Parsing ("aa @:dir foo. bb") should produce (ss(txt("aa "), invalid("@:dir foo.",msg), txt(" bb")))
    }
  }
  
  it should "parse a directive with a missing optional default int attribute" in {
    new SpanParser with OptionalDefaultAttribute {
      Parsing ("aa @:dir. bb") should produce (ss(txt("aa <> bb")))
    }
  }
  
  it should "parse a directive with one required named string attribute" in {
    new SpanParser with RequiredNamedAttribute {
      Parsing ("aa @:dir name=foo. bb") should produce (ss(txt("aa foo bb")))
    }
  }
  
  it should "parse a directive with a named string attribute value in quotes" in {
    new SpanParser with RequiredNamedAttribute {
      Parsing ("""aa @:dir name="foo bar". bb""") should produce (ss(txt("aa foo bar bb")))
    }
  }
  
  it should "detect a directive with a missing required named attribute" in {
    new SpanParser with RequiredNamedAttribute {
      val msg = "One or more errors processing directive 'dir': required attribute with name 'name' is missing"
      Parsing ("aa @:dir. bb") should produce (ss(txt("aa "), invalid("@:dir.",msg), txt(" bb")))
    }
  }
  
  it should "parse a directive with an optional named int attribute" in {
    new SpanParser with OptionalNamedAttribute {
      Parsing ("aa @:dir name=5. bb") should produce (ss(txt("aa 5 bb")))
    }
  }
  
  it should "detect a directive with an optional invalid named int attribute" in {
    new SpanParser with OptionalNamedAttribute {
      val msg = "One or more errors processing directive 'dir': error converting attribute with name 'name': not an integer: foo"
      Parsing ("aa @:dir name=foo. bb") should produce (ss(txt("aa "), invalid("@:dir name=foo.",msg), txt(" bb")))
    }
  }
  
  it should "parse a directive with a missing optional named int attribute" in {
    new SpanParser with OptionalNamedAttribute {
      val msg = "One or more errors processing directive 'dir': required default attribute is missing"
      Parsing ("aa @:dir. bb") should produce (ss(txt("aa <> bb")))
    }
  }
  
  it should "parse a directive with a required default body" in {
    new SpanParser with RequiredDefaultBody {
      val body = ss(txt(" some value text "))
      Parsing ("aa @:dir: { some {{config.ref}} text } bb") should produce (ss(txt("aa "), body, txt(" bb")))
    }
  }
  
  it should "support a directive with a nested pair of braces" in {
    new SpanParser with RequiredDefaultBody {
      val body = ss(txt(" some {ref} text "))
      Parsing ("aa @:dir: { some {ref} text } bb") should produce (ss(txt("aa "), body, txt(" bb")))
    }
  }
  
  it should "detect a directive with a missing required default body" in {
    new SpanParser with RequiredDefaultBody {
      val msg = "One or more errors processing directive 'dir': required body is missing"
      Parsing ("aa @:dir. bb") should produce (ss(txt("aa "), invalid("@:dir.",msg), txt(" bb")))
    }
  }
  
  it should "parse a full directive spec with all elements present" in {
    new FullDirectiveSpec with SpanParser {
      val body = ss(
        txt("foo:str:7 1 value 2 ")
      )
      Parsing ("aa @:dir foo strAttr=str intAttr=7: { 1 {{config.ref}} 2 } bb") should produce (ss(txt("aa "), body, txt(" bb")))
    }
  }
  
  it should "parse a full directive spec with all optional elements missing" in {
    new FullDirectiveSpec with SpanParser {
      val body = ss(
        txt("foo:..:0 1 value 2 ")
      )
      Parsing ("aa @:dir foo: { 1 {{config.ref}} 2 } bb") should produce (ss(txt("aa "), body, txt(" bb")))
    }
  }
  
  it should "detect a full directive spec with all one required attribute and one required body missing" in {
    new FullDirectiveSpec with SpanParser {
      val msg = "One or more errors processing directive 'dir': required default attribute is missing, required body is missing"
      Parsing ("aa @:dir strAttr=str. bb") should produce (ss(txt("aa "), invalid("@:dir strAttr=str.",msg), txt(" bb")))
    }
  }
  
  it should "parse a directive with a required default body and parser access" in {
    new DirectiveWithParserAccess with SpanParser {
      val body = ss(txt("me value text "))
      Parsing ("aa @:dir: { some {{config.ref}} text } bb") should produce (ss(txt("aa "), body, txt(" bb")))
    }
  }
  
  it should "parse a directive with a required default body and cursor access" in {
    new DirectiveWithContextAccess with SpanParser {
      Parsing ("aa @:dir: { text } bb") should produce (ss(txt("aa  text / bb")))
    }
  }
  
  it should "detect a directive with an unknown name" in {
    new SpanParser with OptionalNamedAttribute {
      val msg = "One or more errors processing directive 'foo': No span directive registered with name: foo"
      Parsing ("aa @:foo name=foo. bb") should produce (ss(txt("aa "), invalid("@:foo name=foo.",msg), txt(" bb")))
    }
  }
  
  
}
