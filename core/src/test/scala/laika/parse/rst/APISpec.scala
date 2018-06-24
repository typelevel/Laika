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
  
package laika.parse.rst

import laika.api._
import laika.directive.DirectiveRegistry
import laika.directive.Directives.{Blocks, Spans}
import laika.parse.rst.Directives.Parts._
import laika.parse.rst.Directives._
import laika.parse.rst.TextRoles._
import laika.tree.Elements._
import laika.tree.helper.ModelBuilder
import org.scalatest.{FlatSpec, Matchers}

class APISpec extends FlatSpec 
                 with Matchers
                 with ModelBuilder {
  
  
  "The API" should "support registration of block directives" in {
    val directives = List(
      BlockDirective("oneArg")(argument() map p),
      BlockDirective("twoArgs")((argument() ~ argument()) { (arg1,arg2) => p(arg1+arg2) })
    )
    val input = """.. oneArg:: arg
      |
      |.. twoArgs:: arg arg""".stripMargin
    (Parse as (ReStructuredText withBlockDirectives (directives:_*)) fromString input).content should be (root (p("arg"),p("argarg")))
  }
  
  it should "support registration of span directives" in {
    val directives = List(
      SpanDirective("oneArg")(argument() map (Text(_))),
      SpanDirective("twoArgs")((argument() ~ argument()) { (arg1,arg2) => Text(arg1+arg2) })
    )
    val input = """foo |one| foo |two|
      |
      |.. |one| oneArg:: arg
      |
      |.. |two| twoArgs:: arg arg""".stripMargin
    (Parse as (ReStructuredText withSpanDirectives (directives:_*)) fromString input).content should be (root 
        (p(txt("foo "), txt("arg"), txt(" foo "), txt("argarg"))))
  }
  
  it should "support registration of text roles" in {
    import laika.parse.rst.TextRoles.{Parts => P}
    val roles = List(
      TextRole("oneArg", "foo1")(P.field("name")) { (res,text) =>
       txt(res+text)
      },
      TextRole("twoArgs", "foo2")
        ((P.field("name1") ~ P.field("name2")) { (arg1,arg2) => arg1+arg2 }) 
        { (res,text) => txt(res+text) }
    )
    val input = """foo `one`:one: foo :two:`two`
      |
      |.. role::one(oneArg)
      | :name: val
      |
      |.. role::two(twoArgs)
      | :name1: val1
      | :name2: val2""".stripMargin
    (Parse as (ReStructuredText withTextRoles (roles:_*)) fromString input).content should be (root 
        (p(txt("foo "), txt("valone"), txt(" foo "), txt("val1val2two"))))
  }
  
  trait BlockDirectives {
    import Blocks.Combinators._
    import laika.directive.Directives.Default
    import laika.util.Builders._

    object TestDirectives extends DirectiveRegistry {

      val blockDirectives: List[Blocks.Directive] = List(
        Blocks.create("oneArg")(attribute(Default) map p),
        Blocks.create("twoArgs")((attribute(Default) ~ attribute("name")) { (arg1, arg2) => p(arg1 + arg2) })
      )

      val spanDirectives = Seq()
    }
  }
  
  trait SpanDirectives {
    import Spans.Combinators._
    import laika.directive.Directives.Default
    import laika.util.Builders._

    object TestDirectives extends DirectiveRegistry {

      val spanDirectives: List[Spans.Directive] = List(
        Spans.create("oneArg")(attribute(Default) map txt),
        Spans.create("twoArgs")((attribute(Default) ~ attribute("name")) { (arg1,arg2) => txt(arg1+arg2) })
      )

      val blockDirectives = Seq()
    }
  }
  
  it should "support the registration of Laika block directives" in {
    new BlockDirectives {
      val input = """@:oneArg arg.
        |
        |@:twoArgs arg1 name=arg2.""".stripMargin
      (Parse as ReStructuredText using TestDirectives fromString input).content should be (root (p("arg"),p("arg1arg2")))
    }
  }

  // TODO - resurrect tests for strict mode after its refactoring - merge the two APISpecs

  ignore should "ignore the registration of Laika block directives when run in strict mode" in {
    new BlockDirectives {
      val input = """@:oneArg arg.
        |
        |@:twoArgs arg1 name=arg2.""".stripMargin
      (Parse as ReStructuredText.strict using TestDirectives fromString input).content should be (root (p("@:oneArg arg."),p("@:twoArgs arg1 name=arg2.")))
    }
  }
  
  it should "support the registration of Laika span directives" in {
    new SpanDirectives {
      val input = """one @:oneArg arg. two @:twoArgs arg1 name=arg2. three"""
      (Parse as ReStructuredText using TestDirectives fromString input).content should be (root (p("one arg two arg1arg2 three")))
    }
  }

  ignore should "ignore the registration of Laika span directives when run in strict mode" in {
    new SpanDirectives {
      val input = """one @:oneArg arg. two @:twoArgs arg1 name=arg2. three"""
      (Parse as ReStructuredText.strict using TestDirectives fromString input).content should be (root (p("one @:oneArg arg. two @:twoArgs arg1 name=arg2. three")))
    }
  }
  
  it should "preprocess tabs" in {
    val input = " Line1\n\tLine2\n\tLine3"
    (Parse as ReStructuredText fromString input).content should be (root( quote(defList + ("Line1", p("Line2\nLine3")))))
  }
  

}
