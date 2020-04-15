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

import cats.implicits._
import laika.api.MarkupParser
import laika.ast.Text
import laika.ast.helper.ModelBuilder
import laika.directive.DirectiveRegistry
import laika.format.Markdown
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class APISpec extends AnyFlatSpec 
                 with Matchers
                 with ModelBuilder {
  
  
  trait BlockDirectives {
    import laika.directive.Blocks
    import Blocks.dsl._

    object TestDirectives extends DirectiveRegistry {

      val blockDirectives: List[Blocks.Directive] = List(
        Blocks.create("oneArg")(defaultAttribute.as[String] map p),
        Blocks.create("twoArgs") {
          (defaultAttribute.as[String], attribute("name").as[String].widen).mapN { 
            (arg1, arg2) => p(arg1 + arg2) 
          }
        },
        Blocks.create("inheritedArg") {
          attribute("name").as[String].inherited.widen.map(p)
        }
      )

      val spanDirectives = Seq()
      val templateDirectives = Seq()
      val linkDirectives = Seq()
    }

  }
  
  trait SpanDirectives {
    import laika.directive.Spans
    import Spans.dsl._

    object TestDirectives extends DirectiveRegistry {

      val spanDirectives: List[Spans.Directive] = List(
        Spans.create("oneArg")(defaultAttribute.as[String].map(Text(_))),
        Spans.create("twoArgs") {
          (defaultAttribute.as[String], attribute("name").as[String].widen).mapN { 
            (arg1, arg2) => Text(arg1+arg2) 
          }
        }
      )

      val blockDirectives = Seq()
      val templateDirectives = Seq()
      val linkDirectives = Seq()
    }
  }
  
  "The API" should "support the registration of block directives" in {
    new BlockDirectives {
      val input = """@:oneArg { arg }
        |
        |@:twoArgs arg1 name=arg2.""".stripMargin
      MarkupParser.of(Markdown).using(TestDirectives).build.parse(input).toOption.get.content should be (root (p("arg"),p("arg1arg2")))
    }
  }

  it should "ignore the registration of block directives when run in strict mode" in {
    new BlockDirectives {
      val input = """@:oneArg { arg }
        |
        |@:twoArgs { arg1 name=arg2 }""".stripMargin
      MarkupParser.of(Markdown).using(TestDirectives).strict.build.parse(input).toOption.get.content should be (root (p("@:oneArg { arg }"),p("@:twoArgs { arg1 name=arg2 }")))
    }
  }
  
  it should "support the registration of block directives with inherited attributes" in {
    new BlockDirectives {
      val input = """{% name = fromHeader %}
        |  
        |@:oneArg { arg }
        |
        |@:inheritedArg""".stripMargin
      MarkupParser.of(Markdown).using(TestDirectives).build.parse(input).toOption.get.content should be (root (p("arg"),p("fromHeader")))
    }
  }
  
  it should "support the registration of span directives" in {
    new SpanDirectives {
      val input = """one @:oneArg { arg } two @:twoArgs { arg1, name=arg2 } three""".stripMargin
      MarkupParser.of(Markdown).using(TestDirectives).build.parse(input).toOption.get.content should be (root (p("one arg two arg1arg2 three")))
    }
  }

  it should "ignore the registration of span directives when run in strict mode" in {
    new SpanDirectives {
      val input = """one @:oneArg { arg } two @:twoArgs { arg1, name=arg2 } three"""
      MarkupParser.of(Markdown).using(TestDirectives).strict.build.parse(input).toOption.get.content should be (root (p("one @:oneArg { arg } two @:twoArgs { arg1, name=arg2 } three")))
    }
  }
  

}
