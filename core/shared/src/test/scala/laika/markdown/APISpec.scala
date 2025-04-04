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

import cats.syntax.all.*
import laika.api.bundle.DirectiveRegistry
import laika.api.{ MarkupParser, RenderPhaseRewrite }
import laika.ast.sample.ParagraphCompanionShortcuts
import laika.ast.{ Block, RootElement, Text }
import laika.format.{ HTML, Markdown }
import munit.FunSuite

class APISpec extends FunSuite with ParagraphCompanionShortcuts with RenderPhaseRewrite {

  object BlockTestDirectives {

    import laika.api.bundle.BlockDirectives
    import BlockDirectives.dsl._

    object Registry extends DirectiveRegistry {

      val blockDirectives: List[BlockDirectives.Directive] = List(
        BlockDirectives.create("oneArg")(attribute(0).as[String] map p),
        BlockDirectives.create("twoArgs") {
          (attribute(0).as[String], attribute("name").as[String].widen).mapN { (arg1, arg2) =>
            p(arg1 + arg2)
          }
        },
        BlockDirectives.create("inheritedArg") {
          attribute("name").as[String].inherited.widen.map(p)
        }
      )

      val spanDirectives     = Seq()
      val templateDirectives = Seq()
      val linkDirectives     = Seq()
    }

    private lazy val parser       = MarkupParser.of(Markdown).using(Registry).build
    private lazy val strictParser = MarkupParser.of(Markdown).using(Registry).strict.build

    def runWith(input: String, parser: MarkupParser, expected: RootElement)(implicit
        loc: munit.Location
    ): Unit = {
      val res = parser.parse(input).flatMap(rewrite(parser, HTML)).map(_.content)
      assertEquals(res, Right(expected))
    }

    def run(input: String, expected: Block*)(implicit loc: munit.Location): Unit =
      runWith(input, parser, RootElement(expected))

    def runStrict(input: String, expected: Block*)(implicit loc: munit.Location): Unit =
      runWith(input, strictParser, RootElement(expected))

  }

  object SpanTestDirectives {

    import laika.api.bundle.SpanDirectives
    import SpanDirectives.dsl._

    object Registry extends DirectiveRegistry {

      val spanDirectives: List[SpanDirectives.Directive] = List(
        SpanDirectives.create("oneArg")(attribute(0).as[String].map(Text(_))),
        SpanDirectives.create("twoArgs") {
          (attribute(0).as[String], attribute("name").as[String].widen).mapN { (arg1, arg2) =>
            Text(arg1 + arg2)
          }
        }
      )

      val blockDirectives    = Seq()
      val templateDirectives = Seq()
      val linkDirectives     = Seq()
    }

    private lazy val parser       = MarkupParser.of(Markdown).using(Registry).build
    private lazy val strictParser = MarkupParser.of(Markdown).using(Registry).strict.build

    def runWith(input: String, parser: MarkupParser, expected: RootElement)(implicit
        loc: munit.Location
    ): Unit = {
      val res = parser.parse(input).flatMap(rewrite(parser, HTML)).map(_.content)
      assertEquals(res, Right(expected))
    }

    def run(input: String, expected: Block*)(implicit loc: munit.Location): Unit =
      runWith(input, parser, RootElement(expected))

    def runStrict(input: String, expected: Block*)(implicit loc: munit.Location): Unit =
      runWith(input, strictParser, RootElement(expected))

  }

  test("registration of block directives") {
    val input = """@:oneArg(arg)
                  |
                  |@:twoArgs(arg1) { name=arg2 }""".stripMargin
    BlockTestDirectives.run(input, p("arg"), p("arg1arg2"))
  }

  test("ignore the registration of block directives when run in strict mode") {
    val input = """@:oneArg(arg)
                  |
                  |@:twoArgs(arg1) { name=arg2 }""".stripMargin
    BlockTestDirectives.runStrict(input, p("@:oneArg(arg)"), p("@:twoArgs(arg1) { name=arg2 }"))
  }

  test("registration of block directives with inherited attributes") {
    val input = """{% name = fromHeader %}
                  |  
                  |@:oneArg(arg)
                  |
                  |@:inheritedArg""".stripMargin
    BlockTestDirectives.run(input, p("arg"), p("fromHeader"))
  }

  test("registration of span directives") {
    val input = """one @:oneArg(arg) two @:twoArgs(arg1) { name=arg2 } three""".stripMargin
    SpanTestDirectives.run(input, p("one arg two arg1arg2 three"))
  }

  test("ignore the registration of span directives when run in strict mode") {
    val input = """one @:oneArg(arg) two @:twoArgs(arg1) { name=arg2 } three"""
    SpanTestDirectives.runStrict(
      input,
      p("one @:oneArg(arg) two @:twoArgs(arg1) { name=arg2 } three")
    )
  }

}
