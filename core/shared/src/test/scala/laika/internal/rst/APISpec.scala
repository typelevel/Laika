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

package laika.internal.rst

import cats.syntax.all.*
import laika.api._
import laika.api.bundle.{ BlockDirectives, DirectiveRegistry, SpanDirectives }
import laika.ast._
import laika.ast.sample.ParagraphCompanionShortcuts
import laika.format.{ AST, ReStructuredText }
import laika.parse.builders.~
import laika.internal.rst.ext.Directives.Parts._
import laika.internal.rst.ext.Directives._
import laika.internal.rst.ext.ExtensionProvider
import laika.internal.rst.ext.TextRoles._
import munit.FunSuite

class APISpec extends FunSuite with ParagraphCompanionShortcuts with RenderPhaseRewrite {

  test("registration of native block directives") {
    val directives = List(
      BlockDirective("oneArg")(argument() map p),
      BlockDirective("twoArgs")((argument() ~ argument()).map { case arg1 ~ arg2 =>
        p(arg1 + arg2)
      })
    )
    val input      = """.. oneArg:: arg
                  |
                  |.. twoArgs:: arg arg""".stripMargin
    val parser     = MarkupParser
      .of(ReStructuredText)
      .using(ExtensionProvider.forExtensions(blocks = directives))
      .build
    val res        = parser
      .parse(input)
      .flatMap(rewrite(parser, AST))
      .map(_.content)
    assertEquals(res, Right(RootElement(p("arg"), p("argarg"))))
  }

  test("registration of native span directives") {
    val directives = List(
      SpanDirective("oneArg")(argument() map (Text(_))),
      SpanDirective("twoArgs")((argument() ~ argument()).map { case arg1 ~ arg2 =>
        Text(arg1 + arg2)
      })
    )
    val input      = """foo |one| foo |two|
                  |
                  |.. |one| oneArg:: arg
                  |
                  |.. |two| twoArgs:: arg arg""".stripMargin
    val parser     = MarkupParser
      .of(ReStructuredText)
      .using(ExtensionProvider.forExtensions(spans = directives))
      .build
    val res        = parser
      .parse(input)
      .flatMap(rewrite(parser, AST))
      .map(_.content)
    assertEquals(res, Right(RootElement(p(Text("foo arg foo argarg")))))
  }

  test("registration of text roles") {
    import laika.internal.rst.ext.TextRoles.{ Parts => P }
    val roles  = List(
      TextRole("oneArg", "foo1")(P.field("name")) { (res, text) =>
        Text(res + text)
      },
      TextRole("twoArgs", "foo2")((P.field("name1") ~ P.field("name2")).map { case arg1 ~ arg2 =>
        arg1 + arg2
      }) { (res, text) => Text(res + text) }
    )
    val input  = """foo `one`:one: foo :two:`two`
                  |
                  |.. role::one(oneArg)
                  | :name: val
                  |
                  |.. role::two(twoArgs)
                  | :name1: val1
                  | :name2: val2""".stripMargin
    val parser = MarkupParser
      .of(ReStructuredText)
      .using(ExtensionProvider.forExtensions(roles = roles))
      .build
    val res    = parser
      .parse(input)
      .flatMap(rewrite(parser, AST))
      .map(_.content)
    assertEquals(res, Right(RootElement(p(Text("foo valone foo val1val2two")))))
  }

  object BlockTestDirectives {
    import BlockDirectives.dsl._

    object Registry extends DirectiveRegistry {

      val blockDirectives: List[BlockDirectives.Directive] = List(
        BlockDirectives.create("oneArg")(attribute(0).as[String] map p),
        BlockDirectives.create("twoArgs") {
          (attribute(0).as[String], attribute("name").as[String].widen).mapN { (arg1, arg2) =>
            p(arg1 + arg2)
          }
        }
      )

      val spanDirectives     = Seq()
      val templateDirectives = Seq()
      val linkDirectives     = Seq()
    }

  }

  object SpanTestDirectives {
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

  }

  test("registration of Laika block directives") {
    val input = """@:oneArg(arg)
                  |
                  |@:twoArgs(arg1) { name=arg2 }""".stripMargin
    val res   = MarkupParser
      .of(ReStructuredText)
      .using(BlockTestDirectives.Registry)
      .build
      .parse(input)
      .map(_.content)
    assertEquals(res, Right(RootElement(p("arg"), p("arg1arg2"))))
  }

  test("ignore registration of Laika block directives in strict mode") {
    val input = """@:oneArg(arg)
                  |
                  |@:twoArgs(arg1) { name=arg2 }""".stripMargin
    val res   = MarkupParser
      .of(ReStructuredText)
      .using(BlockTestDirectives.Registry)
      .strict
      .build
      .parse(input)
      .map(_.content)
    assertEquals(res, Right(RootElement(p("@:oneArg(arg)"), p("@:twoArgs(arg1) { name=arg2 }"))))
  }

  test("registration of Laika span directives") {
    val input  = """one @:oneArg(arg) two @:twoArgs(arg1) { name=arg2 } three"""
    val parser = MarkupParser
      .of(ReStructuredText)
      .using(SpanTestDirectives.Registry)
      .build
    val res    = parser
      .parse(input)
      .flatMap(rewrite(parser, AST))
      .map(_.content)
    assertEquals(res, Right(RootElement(p("one arg two arg1arg2 three"))))
  }

  test("ignore registration of Laika span directives in strict mode") {
    val input = """one @:oneArg(arg) two @:twoArgs(arg1) { name=arg2 } three"""
    val res   = MarkupParser
      .of(ReStructuredText)
      .using(SpanTestDirectives.Registry)
      .strict
      .build
      .parse(input)
      .map(_.content)
    assertEquals(
      res,
      Right(RootElement(p("one @:oneArg(arg) two @:twoArgs(arg1) { name=arg2 } three")))
    )
  }

  test("pre-process tabs") {
    val input = " Line1\n\tLine2\n\tLine3"
    val list  = DefinitionList(
      DefinitionListItem("Line1", p("Line2\nLine3"))
    )
    val res   = MarkupParser
      .of(ReStructuredText)
      .build
      .parse(input)
      .map(_.content)
    assertEquals(res, Right(RootElement(QuotedBlock(list))))
  }

}
