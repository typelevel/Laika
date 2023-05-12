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

package laika.parse.css

import laika.ast.StylePredicate._
import laika.ast._
import laika.ast.sample.StyleBuilders
import laika.parse.Parser
import laika.parse.css.CSSParsers._
import munit.FunSuite

class CSSParsersSpec extends FunSuite with StyleBuilders {

  val defaultParser: Parser[Set[StyleDeclaration]] = styleDeclarationSet

  def run(input: String, expected: StyleDeclaration*)(implicit loc: munit.Location): Unit =
    assertEquals(defaultParser.parse(input).toEither, Right(expected.toSet))

  test("parse a style with a type selector") {

    val css = """Type {
                | foo: bar;
                |}""".stripMargin

    run(css, styleDecl(ElementType("Type")))
  }

  test("parse a style with a class selector") {

    val css = """.class {
                | foo: bar;
                |}""".stripMargin

    run(css, styleDecl(StyleName("class")))
  }

  test("parse a style with an id selector") {

    val css = """#id {
                | foo: bar;
                |}""".stripMargin

    run(css, styleDecl(StylePredicate.Id("id")))
  }

  test("parse a style with a type and class selector") {

    val css = """Type.class {
                | foo: bar;
                |}""".stripMargin

    run(css, styleDecl(ElementType("Type"), StyleName("class")))
  }

  test("parse a style with a type and id selector") {

    val css = """Type#id {
                | foo: bar;
                |}""".stripMargin

    run(css, styleDecl(ElementType("Type"), StylePredicate.Id("id")))
  }

  test("parse a style with a child selector") {

    val css = """Type .class {
                | foo: bar;
                |}""".stripMargin

    run(
      css,
      styleDecl(
        selector(selector(StyleName("class")), selector(ElementType("Type")), immediate = false)
      )
    )
  }

  test("parse a style with an immediate child selector") {

    val css = """Type > .class {
                | foo: bar;
                |}""".stripMargin

    run(
      css,
      styleDecl(
        selector(selector(StyleName("class")), selector(ElementType("Type")), immediate = true)
      )
    )
  }

  test("parse a style with multiple child selectors") {

    val css = """Type .class > #id {
                | foo: bar;
                |}""".stripMargin

    val top           = selector(ElementType("Type"))
    val middle        = selector(selector(StyleName("class")), top, immediate = false)
    val finalSelector = selector(selector(StylePredicate.Id("id")), middle, immediate = true)

    run(css, styleDecl(finalSelector))
  }

  test("parse a style with multiple selectors") {

    val css = """Type, .class {
                | foo: bar;
                |}""".stripMargin

    run(css, styleDecl(StyleName("class")).increaseOrderBy(1), styleDecl(ElementType("Type")))
  }

  test("parse multiple style declarations") {

    val css = """Type {
                |  foo: bar;
                |}
                |
                |.class {
                |  bar: foo;
                |}""".stripMargin

    run(
      css,
      styleDecl(Map("bar" -> "foo"), StyleName("class")).increaseOrderBy(1),
      styleDecl(ElementType("Type"))
    )
  }

  test("parse multiple style declarations with comments") {

    val css = """/* comment 1 */
                |Type {
                |  foo: bar;
                |}
                |
                |/* comment 2 */
                |
                |.class {
                |  bar: foo;
                |}
                |
                |/* comment 3 */""".stripMargin

    run(
      css,
      styleDecl(Map("bar" -> "foo"), StyleName("class")).increaseOrderBy(1),
      styleDecl(ElementType("Type"))
    )
  }

  test("parse a style with multiple comments") {

    val css = """Type {
                | /* comment 1 */
                | /* comment 2 */ foo: bar;
                | /* comment 3 */
                | bar: foo; /* comment 4 */
                | /* comment 5 */
                | foo: bar;
                |}""".stripMargin

    run(css, styleDecl(Map("foo" -> "bar", "bar" -> "foo"), ElementType("Type")))
  }

}
