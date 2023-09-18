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

package laika.ast

import laika.ast.Path.Root
import laika.ast.styles.StylePredicate.*
import laika.ast.sample.StyleBuilders
import laika.ast.styles.{ StyleDeclarationSet, StylePredicate }
import munit.FunSuite

class StyleMatchingSpec extends FunSuite with StyleBuilders {

  val styles = new StyleDeclarationSet(
    Set(Root),
    Set(
      styleDecl(Map("selector" -> "type", "foo" -> "bar"), ElementType("Paragraph")),
      styleDecl(Map("selector" -> "class"), StyleName("class")).increaseOrderBy(1),
      styleDecl(Map("selector" -> "id"), StylePredicate.Id("id")).increaseOrderBy(2),
      styleDecl(
        Map("selector" -> "child"),
        selector(selector(StyleName("child")), selector(ElementType("Paragraph")), false)
      ).increaseOrderBy(3),
      styleDecl(
        Map("selector" -> "immediateChild"),
        selector(selector(StyleName("immediateChild")), selector(ElementType("Paragraph")), true)
      ).increaseOrderBy(4),
      styleDecl(
        Map("selector" -> "twoChildren"), {
          val top    = selector(ElementType("Paragraph"))
          val middle = selector(selector(StyleName("middle")), top, false)
          selector(selector(StylePredicate.Id("bottom")), middle, true)
        }
      ).increaseOrderBy(5),
      styleDecl(
        Map("selector" -> "twoSelectors", "bar" -> "foo"),
        selector(ElementType("Paragraph"), StyleName("twoSelectors"))
      ).increaseOrderBy(6),
      styleDecl(Map("selector" -> "pos7", "foo" -> "bar"), StyleName("pos")).increaseOrderBy(7),
      styleDecl(Map("selector" -> "pos8", "bar" -> "foo"), StyleName("pos")).increaseOrderBy(8)
    )
  )

  private val emptyResult: Map[String, String] = Map.empty

  test("not collect any styles when no selector matches") {
    assert(styles.collectStyles(Text(""), Nil).isEmpty)
  }

  test("collect styles with matching type selectors") {
    assertEquals(
      styles.collectStyles(Paragraph.empty, Nil),
      Map("selector" -> "type", "foo" -> "bar")
    )
  }

  test("collect styles with matching class selectors") {
    assertEquals(
      styles.collectStyles(Text("", laika.ast.Styles("class")), Nil),
      Map("selector" -> "class")
    )
  }

  test("collect styles with matching id selectors") {
    assertEquals(styles.collectStyles(Text("", laika.ast.Id("id")), Nil), Map("selector" -> "id"))
  }

  test("collect styles with matching type and class selectors") {
    assertEquals(
      styles.collectStyles(Paragraph(Nil, laika.ast.Styles("twoSelectors")), Nil),
      Map("selector" -> "twoSelectors", "foo" -> "bar", "bar" -> "foo")
    )
  }

  test("collect styles with matching parent and child selectors") {
    assertEquals(
      styles.collectStyles(Text("", laika.ast.Styles("child")), List(Paragraph.empty)),
      Map("selector" -> "child")
    )
  }

  test("not collect any styles when the parent selector does not match") {
    assertEquals(
      styles.collectStyles(Text("", laika.ast.Styles("child")), List(Emphasized(Nil))),
      emptyResult
    )
  }

  test("collect styles with matching parent and immediate child selectors") {
    assertEquals(
      styles.collectStyles(Text("", laika.ast.Styles("immediateChild")), List(Paragraph.empty)),
      Map("selector" -> "immediateChild")
    )
  }

  test("not collect any styles when the matching parent selector is not an immediate parent") {
    assertEquals(
      styles.collectStyles(
        Text("", laika.ast.Styles("immediateChild")),
        List(Emphasized(Nil), Paragraph.empty)
      ),
      emptyResult
    )
  }

  test("collect styles with two matching parent selectors") {
    assertEquals(
      styles.collectStyles(
        Text("", laika.ast.Id("bottom")),
        List(Emphasized(Nil, laika.ast.Styles("middle")), Paragraph.empty)
      ),
      Map("selector" -> "twoChildren")
    )
  }

  test("not collect any styles when one of the parent selectors does not match") {
    assertEquals(
      styles.collectStyles(
        Text("", laika.ast.Id("bottom")),
        List(Emphasized(Nil, laika.ast.Styles("middle")), Header(1, Nil))
      ),
      emptyResult
    )
  }

  test("should apply styles based on order if two declarations have the same specificity") {
    assertEquals(
      styles.collectStyles(Text("", laika.ast.Styles("pos")), Nil),
      Map("selector" -> "pos8", "foo" -> "bar", "bar" -> "foo")
    )
  }

}
