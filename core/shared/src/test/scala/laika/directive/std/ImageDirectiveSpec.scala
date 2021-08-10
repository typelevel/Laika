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

package laika.directive.std

import laika.ast.Path.Root
import laika.ast.RelativePath.CurrentTree
import laika.ast.sample.ParagraphCompanionShortcuts
import laika.ast.{BlockSequence, Image, InternalTarget, LengthUnit, Options, RootElement, SpanSequence, Styles, Text}
import munit.FunSuite

class ImageDirectiveSpec extends FunSuite with ParagraphCompanionShortcuts with MarkupParserSetup {


  val imgTarget = InternalTarget(CurrentTree / "picture.jpg")
  val resolvedImageTarget = InternalTarget(CurrentTree / "picture.jpg").relativeTo(Root / "doc")

  val defaultBlockStyle = Styles("default-image-block")
  val defaultSpanStyle = Styles("default-image-span")

  def blocks(directive: String): String =
    s"""aaa
       |
    |$directive
       |
    |bbb""".stripMargin

  def runBlocks (input: String, expected: Image, options: Options = defaultBlockStyle): Unit = 
    assertEquals(
      parse(blocks(input)).map(_.content), 
      Right(RootElement(
        p("aaa"),
        BlockSequence(Seq(SpanSequence(expected)), options),
        p("bbb")
      ))
    )
    
  def runSpans (input: String, expected: Image): Unit =
    assertEquals(
      parse(input).map(_.content),
      Right(RootElement(p(Text("aaa "), expected, Text(" bbb"))))
    )
  
  
  test("block directive - without any HOCON attributes") {
    runBlocks("@:image(picture.jpg)", Image(resolvedImageTarget))
  }

  test("block directive - alt and title attributes") {
    val input = """@:image(picture.jpg) {
                  |  alt = alt
                  |  title = desc
                  |}""".stripMargin
    runBlocks(input, Image(resolvedImageTarget, alt = Some("alt"), title = Some("desc")))
  }

  test("block directive - intrinsicWidth and intrinsicHeight attributes") {
    val input = """@:image(picture.jpg) {
                  |  intrinsicWidth = 320
                  |  intrinsicHeight = 240
                  |}""".stripMargin
    val result = Image(resolvedImageTarget, width = Some(LengthUnit.px(320)), height = Some(LengthUnit.px(240)))
    runBlocks(input, result)
  }

  test("block directive -  style attribute") {
    val input = """@:image(picture.jpg) {
                  |  style = small-image
                  |}""".stripMargin
    runBlocks(input, Image(resolvedImageTarget), Styles("small-image"))
  }

  test("span directive - without any HOCON attributes") {
    runSpans("aaa @:image(picture.jpg) bbb", Image(resolvedImageTarget, options = defaultSpanStyle))
  }

  test("span directive - alt and title attributes") {
    val input = """aaa @:image(picture.jpg) {
                  |  alt = alt
                  |  title = desc
                  |} bbb""".stripMargin
    val result = Image(resolvedImageTarget, alt = Some("alt"), title = Some("desc"), options = defaultSpanStyle)
    runSpans(input, result)
  }

  test("span directive - intrinsicWidth and intrinsicHeight attributes") {
    val input = """aaa @:image(picture.jpg) {
                  |  intrinsicWidth = 320
                  |  intrinsicHeight = 240
                  |} bbb""".stripMargin
    val result = Image(resolvedImageTarget, width = Some(LengthUnit.px(320)), height = Some(LengthUnit.px(240)), options = defaultSpanStyle)
    runSpans(input, result)
  }

  test("span directive - style attribute") {
    val input = """aaa @:image(picture.jpg) {
                  |  style = small-image
                  |} bbb""".stripMargin
    val result = Image(resolvedImageTarget, options = Styles("small-image"))
    runSpans(input, result)
  }

}
