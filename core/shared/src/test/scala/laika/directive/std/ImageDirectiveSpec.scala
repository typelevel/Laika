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
import laika.ast.{BlockSequence, Image, InternalTarget, LengthUnit, RootElement, SpanSequence, Styles, Text}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ImageDirectiveSpec extends AnyFlatSpec
  with Matchers
  with ParagraphCompanionShortcuts
  with MarkupParserSetup {


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

  
  "The image block directive" should "succeed without any HOCON attributes" in {
    val result = RootElement(p("aaa"), BlockSequence(Seq(SpanSequence(Image(resolvedImageTarget))), defaultBlockStyle), p("bbb"))
    parse(blocks("@:image(picture.jpg)")).map(_.content) shouldBe Right(result)
  }

  it should "support the alt and title attributes" in {
    val input = """@:image(picture.jpg) {
                  |  alt = alt
                  |  title = desc
                  |}""".stripMargin
    val result = RootElement(p("aaa"), BlockSequence(Seq(SpanSequence(Image(resolvedImageTarget, alt = Some("alt"), title = Some("desc")))), defaultBlockStyle), p("bbb"))
    parse(blocks(input)).map(_.content) shouldBe Right(result)
  }

  it should "support the intrinsicWidth and intrinsicHeight attributes" in {
    val input = """@:image(picture.jpg) {
                  |  intrinsicWidth = 320
                  |  intrinsicHeight = 240
                  |}""".stripMargin
    val result = RootElement(p("aaa"), BlockSequence(Seq(SpanSequence(Image(resolvedImageTarget, width = Some(LengthUnit.px(320)), height = Some(LengthUnit.px(240))))), defaultBlockStyle), p("bbb"))
    parse(blocks(input)).map(_.content) shouldBe Right(result)
  }

  it should "support the style attribute" in {
    val input = """@:image(picture.jpg) {
                  |  style = small-image
                  |}""".stripMargin
    val result = RootElement(p("aaa"), BlockSequence(Seq(SpanSequence(Image(resolvedImageTarget))), Styles("small-image")), p("bbb"))
    parse(blocks(input)).map(_.content) shouldBe Right(result)
  }

  "The image span directive" should "succeed without any HOCON attributes" in {
    val result = RootElement(p(Text("aaa "), Image(resolvedImageTarget, options = defaultSpanStyle), Text(" bbb")))
    parse("aaa @:image(picture.jpg) bbb").map(_.content) shouldBe Right(result)
  }

  it should "support the alt and title attributes" in {
    val input = """aaa @:image(picture.jpg) {
                  |  alt = alt
                  |  title = desc
                  |} bbb""".stripMargin
    val result = RootElement(p(Text("aaa "), Image(resolvedImageTarget, alt = Some("alt"), title = Some("desc"), options = defaultSpanStyle), Text(" bbb")))
    parse(input).map(_.content) shouldBe Right(result)
  }

  it should "support the intrinsicWidth and intrinsicHeight attributes" in {
    val input = """aaa @:image(picture.jpg) {
                  |  intrinsicWidth = 320
                  |  intrinsicHeight = 240
                  |} bbb""".stripMargin
    val result = RootElement(p(Text("aaa "), Image(resolvedImageTarget, width = Some(LengthUnit.px(320)), height = Some(LengthUnit.px(240)), options = defaultSpanStyle), Text(" bbb")))
    parse(input).map(_.content) shouldBe Right(result)
  }

  it should "support the style attribute" in {
    val input = """aaa @:image(picture.jpg) {
                  |  style = small-image
                  |} bbb""".stripMargin
    val result = RootElement(p(Text("aaa "), Image(resolvedImageTarget, options = Styles("small-image")), Text(" bbb")))
    parse(input).map(_.content) shouldBe Right(result)
  }

}
