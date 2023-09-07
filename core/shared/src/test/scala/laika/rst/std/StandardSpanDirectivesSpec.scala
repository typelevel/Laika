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

package laika.rst.std

import laika.api.{ MarkupParser, RenderPhaseRewrite }
import laika.ast.Path.Root
import laika.ast.RelativePath.CurrentTree
import laika.ast.*
import laika.ast.sample.ParagraphCompanionShortcuts
import laika.format.{ AST, ReStructuredText }
import laika.parse.markup.DocumentParser.TransformationError
import laika.time.PlatformDateTime
import munit.FunSuite

/** @author Jens Halm
  */
class StandardSpanDirectivesSpec extends FunSuite with ParagraphCompanionShortcuts
    with RenderPhaseRewrite {

  private val imgTarget = InternalTarget(CurrentTree / "picture.jpg")

  private val parser = MarkupParser
    .of(ReStructuredText)
    .build

  def parse(input: String): Either[TransformationError, RootElement] =
    parser
      .parse(input, Root / "test.rst")
      .flatMap(rewrite(parser, AST))
      .map(_.content)

  def run(input: String, expected: Block*)(implicit loc: munit.Location): Unit =
    assertEquals(parse(input), Right(RootElement(expected)))

  test("image - without options") {
    val input = """.. |subst| image:: picture.jpg
                  |
                  |Some |subst|""".stripMargin
    run(input, p(Text("Some "), Image(imgTarget)))
  }

  test("image - with alt option") {
    val input = """.. |subst| image:: picture.jpg
                  | :alt: alt
                  |
                  |Some |subst|""".stripMargin
    run(input, p(Text("Some "), Image(imgTarget, alt = Some("alt"))))
  }

  test("image - with target option with a simple reference") {
    val input = """.. |subst| image:: picture.jpg
                  | :target: ref_
                  |
                  |.. _ref: http://foo.com/
                  |
                  |Some |subst|""".stripMargin
    run(input, p(Text("Some "), SpanLink.external("http://foo.com/")(Image(imgTarget))))
  }

  test("image - with target option with a phrase reference") {
    val input = """.. |subst| image:: picture.jpg
                  | :target: `some ref`_
                  |
                  |.. _`some ref`: http://foo.com/
                  |
                  |Some |subst|""".stripMargin
    run(input, p(Text("Some "), SpanLink.external("http://foo.com/")(Image(imgTarget))))
  }

  test("image - with target option with a uri") {
    val input = """.. |subst| image:: picture.jpg
                  | :target: http://foo.com/
                  |
                  |Some |subst|""".stripMargin
    run(input, p(Text("Some "), SpanLink.external("http://foo.com/")(Image(imgTarget))))
  }

  test("image - with class option") {
    val input = """.. |subst| image:: picture.jpg
                  | :class: foo
                  |
                  |Some |subst|""".stripMargin
    run(input, p(Text("Some "), Image(imgTarget, options = Styles("foo"))))
  }

  test("image - with align option") {
    val input = """.. |subst| image:: picture.jpg
                  | :align: top
                  |
                  |Some |subst|""".stripMargin
    run(input, p(Text("Some "), Image(imgTarget, options = Styles("align-top"))))
  }

  test("image - with width and height options") {
    val input          = """.. |subst| image:: picture.jpg
                  | :width: 200px
                  | :height: 120px
                  |
                  |Some |subst|""".stripMargin
    val expectedWidth  = Some(LengthUnit.px(200))
    val expectedHeight = Some(LengthUnit.px(120))
    run(input, p(Text("Some "), Image(imgTarget, width = expectedWidth, height = expectedHeight)))
  }

  test("image - with scale option") {
    val input          = """.. |subst| image:: picture.jpg
                  | :width: 200 px
                  | :height: 120 px
                  | :scale: 50%
                  |
                  |Some |subst|""".stripMargin
    val expectedWidth  = Some(LengthUnit.px(100))
    val expectedHeight = Some(LengthUnit.px(60))
    run(input, p(Text("Some "), Image(imgTarget, width = expectedWidth, height = expectedHeight)))
  }

  test("replace - regular inline markup") {
    val input = """.. |subst| replace:: *text* here
                  |
                  |Some |subst|""".stripMargin
    run(input, p(Text("Some "), SpanSequence(Emphasized("text"), Text(" here"))))
  }

  test("date - using the default pattern when no pattern is specified") {
    val input = """.. |subst| date::
                  |
                  |Some |subst|""".stripMargin
    val date  = PlatformDateTime.format(PlatformDateTime.now, "yyyy-MM-dd").toOption.get
    run(input, p(Text(s"Some $date")))
  }

  test("date - custom pattern") {
    val input = """.. |subst| date:: yyyy-MMM-dd
                  |
                  |Some |subst|""".stripMargin
    val date  = PlatformDateTime.format(PlatformDateTime.now, "yyyy-MMM-dd").toOption.get
    run(input, p(Text(s"Some $date")))
  }

  test("date - custom pattern with a time component") {

    /* Avoid flaky tests caused by potential time differences (1 second offset),
     * in particular when run on GitHub Actions with Scala.js
     */
    def stripMinutesAndSeconds(root: RootElement): RootElement =
      root.rewriteSpans { case Text(str, opt) =>
        Replace(Text(str.replaceFirst("[:]\\d\\d:\\d\\d", ":00:00"), opt))
      }

    val format = "yyyy-MM-dd HH:mm:ss"
    val input  = s""".. |subst| date:: $format
                   |
                   |Some |subst|""".stripMargin
    val date   = PlatformDateTime.format(PlatformDateTime.now, format).toOption.get
    val result = stripMinutesAndSeconds(RootElement(p(Text(s"Some $date"))))
    assertEquals(parse(input).map(stripMinutesAndSeconds), Right(result))
  }

  test("unicode - hex values starting with '0x' intertwined with normal text") {
    val input = """.. |subst| unicode:: 0xA9 Company
                  |
                  |Copyright |subst|""".stripMargin
    run(input, p(Text("Copyright " + '\u00a9'.toString + " Company")))
  }

  test("unicode - hex values starting with 'x' intertwined with normal text") {
    val input = """.. |subst| unicode:: xA9 Company
                  |
                  |Copyright |subst|""".stripMargin
    run(input, p(Text("Copyright " + '\u00a9'.toString + " Company")))
  }

  test("unicode - hex values starting with '\\x' intertwined with normal text") {
    val input = """.. |subst| unicode:: \xA9 Company
                  |
                  |Copyright |subst|""".stripMargin
    run(input, p(Text("Copyright " + '\u00a9'.toString + " Company")))
  }

  test("unicode - hex values starting with 'U+' intertwined with normal text") {
    val input = """.. |subst| unicode:: U+A9 Company
                  |
                  |Copyright |subst|""".stripMargin
    run(input, p(Text("Copyright " + '\u00a9'.toString + " Company")))
  }

  test("unicode - hex values starting with 'u' intertwined with normal text") {
    val input = """.. |subst| unicode:: uA9 Company
                  |
                  |Copyright |subst|""".stripMargin
    run(input, p(Text("Copyright " + '\u00a9'.toString + " Company")))
  }

  test("unicode - hex values starting with '\\u' intertwined with normal text") {
    val input = """.. |subst| unicode:: \""" + """uA9 Company
                                                 |
                                                 |Copyright |subst|""".stripMargin
    run(input, p(Text("Copyright " + '\u00a9'.toString + " Company")))
  }

  test("unicode - hex values as XML-style entities intertwined with normal text") {
    val input = """.. |subst| unicode:: &#xA9; Company
                  |
                  |Copyright |subst|""".stripMargin
    run(input, p(Text("Copyright " + '\u00a9'.toString + " Company")))
  }

  test("unicode - decimal values intertwined with normal text") {
    val input = """.. |subst| unicode:: 169 Company
                  |
                  |Copyright |subst|""".stripMargin
    run(input, p(Text("Copyright " + '\u00a9'.toString + " Company")))
  }

}
