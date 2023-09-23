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

import cats.data.NonEmptySet
import cats.implicits._
import laika.api.config.{ Config, ConfigBuilder }
import laika.ast._
import laika.ast.sample.{ ParagraphCompanionShortcuts, TestSourceBuilders }
import laika.config.IconRegistry
import munit.FunSuite

class StandardDirectiveSpec extends FunSuite
    with ParagraphCompanionShortcuts
    with TemplateParserSetup
    with MarkupParserSetup
    with TestSourceBuilders {

  def run(input: String, expectedContent: Block*)(implicit loc: munit.Location): Unit = {
    assertEquals(parse(input).map(_.content.content), Right(expectedContent))
  }

  def runFragment(input: String, expectedFragment: Block)(implicit loc: munit.Location): Unit = {
    val res = parse(input)
    assertEquals(res.map(_.content.content), Right(Seq(p("aa"), p("bb"))))
    assertEquals(res.map(_.fragments), Right(Map("foo" -> expectedFragment)))
  }

  def runTemplate(input: String, config: String, expectedContent: TemplateSpan*)(implicit
      loc: munit.Location
  ): Unit = {
    assertEquals(
      parseTemplateWithConfig(input, config),
      Right(RootElement(TemplateRoot(expectedContent)))
    )
  }

  def runTemplate(input: String, config: Config, expectedContent: TemplateSpan*)(implicit
      loc: munit.Location
  ): Unit = {
    assertEquals(
      parseTemplateWithConfig(input, config),
      Right(RootElement(TemplateRoot(expectedContent)))
    )
  }

  test("fragment directive with single paragraph") {
    val input = """aa
                  |
                  |@:fragment(foo)
                  |
                  |Fragment Text
                  |
                  |@:@
                  |
                  |bb""".stripMargin

    val expectedFragment = Paragraph(List(Text("Fragment Text")), Styles("foo"))
    runFragment(input, expectedFragment)
  }

  test("fragment directive with two paragraphs") {
    val input            = """aa
                  |
                  |@:fragment(foo)
                  |
                  |Line 1
                  |
                  |Line 2
                  |
                  |@:@
                  |
                  |bb""".stripMargin
    val expectedFragment = BlockSequence(List(p("Line 1"), p("Line 2")), Styles("foo"))
    runFragment(input, expectedFragment)
  }

  test("pageBreak directive") {
    val input = """aa
                  |
                  |@:pageBreak
                  |
                  |bb""".stripMargin
    run(input, p("aa"), PageBreak(), p("bb"))
  }

  test("todo directive as block") {
    val input = """aa
                  |
                  |@:todo(FIXME LATER)
                  |
                  |bb""".stripMargin
    run(input, p("aa"), BlockSequence(Nil), p("bb"))
  }

  test("todo directive as span") {
    val input = """aa @:todo(FIXME LATER) bb"""
    run(input, p(Text("aa "), SpanSequence(Nil), Text(" bb")))
  }

  private val resolvedTarget = InternalTarget.Resolved(
    absolutePath = Path.parse("/theme/theme.css"),
    relativePath = RelativePath.parse("../theme/theme.css")
  )

  test("target directive - translate a relative path") {
    val input = """aa @:target(theme.css) bb"""
    runTemplate(
      input,
      Config.empty,
      TemplateString("aa "),
      TemplateElement(RawLink(resolvedTarget)),
      TemplateString(" bb")
    )
  }

  test("target directive - translate an absolute path") {
    val input = """aa @:target(/theme/theme.css) bb"""
    runTemplate(
      input,
      Config.empty,
      TemplateString("aa "),
      TemplateElement(RawLink(resolvedTarget)),
      TemplateString(" bb")
    )
  }

  test("target directive - fail with an invalid target") {
    val dirSrc = "@:target(/theme/theme.css)"
    val input  = s"""aa $dirSrc bb"""
    val msg    =
      "One or more errors processing directive 'target': unresolved internal reference: ../theme/theme.css"
    runTemplate(
      input,
      "laika.links.validation.scope = global",
      TemplateString("aa "),
      TemplateElement(InvalidSpan(msg, source(dirSrc, input)).copy(fallback = Literal(dirSrc))),
      TemplateString(" bb")
    )
  }

  test("target directive - translate an absolute path from a config value") {
    val input = """aa @:target(var.link) bb"""
    runTemplate(
      input,
      "var.link = \"/theme/theme.css\"",
      TemplateString("aa "),
      TemplateElement(RawLink(resolvedTarget)),
      TemplateString(" bb")
    )
  }

  test("target directive - render an external target") {
    val input = """aa @:target(http://foo.com) bb"""
    runTemplate(
      input,
      Config.empty,
      TemplateString("aa "),
      TemplateElement(RawLink(ExternalTarget("http://foo.com"))),
      TemplateString(" bb")
    )
  }

  test("attribute directive - produce a string value for a valid config value") {
    val input = """<a @:attribute(src, foo.bar)/>"""
    runTemplate(
      input,
      "foo.bar = ../image.jpg",
      TemplateString("<a "),
      TemplateString("""src="../image.jpg""""),
      TemplateString("/>")
    )
  }

  test("attribute directive - produce an empty string for a missing config value") {
    val input = """<a @:attribute(src, foo.baz)/>"""
    runTemplate(
      input,
      "foo.bar = ../image.jpg",
      TemplateString("<a "),
      TemplateString(""),
      TemplateString("/>")
    )
  }

  test("attribute directive - fail with an invalid config value") {
    val dirSrc = "@:attribute(src, foo.bar)"
    val input  = s"""<a $dirSrc/>"""
    val msg    =
      "One or more errors processing directive 'attribute': value with key 'foo.bar' is a structured value (Array, Object, AST) which is not supported by this directive"
    runTemplate(
      input,
      "foo.bar = [1,2,3]",
      TemplateString("<a "),
      TemplateElement(InvalidSpan(msg, source(dirSrc, input)).copy(fallback = Literal(dirSrc))),
      TemplateString("/>")
    )
  }

  test("style directive - body with a single block") {
    val input = """aa
                  |
                  |@:style(foo)
                  |
                  |11
                  |22
                  |
                  |@:@
                  |
                  |bb""".stripMargin
    run(input, p("aa"), Paragraph(List(Text("11\n22")), Styles("foo")), p("bb"))
  }

  test("style directive - body containing a header") {
    val input = """aa
                  |
                  |@:style(foo)
                  |
                  |# Headline
                  |
                  |Text
                  |
                  |@:@
                  |
                  |bb""".stripMargin
    run(
      input,
      p("aa"),
      BlockSequence(Header(1, "Headline").withId("headline"), p("Text")).withStyles("foo"),
      p("bb")
    )
  }

  test("style directive - multiple styles") {
    val input = """aa
                  |
                  |@:style(foo,bar,baz)
                  |
                  |11
                  |22
                  |
                  |@:@
                  |
                  |bb""".stripMargin
    run(input, p("aa"), Paragraph(List(Text("11\n22")), Styles("foo", "bar", "baz")), p("bb"))
  }

  test("style directive - body with two blocks") {
    val input = """aa
                  |
                  |@:style(foo)
                  |
                  |11
                  |22
                  |
                  |33
                  |
                  |@:@
                  |
                  |bb""".stripMargin
    run(input, p("aa"), BlockSequence(List(p("11\n22"), p("33")), Styles("foo")), p("bb"))
  }

  test("style directive - single nested span") {
    val input = """aa @:style(foo) 11 @:@ bb"""
    run(
      input,
      p(
        Text("aa "),
        Text(" 11 ", Styles("foo")),
        Text(" bb")
      )
    )
  }

  test("style directive - two nested spans") {
    val input = """aa @:style(foo) 11 *22* 33 @:@ bb"""
    run(
      input,
      p(
        Text("aa "),
        SpanSequence(
          Text(" 11 "),
          Emphasized("22"),
          Text(" 33 ")
        ).withStyles("foo"),
        Text(" bb")
      )
    )
  }

  test("style directive - including link") {
    val input =
      """aa @:style(foo) 11 [link][id] 33 @:@ bb
        |
        |[id]: http://foo.com
        |""".stripMargin
    run(
      input,
      p(
        Text("aa "),
        SpanSequence(
          Text(" 11 "),
          SpanLink.external("http://foo.com")("link"),
          Text(" 33 ")
        ).withStyles("foo"),
        Text(" bb")
      )
    )
  }

  test("icon directive - success") {
    val icon   = IconStyle("open").withStyles("foo")
    val config = ConfigBuilder.empty.withValue(IconRegistry("foo" -> icon)).build
    val input  = """aa @:icon(foo) bb"""
    runTemplate(input, config, TemplateString("aa "), TemplateElement(icon), TemplateString(" bb"))
  }

  test("icon directive - fail when the specified icon does not exist") {
    val dirSrc = "@:icon(foo)"
    val input  = s"aa $dirSrc bb"
    val msg    = "Unresolved icon reference with key 'foo'"
    assertEquals(
      parseAndRewriteTemplate(input),
      Right(
        RootElement(
          TemplateRoot(
            TemplateString("aa "),
            TemplateElement(
              InvalidSpan(msg, source(dirSrc, input)).copy(fallback = Literal(dirSrc))
            ),
            TemplateString(" bb")
          )
        )
      )
    )
  }

  test("callout directive - body with a single block") {
    val input = """aa
                  |
                  |@:callout(info)
                  |
                  |11
                  |22
                  |
                  |@:@
                  |
                  |bb""".stripMargin
    run(input, p("aa"), BlockSequence("11\n22").withStyles("callout", "info"), p("bb"))
  }

  test("callout directive - body with a link reference") {
    val input = """aa
                  |
                  |@:callout(info)
                  |
                  |11 [link][id] 22
                  |
                  |@:@
                  |
                  |[id]: http://foo.com
                  |
                  |bb""".stripMargin
    run(
      input,
      p("aa"),
      BlockSequence(
        p(Text("11 "), SpanLink.external("http://foo.com")("link"), Text(" 22"))
      ).withStyles("callout", "info"),
      p("bb")
    )
  }

  test("callout directive - body with two blocks") {
    val input = """aa
                  |
                  |@:callout(info)
                  |
                  |11
                  |22
                  |
                  |33
                  |
                  |@:@
                  |
                  |bb""".stripMargin
    run(
      input,
      p("aa"),
      BlockSequence(List(p("11\n22"), p("33")), Styles("callout", "info")),
      p("bb")
    )
  }

  test("format directive - body with a single paragraph") {
    val input = """aa
                  |
                  |@:format(foo)
                  |
                  |11
                  |22
                  |
                  |@:@
                  |
                  |bb""".stripMargin
    run(input, p("aa"), TargetFormat(NonEmptySet.one("foo"), p("11\n22")), p("bb"))
  }

  test("format directive - body with two paragraphs") {
    val input = """aa
                  |
                  |@:format(foo)
                  |
                  |11
                  |22
                  |
                  |33
                  |
                  |@:@
                  |
                  |bb""".stripMargin
    run(
      input,
      p("aa"),
      TargetFormat(
        NonEmptySet.one("foo"),
        BlockSequence(
          p("11\n22"),
          p("33")
        )
      ),
      p("bb")
    )
  }

  test("format directive - multiple formats") {
    val input = """aa
                  |
                  |@:format(foo, bar, baz)
                  |
                  |11
                  |22
                  |
                  |@:@
                  |
                  |bb""".stripMargin
    run(input, p("aa"), TargetFormat(NonEmptySet.of("foo", "bar", "baz"), p("11\n22")), p("bb"))
  }

}
