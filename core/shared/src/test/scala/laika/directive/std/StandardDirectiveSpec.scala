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
import laika.ast.Path.Root
import laika.ast._
import laika.ast.sample.{ParagraphCompanionShortcuts, TestSourceBuilders}
import laika.config.ConfigBuilder
import laika.parse.markup.DocumentParser.ParserError
import laika.rewrite.link.IconRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class StandardDirectiveSpec extends AnyFlatSpec
  with Matchers
  with ParagraphCompanionShortcuts
  with TemplateParserSetup 
  with MarkupParserSetup
  with TestSourceBuilders {

  def parseWithFragments (input: String, path: Path = Root / "doc"): Either[ParserError, (Map[String,Element], RootElement)] = {
    parse(input, path).map { doc =>
      (doc.fragments, doc.content)
    }
  }

  "The fragment directive" should "parse a fragment with a single paragraph" in {
    val input = """aa
                  |
                  |@:fragment(foo)
                  |
                  |Fragment Text
                  |
                  |@:@
                  |
                  |bb""".stripMargin
    val expectedFragments = Map("foo" -> Paragraph(List(Text("Fragment Text")), Styles("foo")))
    val expectedRoot      = RootElement(p("aa"), p("bb"))
    parseWithFragments(input) shouldBe Right((expectedFragments, expectedRoot))
  }

  it should "parse a fragment with a two paragraphs" in {
    val input = """aa
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
    val expectedFragments = Map("foo" -> BlockSequence(List(p("Line 1"), p("Line 2")), Styles("foo")))
    val expectedRoot      = RootElement(p("aa"), p("bb"))
    parseWithFragments(input) shouldBe Right((expectedFragments, expectedRoot))
  }


  "The pageBreak directive" should "parse an empty directive" in {
    val input = """aa
                  |
                  |@:pageBreak
                  |
                  |bb""".stripMargin
    parse(input).map(_.content) shouldBe Right(RootElement(p("aa"),PageBreak(),p("bb")))
  }


  "The todo directive" should "parse a block directive" in {
    val input = """aa
                  |
                  |@:todo(FIXME LATER)
                  |
                  |bb""".stripMargin
    parse(input).map(_.content) shouldBe Right(RootElement(p("aa"),BlockSequence(Nil),p("bb")))
  }

  it should "parse a span directive" in {
    val input = """aa @:todo(FIXME LATER) bb"""
    parse(input).map(_.content) shouldBe Right(RootElement(p(Text("aa "),SpanSequence(Nil),Text(" bb"))))
  }


  "The path directive" should "translate a relative path" in {
    val input = """aa @:path(theme.css) bb"""
    parseTemplateWithConfig(input, "laika.links.excludeFromValidation = [\"/\"]") shouldBe Right(RootElement(TemplateRoot(
      TemplateString("aa "),
      TemplateElement(RawLink.internal("../theme/theme.css")),
      TemplateString(" bb")
    )))
  }

  it should "translate an absolute path" in {
    val input = """aa @:path(/theme/theme.css) bb"""
    parseTemplateWithConfig(input, "laika.links.excludeFromValidation = [\"/\"]") shouldBe Right(RootElement(TemplateRoot(
      TemplateString("aa "),
      TemplateElement(RawLink.internal("../theme/theme.css")),
      TemplateString(" bb")
    )))
  }

  it should "fail with an invalid target" in {
    val dirSrc = "@:path(/theme/theme.css)"
    val input = s"""aa $dirSrc bb"""
    val msg = "One or more errors processing directive 'path': unresolved internal reference: ../theme/theme.css"
    parseTemplateWithConfig(input, "") shouldBe Right(RootElement(TemplateRoot(
      TemplateString("aa "),
      TemplateElement(InvalidSpan(msg, source(dirSrc, input)).copy(fallback = Text(dirSrc))),
      TemplateString(" bb")
    )))
  }
  
  "The attribute directive" should "produce a string value for a valid config value" in {
    val input = """<a @:attribute(src, foo.bar)/>"""
    parseTemplateWithConfig(input, "foo.bar = ../image.jpg") shouldBe Right(RootElement(TemplateRoot(
      TemplateString("<a "),
      TemplateString("""src="../image.jpg""""),
      TemplateString("/>")
    )))
  }

  it should "produce an empty string for a missing config value" in {
    val input = """<a @:attribute(src, foo.baz)/>"""
    parseTemplateWithConfig(input, "foo.bar = ../image.jpg") shouldBe Right(RootElement(TemplateRoot(
      TemplateString("<a "),
      TemplateString(""),
      TemplateString("/>")
    )))
  }

  it should "fail with an invalid config value" in {
    val dirSrc = "@:attribute(src, foo.bar)"
    val input = s"""<a $dirSrc/>"""
    val msg = "One or more errors processing directive 'attribute': value with key 'foo.bar' is a structured value (Array, Object, AST) which is not supported by this directive"
    parseTemplateWithConfig(input, "foo.bar = [1,2,3]") shouldBe Right(RootElement(TemplateRoot(
      TemplateString("<a "),
      TemplateElement(InvalidSpan(msg, source(dirSrc, input)).copy(fallback = Text(dirSrc))),
      TemplateString("/>")
    )))
  }


  "The style directive" should "parse a body with a single block" in {
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
    parse(input).map(_.content) shouldBe Right(RootElement(
      p("aa"), 
      Paragraph(List(Text("11\n22")), Styles("foo")), 
      p("bb")
    ))
  }

  it should "support assigning multiple styles" in {
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
    parse(input).map(_.content) shouldBe Right(RootElement(
      p("aa"), 
      Paragraph(List(Text("11\n22")), Styles("foo", "bar", "baz")), 
      p("bb")
    ))
  }

  it should "parse a body with two blocks" in {
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
    parse(input).map(_.content) shouldBe Right(RootElement(
      p("aa"), 
      BlockSequence(List(p("11\n22"),p("33")), Styles("foo")),
      p("bb")
    ))
  }

  it should "parse a single nested span" in {
    val input = """aa @:style(foo) 11 @:@ bb"""
    parse(input).map(_.content) shouldBe Right(RootElement(p(
      Text("aa "), 
      Text(" 11 ", Styles("foo")), 
      Text(" bb")
    )))
  }

  it should "parse two nested spans" in {
    val input = """aa @:style(foo) 11 *22* 33 @:@ bb"""
    parse(input).map(_.content) shouldBe Right(RootElement(
      p(
        Text("aa "),
        SpanSequence(
          Text(" 11 "),
          Emphasized("22"),
          Text(" 33 ")
        ).withStyles("foo"),
        Text(" bb")
      )
    ))
  }
  
  "The icon directive" should "produce an icon" in {
    val icon = IconStyle("open")
    val config = ConfigBuilder.empty.withValue(IconRegistry("foo"->icon)).build
    val input = """aa @:icon(foo) bb"""
    parseTemplateWithConfig(input, config) shouldBe Right(RootElement(TemplateRoot(
      TemplateString("aa "),
      TemplateElement(icon),
      TemplateString(" bb")
    )))
  }

  it should "fail when the specified icon does not exist" in {
    val dirSrc = "@:icon(foo)"
    val input = s"aa $dirSrc bb"
    val msg = "Unresolved icon reference with key 'foo'"
    parseAndRewriteTemplate(input) shouldBe Right(RootElement(TemplateRoot(
      TemplateString("aa "),
      TemplateElement(InvalidSpan(msg, source(dirSrc, input)).copy(fallback = Text(dirSrc))),
      TemplateString(" bb")
    )))
  }
    
  "The callout directive" should "parse a body with a single block" in {
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
    parse(input).map(_.content) shouldBe Right(RootElement(
      p("aa"),
      BlockSequence("11\n22").withStyles("callout", "info"),
      p("bb")
    ))
  }

  it should "parse a body with two blocks" in {
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
    parse(input).map(_.content) shouldBe Right(RootElement(
      p("aa"),
      BlockSequence(List(p("11\n22"),p("33")), Styles("callout", "info")),
      p("bb")
    ))
  }

  "The format directive" should "parse a body with a single paragraph" in {
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
    parse(input).map(_.content) shouldBe Right(RootElement(
      p("aa"),
      TargetFormat(NonEmptySet.one("foo"), p("11\n22")), 
      p("bb")
    ))
  }

  it should "parse a body with two paragraphs" in {
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
    parse(input).map(_.content) shouldBe Right(RootElement(
      p("aa"),
      TargetFormat(NonEmptySet.one("foo"), BlockSequence(
        p("11\n22"),
        p("33")
      )), 
      p("bb")
    ))
  }

  it should "parse a directive with multiple formats" in {
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
    parse(input).map(_.content) shouldBe Right(RootElement(
      p("aa"),
      TargetFormat(NonEmptySet.of("foo", "bar", "baz"), p("11\n22")),
      p("bb")
    ))
  }
  
}
