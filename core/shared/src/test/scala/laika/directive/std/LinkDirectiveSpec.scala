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

import laika.api.MarkupParser
import laika.ast.Path.Root
import laika.ast.{Document, ExternalTarget, InvalidSpan, MessageFilter, Path, RootElement, SpanLink, Text}
import laika.ast.helper.{ModelBuilder, TestSourceBuilders}
import laika.format.Markdown
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LinkDirectiveSpec extends AnyFlatSpec
  with Matchers
  with ModelBuilder
  with TestSourceBuilders {


  lazy val markupParser = MarkupParser.of(Markdown).failOnMessages(MessageFilter.None).build

  def parse (input: String, path: Path = Root / "doc"): Document = markupParser.parse(input, path).toOption.get
  

  trait ApiDirectiveSetup {
    def input (typeName: String): String = blockInput(s"aa @:api($typeName) bb")

    def blockInput (block: String): String =
      s"""{%
         |  laika.links.api = [
         |    { baseUri = "https://default.api/" },
         |    { baseUri = "https://foo.api/", packagePrefix = foo },
         |    { baseUri = "https://bar.api/", packagePrefix = foo.bar }
         |    { baseUri = "local/path/", packagePrefix = internal }
         |  ]
         |%}
         |
        |$block
      """.stripMargin
  }

  "The api directive" should "create a span link based on the default base URI" in new ApiDirectiveSetup {
    parse(input("def.bar.Baz")).content should be (RootElement(p(
      Text("aa "),
      SpanLink.external("https://default.api/def/bar/Baz.html")("Baz"),
      Text(" bb")
    )))
  }

  it should "create a span link based on the longest prefix match" in new ApiDirectiveSetup {
    parse(input("foo.bar.Baz")).content should be (RootElement(p(
      Text("aa "),
      SpanLink.external("https://bar.api/foo/bar/Baz.html")("Baz"),
      Text(" bb")
    )))
  }

  it should "create a span link based on the shorter prefix match" in new ApiDirectiveSetup {
    parse(input("foo.baz.Baz")).content should be (RootElement(p(
      Text("aa "),
      SpanLink.external("https://foo.api/foo/baz/Baz.html")("Baz"),
      Text(" bb")
    )))
  }

  it should "create a span link to a method" in new ApiDirectiveSetup {
    parse(input("foo.baz.Baz#canEqual(that:Any\\):Boolean")).content should be (RootElement(p(
      Text("aa "),
      SpanLink.external("https://foo.api/foo/baz/Baz.html#canEqual(that:Any):Boolean")("Baz.canEqual"),
      Text(" bb")
    )))
  }

  it should "create a span link for a package" in new ApiDirectiveSetup {
    parse(input("foo.bar.package")).content should be (RootElement(p(
      Text("aa "),
      SpanLink.external("https://bar.api/foo/bar/index.html")("foo.bar"),
      Text(" bb")
    )))
  }

  it should "fail for an internal link to a missing target" in new ApiDirectiveSetup {
    val directive = "@:api(internal.foo.Foo)"
    val input = blockInput(s"aa $directive bb")
    val path = Path.parse("/local/path/internal/foo/Foo.html")
    val msg = "One or more errors processing directive 'api': unresolved internal reference: local/path/internal/foo/Foo.html"
    parse(input).content should be (RootElement(p(
      Text("aa "),
      InvalidSpan(msg, source(directive, input)),
      Text(" bb")
    )))
  }

  it should "fail when there is no matching base URI defined" in new ApiDirectiveSetup {
    val directive = "@:api(foo.bar.Baz)"
    val input = s"aa $directive bb"
    val msg = "One or more errors processing directive 'api': No base URI defined for 'foo.bar.Baz' and no default URI available."
    parse(input).content should be (RootElement(p(
      Text("aa "),
      InvalidSpan(msg, source(directive, input)),
      Text(" bb")
    )))
  }

  it should "parse an api directive as the only element of a block" ignore new ApiDirectiveSetup {
    // TODO - this fails right now, might need auto-promotion of span directives without body to block directives
    val input = """aa
                  |
                  |@:api(foo.bar.Baz)
                  |
                  |bb""".stripMargin
    parse(blockInput(input)).content should be (RootElement(
      p("aa"),
      p(SpanLink.external("https://bar.api/foo/bar/Baz.html")("Baz")),
      p("bb")
    ))
  }

  it should "parse an api directive as the first element of a block" in new ApiDirectiveSetup {
    val input = """aa
                  |
                  |@:api(foo.bar.Baz) bb
                  |
                  |cc""".stripMargin
    parse(blockInput(input)).content should be (RootElement(
      p("aa"),
      p(SpanLink.external("https://bar.api/foo/bar/Baz.html")("Baz"), Text(" bb")),
      p("cc")
    ))
  }

  it should "parse an api directive as the first line of a block" ignore new ApiDirectiveSetup {
    val input = """aa
                  |
                  |@:api(foo.bar.Baz)
                  |bb
                  |
                  |cc""".stripMargin
    parse(blockInput(input)).content should be (RootElement(
      p("aa"),
      p(SpanLink.external("https://bar.api/foo/bar/Baz.html")("Baz"), Text(" bb")),
      p("cc")
    ))
  }

  trait SourceDirectiveSetup {
    def input (typeName: String) =
      s"""{%
         |  laika.links.source = [
         |    { baseUri = "https://default.source/", suffix = scala },
         |    { baseUri = "https://foo.source/", suffix = scala, packagePrefix = foo },
         |    { baseUri = "https://bar.source/", suffix = java, packagePrefix = foo.bar }
         |  ]
         |%}
         |
        |aa @:source($typeName) bb
      """.stripMargin
  }

  "The source directive" should "create a span link based on the default base URI" in new SourceDirectiveSetup {
    parse(input("def.bar.Baz")).content should be (RootElement(p(
      Text("aa "),
      SpanLink.external("https://default.source/def/bar/Baz.scala")("Baz"),
      Text(" bb")
    )))
  }

  it should "create a span link based on the longest prefix match" in new SourceDirectiveSetup {
    parse(input("foo.bar.Baz")).content should be (RootElement(p(
      Text("aa "),
      SpanLink.external("https://bar.source/foo/bar/Baz.java")("Baz"),
      Text(" bb")
    )))
  }

  it should "create a span link based on the shorter prefix match" in new SourceDirectiveSetup {
    parse(input("foo.baz.Baz")).content should be (RootElement(p(
      Text("aa "),
      SpanLink.external("https://foo.source/foo/baz/Baz.scala")("Baz"),
      Text(" bb")
    )))
  }

  it should "fail when there is no matching base URI defined" in new SourceDirectiveSetup {
    val directive = "@:api(foo.bar.Baz)"
    val input = s"aa $directive bb"
    val msg = "One or more errors processing directive 'api': No base URI defined for 'foo.bar.Baz' and no default URI available."
    parse(input).content should be (RootElement(p(
      Text("aa "),
      InvalidSpan(msg, source(directive, input)),
      Text(" bb")
    )))
  }
  
  
}
