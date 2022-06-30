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

import laika.api.{MarkupParser, RenderPhaseRewrite}
import laika.ast.Path.Root
import laika.ast._
import laika.ast.sample.{ParagraphCompanionShortcuts, TestSourceBuilders}
import laika.format.{HTML, Markdown}
import laika.parse.markup.DocumentParser.TransformationError
import munit.FunSuite

class LinkDirectiveSpec extends FunSuite with ParagraphCompanionShortcuts with TestSourceBuilders with RenderPhaseRewrite {


  private lazy val markupParser = MarkupParser.of(Markdown).failOnMessages(MessageFilter.None).build

  def parse (input: String, path: Path = Root / "doc"): Either[TransformationError, Document] = 
    markupParser.parse(input, path).flatMap(rewrite(markupParser, HTML))
  

  object Api {
    
    def runType (typeName: String, expected: SpanLink): Unit = run(typeName, expected)
    def runType (typeName: String, invalid: InvalidSpan, withoutConfig: Boolean = false): Unit = 
      run(typeName, invalid, withoutConfig)
    
    private def run (typeName: String, expected: Span, withoutConfig: Boolean = false): Unit = assertEquals(
      parse(if (withoutConfig) lineInput(directiveInput(typeName)) else input(typeName)).map(_.content),
      Right(RootElement(p(Text("aa "), expected, Text(" bb"))))
    )
    
    def runTypeBlock (block: String, expected: Span*): Unit = {
      val input = s"""aa
                    |
                    |$block
                    |
                    |bb""".stripMargin
      assertEquals(
        parse(configInput(input)).map(_.content),
        Right(RootElement(p("aa"), p(expected:_*), p("bb")))
      )
    }
    
    def directiveInput (typeName: String): String = s"@:api($typeName)"
    
    def lineInput (directive: String): String = s"aa $directive bb"

    def input (typeName: String): String = configInput(lineInput(directiveInput(typeName)))

    def configInput (block: String): String =
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

  test("api directive - span link based on the default base URI") {
    Api.runType("def.bar.Baz", SpanLink.external("https://default.api/def/bar/Baz.html")("Baz"))
  }

  test("api directive - strip the $ postfix from the link text") {
    Api.runType("def.bar.Baz$", SpanLink.external("https://default.api/def/bar/Baz$.html")("Baz"))
  }

  test("api directive - span link based on the longest prefix match") {
    Api.runType("foo.bar.Baz", SpanLink.external("https://bar.api/foo/bar/Baz.html")("Baz"))
  }

  test("api directive - span link based on the shorter prefix match") {
    Api.runType("foo.baz.Baz", SpanLink.external("https://foo.api/foo/baz/Baz.html")("Baz"))
  }

  test("api directive - span link to a method") {
    Api.runType(
      "foo.baz.Baz#canEqual(that:Any\\):Boolean", 
      SpanLink.external("https://foo.api/foo/baz/Baz.html#canEqual(that:Any):Boolean")("Baz.canEqual")
    )
  }

  test("api directive - span link for a package") {
    Api.runType("foo.bar.package", SpanLink.external("https://bar.api/foo/bar/index.html")("foo.bar"))
  }

  test("api directive - fail for an internal link to a missing target") {
    val typeName = "internal.foo.Foo"
    val msg = "One or more errors processing directive 'api': unresolved internal reference: local/path/internal/foo/Foo.html"
    val expected = InvalidSpan(msg, source(Api.directiveInput(typeName), Api.input(typeName), defaultPath))
    Api.runType(typeName, expected)
  }

  test("api directive - fail when there is no matching base URI defined") {
    val typeName = "foo.bar.Baz"
    val directive = Api.directiveInput(typeName)
    val msg = s"One or more errors processing directive 'api': No base URI defined for '$typeName' and no default URI available."
    val expected = InvalidSpan(msg, source(directive, Api.lineInput(directive), defaultPath))
    Api.runType(typeName, expected, withoutConfig = true)
  }

  test("api directive - as the only element of a block".ignore) {
    // TODO - this fails right now, might need auto-promotion of span directives without body to block directives
    Api.runTypeBlock("@:api(foo.bar.Baz)", SpanLink.external("https://bar.api/foo/bar/Baz.html")("Baz"))
  }

  test("api directive - as the first element of a block") {
    Api.runTypeBlock("@:api(foo.bar.Baz) bb", SpanLink.external("https://bar.api/foo/bar/Baz.html")("Baz"), Text(" bb"))
  }

  test("api directive - as the first line of a block".ignore) {
    Api.runTypeBlock("@:api(foo.bar.Baz)\nbb", SpanLink.external("https://bar.api/foo/bar/Baz.html")("Baz"), Text(" bb"))
  }

  object Source {
    def configInput (line: String): String =
      s"""{%
         |  laika.links.source = [
         |    { baseUri = "https://default.source/", suffix = scala },
         |    { baseUri = "https://foo.source/", suffix = scala, packagePrefix = foo },
         |    { baseUri = "https://bar.source/", suffix = java, packagePrefix = foo.bar }
         |  ]
         |%}
         |
         |$line
      """.stripMargin

    def directiveInput (typeName: String): String = s"@:source($typeName)"

    def lineInput (directive: String): String = s"aa $directive bb"

    def input (typeName: String): String = configInput(lineInput(directiveInput(typeName)))
    
    def runType (typeName: String, expected: SpanLink): Unit = run(typeName, expected)
    def runType (typeName: String, expected: InvalidSpan, withoutConfig: Boolean = false): Unit =
      run(typeName, expected, withoutConfig)

    private def run (typeName: String, expected: Span, withoutConfig: Boolean = false): Unit = assertEquals(
      parse(if (withoutConfig) lineInput(directiveInput(typeName)) else input(typeName)).map(_.content),
      Right(RootElement(p(Text("aa "), expected, Text(" bb"))))
    )
  }

  test("source directive - span link based on the default base URI") {
    Source.runType("def.bar.Baz", SpanLink.external("https://default.source/def/bar/Baz.scala")("Baz"))
  }

  test("source directive - span link based on the longest prefix match") {
    Source.runType("foo.bar.Baz", SpanLink.external("https://bar.source/foo/bar/Baz.java")("Baz"))
  }

  test("source directive - span link based on the shorter prefix match") {
    Source.runType("foo.baz.Baz", SpanLink.external("https://foo.source/foo/baz/Baz.scala")("Baz"))
  }

  test("source directive - fail when there is no matching base URI defined") {
    val typeName = "foo.bar.Baz"
    val directive = Source.directiveInput(typeName)
    val msg = s"One or more errors processing directive 'source': No base URI defined for '$typeName' and no default URI available."
    val expected = InvalidSpan(msg, source(directive, Source.lineInput(directive), defaultPath))
    Source.runType(typeName, expected, withoutConfig = true)
  }
  
  
}
