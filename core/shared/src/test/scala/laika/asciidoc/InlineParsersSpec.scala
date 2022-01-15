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
package laika.asciidoc


import laika.api.builder.OperationConfig
import laika.ast._
import laika.parse.Parser
import laika.parse.markup.RootParserProvider.RootParserWrapper
import munit.FunSuite
import laika.ast.sample.TestSourceBuilders
import laika.format.AsciiDoc
     
class InlineParsersSpec extends FunSuite with TestSourceBuilders {


  val rootParser = new RootParserWrapper(AsciiDoc, OperationConfig(/*Markdown.extensions*/).markupExtensions)

  val defaultParser: Parser[List[Span]] = rootParser.standaloneSpanParser

  def run (input: String, spans: Span*): Unit =
    assertEquals(defaultParser.parse(input).toEither, Right(spans.toList))

  def runEnclosed (input: String, middleSpan: Span): Unit =
    run(input, Text("some "), middleSpan, Text(" here"))
  
  
  test("content without any markup") {
    run("some text", Text("some text"))
  }
  
  test("em - enclosed in _ at the beginning of a phrase") {
    run("_some_ text", Emphasized("some"), Text(" text"))
  }

  test("em - enclosed in __ at the beginning of a phrase") {
    run("__some__ text", Emphasized("some"), Text(" text"))
  }
  test("em - enclosed in __ at the beginning of a phrase without trailing space") {
    run("__some__text", Emphasized("some"), Text("text"))
  }
  
  test("em - enclosed in _ at the end of a phrase") {
    run("some _text_", Text("some "), Emphasized("text"))
  }
  
  test("em - enclosed in _ in the middle of a phrase") {
    runEnclosed("some _text_ here", Emphasized("text"))
  }
  
  test("em - enclosed in * when it spans the entire phrase") {
    run("_text_", Emphasized("text"))
  }

  test("em -enclosed in __ without outside space") {
    run("some__text__here",Text("some"),Emphasized("text"),Text("here"))
  }
  
  test("em - ignore an _ character when it is enclosed in spaces") {
    run("some _ text _ here", Text("some _ text _ here"))
  }

  test("em - ignore when leading _ is mising") {
    run("some text_ here",Text("some text_ here"))
  }
  
  test("em - ignore when trailing _ is mising") {
    run("some _text here",Text("some _text here"))
  }

  
  test("em - ignore an _ character when it is not matched by a second _") {
    run("some _text here", Text("some _text here"))
  }
      
  test("strong - enclosed in ** at the beginning of a phrase") {
    run("**some** text", Strong("some"), Text(" text"))
  }
  
  test("strong - enclosed in ** at the beginning of a phrase without trailing space") {
    run("**some**text", Strong("some"), Text("text"))
  }

  test("strong - enclosed in * at the beginning of a phrase") {
    run("*some* text", Strong("some"), Text(" text"))
  }
  
  test("strong - enclosed in ** at the end of a phrase") {
    run("some **text**", Text("some "), Strong("text"))
  }
  test("strong - enclosed in ** at the end of a phrase without leading space") {
    run("some**text**", Text("some"), Strong("text"))
  }
  
  test("strong - enclosed in ** in the middle of a phrase") {
    runEnclosed("some **text** here", Strong("text"))
  }

  test("strong - enclosed in ** with a nested em span") {
    runEnclosed("some _**text**_ here", Emphasized(Strong("text")))
  }
  
  test("strong - enclosed in ** when it spans the entire phrase") {
    run("**text**", Strong("text"))
  }

  test("strong - enclosed in ** with space inside") {
    run("some ** text ** here",Text("some "),Strong(" text "),Text(" here"))
  }
  
  test("strong - ignore a * sequence when it is enclosed in spaces") {
    run("some * text * here", Text("some * text * here"))
  }
  
  test("strong - ignore a ** sequence when it is not matched by a second **") {
    run("some **text here", Text("some **text here"))
  }
    

  
  test("literal - enclosed in ` at the beginning of a phrase") {
    run("`some` text", Literal("some"), Text(" text"))
  }
  
  test("literal - enclosed in ` at the end of a phrase") {
    run("some `text`", Text("some "), Literal("text"))
  }
  
  test("literal - enclosed in ` in the middle of a phrase") {
    runEnclosed("some `text` here", Literal("text"))
  }
  
  test("literal - enclosed in ` when it spans the entire phrase") {
    run("`text`", Literal("text"))
  }
  
  test("literal - treat a ` character as markup even when it is enclosed in spaces") {
    runEnclosed("some ` text ` here", Literal("text"))
  }
  
  test("literal - ignore a ` character when it is not matched by a second `") {
    run("some `text here", Text("some `text here"))
  }
  
  test("literal - do not treat a single ` as markup when the code span is enclosed in double ``") {
    runEnclosed("some ``text`text`` here", Literal("text`text"))
  }

  test("literal - support three ` characters as delimiter") {
    runEnclosed("some ``` text`text ``` here", Literal("text`text"))
  }

  test("literal - support five ` characters as delimiter") {
    runEnclosed("some ````` text```text ````` here", Literal("text```text"))
  }
  
  test("nested spans - code span inside emphasized text") {
    runEnclosed("some _nested `code` span_ here", Emphasized(Text("nested "), Literal("code"), Text(" span")))
  }
  
  test("nested spans - ignore the attempt to close an outer span inside an inner span") {
    run("some *nested `code* span` here", Text("some *nested "), Literal("code* span"), Text(" here"))
  }
  
  
  
  test("escapes - escapable character not treated as markup") {
    run("""some \*text* here""", Text("some *text* here"))
  }
  
  
  test("escapes - backslash treated as a literal character when the following character is not escapable") {
    run("""some \?text here""", Text("""some \?text here"""))
  }
  
  
  test("links -detect inline link without title") {
    runEnclosed("some http://foo here",SpanLink.external("http://foo")("http://foo"))
  }
  
  test("links -detect inline link with title") {
    runEnclosed("some http://foo[link] here",SpanLink.external("http://foo")("link"))
  }
  

  test("links - detect email link as external link") {
    runEnclosed("some nobody@nowhere.com here", SpanLink.external("mailto:nobody@nowhere.com")("nobody@nowhere.com"))
  }
  
  test("linebreak -  + invoke hard line break") {
    run("""|some +      
           |text+
           |here""".stripMargin, Text("some"),LineBreak(),Text("text+\nhere"))
  }
}
