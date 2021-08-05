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

package laika.markdown

import laika.api.builder.OperationConfig
import laika.ast._
import laika.ast.sample.TestSourceBuilders
import laika.format.Markdown
import laika.parse.Parser
import laika.parse.helper.MigrationFlatSpec
import laika.parse.markup.RootParserProvider.RootParserWrapper
import org.scalatest.Assertion
     
class InlineParsersSpec extends MigrationFlatSpec with TestSourceBuilders {


  val rootParser = new RootParserWrapper(Markdown, OperationConfig(Markdown.extensions).markupExtensions)

  val defaultParser: Parser[List[Span]] = rootParser.standaloneSpanParser

  def run (input: String, spans: Span*): Assertion =
    assertEquals(defaultParser.parse(input).toEither, Right(spans.toList))

  def runEnclosed (input: String, middleSpan: Span): Assertion =
    run(input, Text("some "), middleSpan, Text(" here"))
  
  
  "The text parser" should "parse content without any markup as plain text" in {
    run("some text", Text("some text"))
  }
  

  
  "The em parser" should "parse content enclosed in * at the beginning of a phrase" in {
    run("*some* text", Emphasized("some"), Text(" text"))
  }
  
  it should "parse content enclosed in * at the end of a phrase" in {
    run("some *text*", Text("some "), Emphasized("text"))
  }
  
  it should "parse content enclosed in * in the middle of a phrase" in {
    runEnclosed("some *text* here", Emphasized("text"))
  }
  
  it should "parse content enclosed in * when it spans the entire phrase" in {
    run("*text*", Emphasized("text"))
  }
  
  it should "ignore an * character when it is enclosed in spaces" in {
    run("some * text * here", Text("some * text * here"))
  }

  it should "ignore an * start markup when it is at the end of the line" in {
    run("some *\ntext* here", Text("some *\ntext* here"))
  }
  
  it should "ignore an * character when it is not matched by a second *" in {
    run("some *text here", Text("some *text here"))
  }
    
  it should "treat an '_' the same as an '*'" in {
    runEnclosed("some _text_ here", Emphasized("text"))
  }
  
  
  
  "The strong parser" should "parse content enclosed in ** at the beginning of a phrase" in {
    run("**some** text", Strong("some"), Text(" text"))
  }
  
  it should "parse content enclosed in ** at the end of a phrase" in {
    run("some **text**", Text("some "), Strong("text"))
  }
  
  it should "parse content enclosed in ** in the middle of a phrase" in {
    runEnclosed("some **text** here", Strong("text"))
  }

  it should "parse content enclosed in ** with a nested em span" in {
    runEnclosed("some ***text*** here", Strong(Emphasized("text")))
  }
  
  it should "parse content enclosed in ** when it spans the entire phrase" in {
    run("**text**", Strong("text"))
  }
  
  it should "ignore a ** sequence when it is enclosed in spaces" in {
    run("some ** text ** here", Text("some ** text ** here"))
  }
  
  it should "ignore a ** sequence when it is not matched by a second **" in {
    run("some **text here", Text("some **text here"))
  }
  
  it should "treat an '_' the same as an '*'" in {
    runEnclosed("some __text__ here", Strong("text"))
  }
  
  
  
  "The code parser" should "parse content enclosed in ` at the beginning of a phrase" in {
    run("`some` text", Literal("some"), Text(" text"))
  }
  
  it should "parse content enclosed in ` at the end of a phrase" in {
    run("some `text`", Text("some "), Literal("text"))
  }
  
  it should "parse content enclosed in ` in the middle of a phrase" in {
    runEnclosed("some `text` here", Literal("text"))
  }
  
  it should "parse content enclosed in ` when it spans the entire phrase" in {
    run("`text`", Literal("text"))
  }
  
  it should "treat a ` character as markup even when it is enclosed in spaces" in {
    runEnclosed("some ` text ` here", Literal("text"))
  }
  
  it should "ignore a ` character when it is not matched by a second `" in {
    run("some `text here", Text("some `text here"))
  }
  
  it should "not treat a single ` as markup when the code span is enclosed in double ``" in {
    runEnclosed("some ``text`text`` here", Literal("text`text"))
  }

  it should "support three ` characters as delimiter" in {
    runEnclosed("some ``` text`text ``` here", Literal("text`text"))
  }

  it should "support five ` characters as delimiter" in {
    runEnclosed("some ````` text```text ````` here", Literal("text```text"))
  }
  
  
  
  "The span parser" should "allow nesting of spans, in this case a code span inside emphasized text" in {
    runEnclosed("some *nested `code` span* here", Emphasized(Text("nested "), Literal("code"), Text(" span")))
  }
  
  it should "ignore the attempt to close an outer span inside an inner span" in {
    run("some *nested `code* span` here", Text("some *nested "), Literal("code* span"), Text(" here"))
  }
  
  
  
  "A backslash " should "cause a following escapable character not to be treated as markup" in {
    run("""some \*text* here""", Text("some *text* here"))
  }
  
  it should "be treated as a literal character when the following character is not escapable" in {
    run("""some \?text here""", Text("""some \?text here"""))
  }
  
  
  
  "The link parser" should "parse an inline link without title" in {
    runEnclosed("some [link](http://foo) here", SpanLink.external("http://foo")("link"))
  }
  
  it should "parse an inline link with an optional title enclosed in double quotes" in {
    runEnclosed("""some [link](http://foo "a title") here""", 
      SpanLink.external("http://foo")("link").copy(title = Some("a title")) 
    )
  }
  
  it should "parse an inline link with an optional title enclosed in single quotes" in {
    runEnclosed("""some [link](http://foo 'a title') here""",
      SpanLink.external("http://foo")("link").copy(title = Some("a title"))
    )
  }
  
  it should "ignore an inline link with a malformed title" in {
    val input = """some [link](http://foo 'a title) here"""
    val ref = LinkIdReference("link", source("[link]", input))("link")
    run(input, Text("some "), ref, Text("(http://foo 'a title) here"))
  }
  
  it should "parse markup inside the text of an inline link" in {
    runEnclosed("some [link *text*](http://foo) here",
      SpanLink.external("http://foo")(Text("link "), Emphasized("text"))
    )
  }

  it should "properly parse escape sequences in the text of an inline link" in {
    runEnclosed("some [link \\_text\\_](http://foo) here", 
      SpanLink.external("http://foo")("link _text_") 
    )
  }
  
  
  
  "The image parser" should "parse an inline image without title" in {
    runEnclosed(
      "some ![link](http://foo.jpg) here", 
      Image(ExternalTarget("http://foo.jpg"), alt = Some("link")) 
    )
  }
  
  it should "parse an inline image with an optional title enclosed in double quotes" in {
    runEnclosed(
      """some ![link](http://foo.jpg "a title") here""", 
      Image(ExternalTarget("http://foo.jpg"), alt = Some("link"), title = Some("a title"))
    )
  }
  
  it should "parse an inline image with an optional title enclosed in single quotes" in {
    runEnclosed(
      """some ![link](http://foo.jpg 'a title') here""", 
      Image(ExternalTarget("http://foo.jpg"), alt = Some("link"), title = Some("a title"))
    )
  }
  
  it should "ignore an inline image with a malformed title" in {
    val input = """some ![link](http://foo.jpg 'a title) here"""
    run(input, Text("some "), ImageIdReference("link","link", source("![link]", input)), Text("(http://foo.jpg 'a title) here"))
  }
  
  it should "not parse markup inside the text of an inline link" in {
    runEnclosed(
      "some ![link *text*](http://foo.jpg) here", 
      Image(ExternalTarget("http://foo.jpg"), alt = Some("link *text*"))
    )
  }
  
  
  
  "The link reference parser" should "parse a link reference with an explicit id" in {
    val input = "some [link][id] here"
    val ref = LinkIdReference("id", source("[link][id]", input))("link")
    runEnclosed(input, ref)
  }
  
  it should "parse a link reference with an empty id" in {
    val input = "some [link][] here"
    val ref = LinkIdReference("link", source("[link][]", input))("link")
    runEnclosed(input, ref)
  }
  
  it should "parse a link reference with an explicit id separated by a space" in {
    val input = "some [link] [id] here"
    val ref = LinkIdReference("id", source("[link] [id]", input))("link")
    runEnclosed(input, ref)
  }
  
  it should "parse a link reference with an empty id separated by a space" in {
    val input = "some [link] [] here"
    val ref = LinkIdReference("link", source("[link] []", input))("link")
    runEnclosed(input, ref)
  }
  
  it should "parse a link reference with an implicit id" in {
    val input = "some [link] here"
    val ref = LinkIdReference("link", source("[link]", input))("link")
    runEnclosed(input, ref)
  }
  
  
  
  "The image reference parser" should "parse an image reference with an explicit id" in {
    val input = "some ![image][id] here"
    runEnclosed(input, ImageIdReference("image","id", source("![image][id]", input)))
  }
  
  it should "parse an image reference with an empty id" in {
    val input = "some ![image][] here"
    runEnclosed(input, ImageIdReference("image","image", source("![image][]", input)))
  }
  
  it should "parse an image reference with an implicit id" in {
    val input = "some ![image] here"
    runEnclosed(input, ImageIdReference("image","image", source("![image]", input)))
  }
  
  
  
  "The simple link parser" should "parse a link enclosed in angle brackets and set the url as the link text" in {
    runEnclosed("some <http://foo> here", SpanLink.external("http://foo")("http://foo"))
  }
  
  
}
