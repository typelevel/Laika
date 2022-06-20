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

import laika.api.{MarkupParser, Renderer}
import laika.ast.sample.ParagraphCompanionShortcuts
import laika.ast.{Element, QuotedBlock, Text}
import laika.format.{HTML, Markdown}
import laika.markdown.ast.{HTMLAttribute, HTMLBlock, HTMLScriptElement, HTMLStartTag}
import munit.FunSuite
 

class VerbatimHTMLRendererSpec extends FunSuite 
                               with ParagraphCompanionShortcuts 
                               with HTMLModelBuilder {

  val renderer: Renderer = {
    val parser = MarkupParser.of(Markdown).withRawContent
    Renderer.of(HTML).withConfig(parser.config).build
  }
   
  def run (elem: Element, expected: String)(implicit loc: munit.Location): Unit =
    assertEquals(renderer.render(elem), Right(expected))
  
  
  test("render an HTML character reference unescaped") {
    val elem = p(Text("some "), charRef("&amp;"), Text(" & text"))
    run(elem, "<p>some &amp; &amp; text</p>") 
  }
  
  test("render an HTML comment with content unescaped") {
    val elem = p(Text("some "), comment(" yes < no "), Text(" & text"))
    run(elem, "<p>some <!-- yes < no --> &amp; text</p>") 
  }

  test("render a script element with content unescaped") {
    val elem = p(Text("some "), HTMLScriptElement(Nil, " var x = 'foo'; "), Text(" & text"))
    run(elem, "<p>some <script> var x = 'foo'; </script> &amp; text</p>")
  }

  test("render a script element with attributes with content unescaped") {
    val script = HTMLScriptElement(List(
      HTMLAttribute("type", List(Text("text/javascript")),Some('"')),
      HTMLAttribute("defer", List(),None)
    ), " var x = 'foo'; ")
    val elem = p(Text("some "), script, Text(" & text"))
    run(elem, "<p>some <script type=\"text/javascript\" defer> var x = 'foo'; </script> &amp; text</p>")
  }
  
  test("render an HTML end tag unescaped") {
    val elem = p(Text("some "), endTag("orphan"), Text(" & text"))
    run(elem, "<p>some </orphan> &amp; text</p>") 
  }
  
  test("render an HTML start tag without attributes unescaped") {
    val elem = p(Text("some "), startTag("hr"), Text(" & text"))
    run(elem, "<p>some <hr> &amp; text</p>") 
  }
  
  test("render an HTML start tag with one attribute unescaped") {
    val elem = p(Text("some "), startTag("hr", ("foo",Text("bar"))), Text(" & text"))
    run(elem, """<p>some <hr foo="bar"> &amp; text</p>""") 
  }
  
  test("render an HTML start tag with two attributes unescaped") {
    val elem = p(Text("some "), startTag("hr", ("foo",Text("bar")), ("bar",Text("foo"))), Text(" & text"))
    run(elem, """<p>some <hr foo="bar" bar="foo"> &amp; text</p>""") 
  }
  
  test("render an empty HTML tag without attributes unescaped") {
    val elem = p(Text("some "), emptyTag("br"), Text(" & text"))
    run(elem, "<p>some <br/> &amp; text</p>") 
  }
  
  test("render an empty HTML tag with one attribute unescaped") {
    val elem = p(Text("some "), emptyTag("br", ("foo",Text("bar"))), Text(" & text"))
    run(elem, """<p>some <br foo="bar"/> &amp; text</p>""") 
  }
  
  test("render an HTML element without attributes unescaped") {
    val elem = p(Text("some "), element(startTag("span"), Text("inner")), Text(" & text"))
    run(elem, "<p>some <span>inner</span> &amp; text</p>") 
  }
  
  test("render an HTML element with one attribute unescaped") {
    val elem = p(Text("some "), element(startTag("span", ("foo",Text("bar"))), Text("inner")), Text(" & text"))
    run(elem, """<p>some <span foo="bar">inner</span> &amp; text</p>""") 
  }
  
  test("render two nested HTML elements unescaped") {
    val inner = element(startTag("span"), Text("inner"))
    val outer = element(startTag("span"), Text("aaa "), inner, Text(" bbb"))
    val elem = p(Text("some "), outer, Text(" & text"))
    run(elem, "<p>some <span>aaa <span>inner</span> bbb</span> &amp; text</p>") 
  }
  
  test("render a <pre> element without indentation") {
    val elem = QuotedBlock(p("1st paragraph"),p(Text("some "), element(startTag("pre"), Text("Line1\nLine2")), Text(" text")), p("3rd paragraph"))
    val html = """<blockquote>
      |  <p>1st paragraph</p>
      |  <p>some <pre>Line1
      |Line2</pre> text</p>
      |  <p>3rd paragraph</p>
      |</blockquote>""".stripMargin
    run(elem, html) 
  }
  
  test("render an HTML attribute with the value in single quotes") {
    val attr = HTMLAttribute("foo", List(Text("bar")), Some('\''))
    val tag = HTMLStartTag("x", List(attr))
    run(tag, "<x foo='bar'>") 
  } 
  
  test("render an HTML attribute with an unquoted value") {
    val attr = HTMLAttribute("foo", List(Text("bar")), None)
    val tag = HTMLStartTag("x", List(attr))
    run(tag, "<x foo=bar>") 
  } 
  
  test("render an HTML attribute without value") {
    val attr = HTMLAttribute("foo", Nil, None)
    val tag = HTMLStartTag("x", List(attr))
    run(tag, "<x foo>") 
  } 
  
  test("render an HTML block unescaped") {
    val inner = element(startTag("span"), Text("inner"))
    val outer = element(startTag("span"), Text("aaa "), inner, Text(" bbb"))
    val elem = HTMLBlock(outer)
    run(elem, "<span>aaa <span>inner</span> bbb</span>") 
  }
  
  
}
