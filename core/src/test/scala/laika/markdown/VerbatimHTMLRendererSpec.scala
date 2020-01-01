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
import laika.ast.{Element, Text}
import laika.ast.helper.ModelBuilder
import laika.format.{HTML, Markdown}
import laika.markdown.ast.{HTMLAttribute, HTMLBlock, HTMLScriptElement, HTMLStartTag}
import org.scalatest.{FlatSpec, Matchers}
 

class VerbatimHTMLRendererSpec extends FlatSpec 
                               with Matchers
                               with ModelBuilder 
                               with HTMLModelBuilder {

  val renderer: Renderer = {
    val parser = MarkupParser.of(Markdown).withRawContent
    Renderer.of(HTML).withConfig(parser.config).build
  }
    
  def render (elem: Element): String = renderer.render(elem)
   
  
  "The Verbatim HTML renderer" should "render an HTML character reference unescaped" in {
    val elem = p(txt("some "), charRef("&amp;"), txt(" & text"))
    render (elem) should be ("<p>some &amp; &amp; text</p>") 
  }
  
  it should "render an HTML comment with content unescaped" in {
    val elem = p(txt("some "), comment(" yes < no "), txt(" & text"))
    render (elem) should be ("<p>some <!-- yes < no --> &amp; text</p>") 
  }

  it should "render a script element with content unescaped" in {
    val elem = p(txt("some "), HTMLScriptElement(Nil, " var x = 'foo'; "), txt(" & text"))
    render (elem) should be ("<p>some <script> var x = 'foo'; </script> &amp; text</p>")
  }

  it should "render a script element with attributes with content unescaped" in {
    val script = HTMLScriptElement(List(
      HTMLAttribute("type", List(Text("text/javascript")),Some('"')),
      HTMLAttribute("defer", List(),None)
    ), " var x = 'foo'; ")
    val elem = p(txt("some "), script, txt(" & text"))
    render (elem) should be ("<p>some <script type=\"text/javascript\" defer> var x = 'foo'; </script> &amp; text</p>")
  }
  
  it should "render an HTML end tag unescaped" in {
    val elem = p(txt("some "), endTag("orphan"), txt(" & text"))
    render (elem) should be ("<p>some </orphan> &amp; text</p>") 
  }
  
  it should "render an HTML start tag without attributes unescaped" in {
    val elem = p(txt("some "), startTag("hr"), txt(" & text"))
    render (elem) should be ("<p>some <hr> &amp; text</p>") 
  }
  
  it should "render an HTML start tag with one attribute unescaped" in {
    val elem = p(txt("some "), startTag("hr", ("foo",txt("bar"))), txt(" & text"))
    render (elem) should be ("""<p>some <hr foo="bar"> &amp; text</p>""") 
  }
  
  it should "render an HTML start tag with two attributes unescaped" in {
    val elem = p(txt("some "), startTag("hr", ("foo",txt("bar")), ("bar",txt("foo"))), txt(" & text"))
    render (elem) should be ("""<p>some <hr foo="bar" bar="foo"> &amp; text</p>""") 
  }
  
  it should "render an empty HTML tag without attributes unescaped" in {
    val elem = p(txt("some "), emptyTag("br"), txt(" & text"))
    render (elem) should be ("<p>some <br/> &amp; text</p>") 
  }
  
  it should "render an empty HTML tag with one attribute unescaped" in {
    val elem = p(txt("some "), emptyTag("br", ("foo",txt("bar"))), txt(" & text"))
    render (elem) should be ("""<p>some <br foo="bar"/> &amp; text</p>""") 
  }
  
  it should "render an HTML element without attributes unescaped" in {
    val elem = p(txt("some "), element(startTag("span"), txt("inner")), txt(" & text"))
    render (elem) should be ("<p>some <span>inner</span> &amp; text</p>") 
  }
  
  it should "render an HTML element with one attribute unescaped" in {
    val elem = p(txt("some "), element(startTag("span", ("foo",txt("bar"))), txt("inner")), txt(" & text"))
    render (elem) should be ("""<p>some <span foo="bar">inner</span> &amp; text</p>""") 
  }
  
  it should "render two nested HTML elements unescaped" in {
    val inner = element(startTag("span"), txt("inner"))
    val outer = element(startTag("span"), txt("aaa "), inner, txt(" bbb"))
    val elem = p(txt("some "), outer, txt(" & text"))
    render (elem) should be ("<p>some <span>aaa <span>inner</span> bbb</span> &amp; text</p>") 
  }
  
  it should "render a <pre> element without indentation" in {
    val elem = quote(p("1st paragraph"),p(txt("some "), element(startTag("pre"), txt("Line1\nLine2")), txt(" text")), p("3rd paragraph"))
    val html = """<blockquote>
      |  <p>1st paragraph</p>
      |  <p>some <pre>Line1
      |Line2</pre> text</p>
      |  <p>3rd paragraph</p>
      |</blockquote>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an HTML attribute with the value in single quotes" in {
    val attr = HTMLAttribute("foo", List(txt("bar")), Some('\''))
    val tag = HTMLStartTag("x", List(attr))
    render (tag) should be ("<x foo='bar'>") 
  } 
  
  it should "render an HTML attribute with an unquoted value" in {
    val attr = HTMLAttribute("foo", List(txt("bar")), None)
    val tag = HTMLStartTag("x", List(attr))
    render (tag) should be ("<x foo=bar>") 
  } 
  
  it should "render an HTML attribute without value" in {
    val attr = HTMLAttribute("foo", Nil, None)
    val tag = HTMLStartTag("x", List(attr))
    render (tag) should be ("<x foo>") 
  } 
  
  it should "render an HTML block unescaped" in {
    val inner = element(startTag("span"), txt("inner"))
    val outer = element(startTag("span"), txt("aaa "), inner, txt(" bbb"))
    val elem = HTMLBlock(outer)
    render (elem) should be ("<span>aaa <span>inner</span> bbb</span>") 
  }
  
  
}
