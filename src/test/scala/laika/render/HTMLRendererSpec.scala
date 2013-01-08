/*
 * Copyright 2013 the original author or authors.
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

package laika.render

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

import laika.api.Render
import laika.tree.Elements.Element
import laika.tree.Elements.Rule
import laika.tree.Elements.Section
import laika.tree.helper.ModelBuilder

class HTMLRendererSpec extends FlatSpec 
												with ShouldMatchers
												with ModelBuilder {
 
  
  def render (elem: Element) = Render as HTML from elem toString 
  
  
  "The HTML renderer" should "render a paragraph with plain text" in {
    val elem = p("some text")
    render (elem) should be ("<p>some text</p>") 
  }
  
  it should "render a document with two paragraphs with plain text" in {
    val elem = doc( p("aaa"), p("bbb"))
    val html = """<div>
      |  <p>aaa</p>
      |  <p>bbb</p>
      |</div>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a blockquote with two paragraphs with plain text" in {
    val elem = quote( p("aaa"), p("bbb"))
    val html = """<blockquote>
      |  <p>aaa</p>
      |  <p>bbb</p>
      |</blockquote>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an unordered list with simple flow content" in {
    val elem = ul( li( fc("aaa")), li( fc("bbb")))
    val html = """<ul>
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ul>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an ordered list with simple flow content" in {
    val elem = ol( li( fc("aaa")), li( fc("bbb")))
    val html = """<ol>
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an unordered list with paragraphs as list items" in {
    val elem = ul( li( p("aaa")), li( p("bbb")))
    val html = """<ul>
      |  <li><p>aaa</p></li>
      |  <li><p>bbb</p></li>
      |</ul>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an ordered list with paragraphs as list items" in {
    val elem = ol( li( p("aaa")), li( p("bbb")))
    val html = """<ol>
      |  <li><p>aaa</p></li>
      |  <li><p>bbb</p></li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a document with two paragraphs separated by a horizontal rule" in {
    val elem = doc( p("aaa"), Rule, p("bbb"))
    val html = """<div>
      |  <p>aaa</p>
      |  <hr>
      |  <p>bbb</p>
      |</div>""".stripMargin
    render (elem) should be (html) 
  } 
  
  it should "render a document with two nested sections" in {
    val nested = Section(h(2, txt("Title 2")), List(p("Line 1"), p("Line 2")))
    val root = doc(Section(h(1, txt("Title 1")), List(p("Line 1"), p("Line 2"))), nested)
    val html = """<div>
      |  
      |  <h1>Title 1</h1>
      |  <p>Line 1</p>
	    |  <p>Line 2</p>
	    |  
      |  <h2>Title 2</h2>
	    |  <p>Line 1</p>
	    |  <p>Line 2</p>
      |</div>""".stripMargin
    render (root) should be (html) 
  }
  
  it should "render a paragraph containing emphasized text" in {
    val elem = p(txt("some "), em("em"), txt(" text"))
    render (elem) should be ("<p>some <em>em</em> text</p>") 
  }
  
  it should "render a paragraph containing strong text" in {
    val elem = p(txt("some "), str("strong"), txt(" text")) 
    render (elem) should be ("<p>some <strong>strong</strong> text</p>") 
  }
  
  it should "render a paragraph containing a code span" in {
    val elem = p(txt("some "), code("code"), txt(" span"))
    render (elem) should be ("<p>some <code>code</code> span</p>") 
  }
  
  it should "render a paragraph containing a link without title" in {
    val elem = p(txt("some "), link(txt("link")).url("/foo"), txt(" span"))
    render (elem) should be ("""<p>some <a href="/foo">link</a> span</p>""") 
  }
  
  it should "render a paragraph containing a link with title" in {
    val elem = p(txt("some "), link(txt("link")).url("/foo").title("title"), txt(" span"))
    render (elem) should be ("""<p>some <a href="/foo" title="title">link</a> span</p>""") 
  }
  
  it should "render a paragraph containing a link with emphasized text" in {
    val elem = p(txt("some "), link(txt("link"),em("text")).url("/foo"), txt(" span"))
    render (elem) should be ("""<p>some <a href="/foo">link<em>text</em></a> span</p>""") 
  }
  
  it should "render a paragraph containing an image without title" in {
    val elem = p(txt("some "), img("img", "foo.jpg"), txt(" span"))
    render (elem) should be ("""<p>some <img src="foo.jpg" alt="img"> span</p>""") 
  }
  
  it should "render a paragraph containing an image with title" in {
    val elem = p(txt("some "), img("img", "foo.jpg", Some("title")), txt(" span"))
    render (elem) should be ("""<p>some <img src="foo.jpg" alt="img" title="title"> span</p>""") 
  }
  
  it should "render a paragraph containing an unresolved link reference" in {
    val elem = p(txt("some "), linkRef(txt("link")).id("id").postFix("] [id]"), txt(" span"))
    render (elem) should be ("""<p>some [link] [id] span</p>""") 
  }
  
  it should "render a paragraph containing an unresolved image reference" in {
    val elem = p(txt("some "), imgRef("img","id","] [id]"), txt(" span"))
    render (elem) should be ("""<p>some ![img] [id] span</p>""") 
  }
  
  it should "render a code block" in {
    val code = """line 1
      |
      |    <line 2
      |
      |line 3""".stripMargin
    val elem = codeBlock(code)
    render (elem) should be ("<code><pre>" + code.replaceAllLiterally("<", "&lt;") + "</pre></code>") 
  }
  
  it should "render a code block inside a blockquote" in {
    val code = """line 1
      |
      |    line 2
      |
      |line 3""".stripMargin
    val html = """<blockquote>
      |  <code><pre>%s</pre></code>
      |</blockquote>""".stripMargin.format(code)
    val elem = quote(codeBlock(code))
    render (elem) should be (html) 
  }
  
  
}