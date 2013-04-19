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
import laika.tree.Elements._
import laika.tree.helper.ModelBuilder

class HTMLRendererSpec extends FlatSpec 
                       with ShouldMatchers
                       with ModelBuilder {
 
  
  def render (elem: Element) = Render as HTML from elem toString 
  
  def render (elem: Element, messageLevel: MessageLevel) = 
    Render as (HTML withMessageLevel messageLevel) from elem toString
  
  
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
  
  it should "render a blockquote with simple flow content" in {
    val elem = quote(ss("aaa"))
    val html = "<blockquote>aaa</blockquote>"
    render (elem) should be (html) 
  }
  
  it should "render a bullet list with simple flow content" in {
    val elem = bl( bli( ss("aaa")), bli( ss("bbb")))
    val html = """<ul>
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ul>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with simple flow content" in {
    val elem = el( eli(1, ss("aaa")), eli(2, ss("bbb")))
    val html = """<ol class="arabic">
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with lower roman enumeration style" in {
    val elem = el(EnumFormat(LowerRoman, "", "."), 1, eli(1, ss("aaa")), eli(2, ss("bbb")))
    val html = """<ol class="lowerroman">
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with upper roman enumeration style" in {
    val elem = el(EnumFormat(UpperRoman, "", "."), 1, eli(1, ss("aaa")), eli(2, ss("bbb")))
    val html = """<ol class="upperroman">
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with lower alpha enumeration style" in {
    val elem = el(EnumFormat(LowerAlpha, "", "."), 1, eli(1, ss("aaa")), eli(2, ss("bbb")))
    val html = """<ol class="loweralpha">
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with upper alpha enumeration style" in {
    val elem = el(EnumFormat(UpperAlpha, "", "."), 1, eli(1, ss("aaa")), eli(2, ss("bbb")))
    val html = """<ol class="upperalpha">
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with the start value if it is not 1" in {
    val elem = el(EnumFormat(Arabic, "", "."), 7, eli(7, ss("aaa")), eli(8, ss("bbb")))
    val html = """<ol class="arabic" start="7">
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a bullet list with paragraphs as list items" in {
    val elem = bl( bli( p("aaa")), bli( p("bbb")))
    val html = """<ul>
      |  <li>
      |    <p>aaa</p>
      |  </li>
      |  <li>
      |    <p>bbb</p>
      |  </li>
      |</ul>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with paragraphs as list items" in {
    val elem = el( eli(1, p("aaa")), eli(2, p("bbb")))
    val html = """<ol class="arabic">
      |  <li>
      |    <p>aaa</p>
      |  </li>
      |  <li>
      |    <p>bbb</p>
      |  </li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a definition list with paragraphs" in {
    val elem = dl(dli("term 1", p("1"), p("1")), dli("term 2", p("2"), p("2")))
    val html = """<dl>
      |  <dt>term 1</dt>
      |  <dd>
      |    <p>1</p>
      |    <p>1</p>
      |  </dd>
      |  <dt>term 2</dt>
      |  <dd>
      |    <p>2</p>
      |    <p>2</p>
      |  </dd>
      |</dl>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a definition list with simple flow content" in {
    val elem = dl(dli("term 1", ss("1")), dli("term 2", ss("2")))
    val html = """<dl>
      |  <dt>term 1</dt>
      |  <dd>1</dd>
      |  <dt>term 2</dt>
      |  <dd>2</dd>
      |</dl>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a footnote" in {
    val elem = Footnote(ResolvedFootnoteLabel("id","label"), List(p("a"),p("b")))
    val html = """<table class="footnote" id="id">
      |  <colgroup>
      |    <col class="label" />
      |    <col />
      |  </colgroup>
      |  <tbody>
      |    <tr>
      |      <td>[label]</td>
      |      <td>
      |        <p>a</p>
      |        <p>b</p>
      |      </td>
      |    </tr>
      |  </tbody>
      |</table>""".stripMargin
    render(elem) should be (html)
  }
  
  it should "render a citation" in {
    val elem = Citation("ref", List(p("a"),p("b")))
    val html = """<table class="footnote" id="ref">
      |  <colgroup>
      |    <col class="label" />
      |    <col />
      |  </colgroup>
      |  <tbody>
      |    <tr>
      |      <td>[ref]</td>
      |      <td>
      |        <p>a</p>
      |        <p>b</p>
      |      </td>
      |    </tr>
      |  </tbody>
      |</table>""".stripMargin
    render(elem) should be (html)
  }
  
  it should "render a table without header cells" in {
    val elem = table(row(cell("a"),cell("b")),row(cell("c"),cell("d")))
    val html = """<table>
      |  <tbody>
      |    <tr>
      |      <td>a</td>
      |      <td>b</td>
      |    </tr>
      |    <tr>
      |      <td>c</td>
      |      <td>d</td>
      |    </tr>
      |  </tbody>
      |</table>""".stripMargin
    render(elem) should be (html)
  }
  
  it should "render a table with header cells" in {
    val elem = Table(List(row(cell("a"),cell("b"))),List(row(cell("c"),cell("d"))))
    val html = """<table>
      |  <thead>
      |    <tr>
      |      <td>a</td>
      |      <td>b</td>
      |    </tr>
      |  </thead>
      |  <tbody>
      |    <tr>
      |      <td>c</td>
      |      <td>d</td>
      |    </tr>
      |  </tbody>
      |</table>""".stripMargin
    render(elem) should be (html)
  }
  
  it should "render a cell using colspan and rowspan attributes" in {
    val elem = cell("a",3,2)
    val html = """<td colspan="3" rowspan="2">a</td>"""
    render(elem) should be (html)
  } 
  
  it should "render a cell with two paragraphs" in {
    val elem = cell(p("a"),p("b"))
    val html = """<td>
      |  <p>a</p>
      |  <p>b</p>
      |</td>""".stripMargin
    render(elem) should be (html)
  } 
  
  it should "render nested line blocks" in {
    val elem = lb(lb(line("1"),line("2")), line("3"))
    val html = """<div class="line-block">
      |  <div class="line-block">
      |    <div class="line">1</div>
      |    <div class="line">2</div>
      |  </div>
      |  <div class="line">3</div>
      |</div>""".stripMargin
    render(elem) should be (html)
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
    val elem = p(txt("some "), lit("code"), txt(" span"))
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
  
  it should "render a paragraph containing a citation reference" in {
    val elem = p(txt("some "), CitationReference("label"), txt(" span"))
    render (elem) should be ("""<p>some <a href="#label" class="citation">[label]</a> span</p>""") 
  }
  
  it should "render a paragraph containing a footnote reference" in {
    val elem = p(txt("some "), FootnoteReference(ResolvedFootnoteLabel("id","label")), txt(" span"))
    render (elem) should be ("""<p>some <a href="#id" class="footnote">[label]</a> span</p>""") 
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
  
  it should "render a paragraph containing an internal link target" in {
    val elem = p(txt("some "), InternalLinkTarget("target"), txt(" span"))
    render (elem) should be ("""<p>some <a id="target" /> span</p>""") 
  }
  
  it should "render a system message" in {
    val html = """<span class="system-message warning">some message</span>"""
    render (SystemMessage(Warning, "some message"), Warning) should be (html)
  }
  
  it should "render a comment" in {
    render (Comment("foo")) should be ("<!-- foo -->")
  }
  
  it should "render an invalid block without the system message in default mode" in {
    val elem = InvalidBlock(SystemMessage(Warning, "some message"), p("fallback"))
    val html = "<p>fallback</p>"
    render (elem) should be (html)
  }
  
  it should "render an invalid block without the system message if the configured message level is higher" in {
    val elem = InvalidBlock(SystemMessage(Warning, "some message"), p("fallback"))
    val html = "<p>fallback</p>"
    render (elem, Error) should be (html)
  }
  
  it should "render an invalid block with the system message if the configured message level is lower or equal" in {
    val elem = InvalidBlock(SystemMessage(Warning, "some message"), p("fallback"))
    val html = """<p><span class="system-message warning">some message</span></p><p>fallback</p>"""
    render (elem, Info) should be (html)
  }
  
  it should "render an invalid span without the system message in default mode" in {
    val elem = InvalidSpan(SystemMessage(Warning, "some message"), txt("fallback"))
    render (elem) should be ("fallback")
  }
  
  it should "render an invalid span without the system message if the configured message level is higher" in {
    val elem = InvalidSpan(SystemMessage(Warning, "some message"), txt("fallback"))
    render (elem, Error) should be ("fallback")
  }
  
  it should "render an invalid span with the system message if the configured message level is lower or equal" in {
    val elem = InvalidSpan(SystemMessage(Warning, "some message"), txt("fallback"))
    val html = """<span class="system-message warning">some message</span> fallback"""
    render (elem, Info) should be (html)
  }
  
  it should "render a code block" in {
    val code = """line 1
      |
      |    <line 2
      |
      |line 3""".stripMargin
    val elem = litBlock(code)
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
    val elem = quote(litBlock(code))
    render (elem) should be (html) 
  }
  
  
}