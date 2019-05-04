/*
 * Copyright 2012-2019 the original author or authors.
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

import laika.api.Render
import laika.ast.Path.Root
import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.config.OperationConfig
import laika.format.{HTML, HTML2}
import laika.io.StringOutput
import org.scalatest.{FlatSpec, Matchers}

class HTMLRenderer2Spec extends FlatSpec 
                       with Matchers
                       with ModelBuilder {
 
  
  def render (elem: Element): String = Render.Op2(HTML2, OperationConfig.default, elem, StringOutput(new StringBuilder, Root)).execute 
  
  def render (elem: Element, messageLevel: MessageLevel): String = 
    Render as HTML withMessageLevel messageLevel from elem toString
    
  def renderUnformatted (elem: Element): String = (Render as HTML).unformatted from elem toString
  
  
  "The HTML renderer" should "render a paragraph with plain text" in {
    val elem = p("some text")
    render (elem) should be ("<p>some text</p>") 
  }
  
  it should "render a document with two paragraphs with plain text" in {
    val elem = root( p("aaa"), p("bbb"))
    val html = """<p>aaa</p>
      |<p>bbb</p>""".stripMargin
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
  
  it should "render a block sequence with a style" in {
    val elem = root(BlockSequence(List(p("aaa"), p("bbb")), Styles("foo")))
    val html = """<div class="foo">
      |  <p>aaa</p>
      |  <p>bbb</p>
      |</div>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a block sequence without a style" in {
    val elem = root(p("aaa"), BlockSequence(List(p("bbb"), p("ccc"))))
    val html = """<p>aaa</p>
      |<p>bbb</p>
      |<p>ccc</p>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a block sequence with a single element" in {
    val elem = root(p("aaa"), BlockSequence(List(p("bbb"))), p("ccc"))
    val html = """<p>aaa</p>
      |<p>bbb</p>
      |<p>ccc</p>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a blockquote with simple flow content" in {
    val elem = quote(p("aaa"))
    val html = "<blockquote>aaa</blockquote>"
    render (elem) should be (html) 
  }
  
  it should "render a blockquote with an attribution" in {
    val elem = quote("aaa","bbb")
    val html = """<blockquote>
      |  <p>aaa</p>
      |  <p class="attribution">bbb</p>
      |</blockquote>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a bullet list with simple flow content" in {
    val elem = bulletList() + "aaa" + "bbb" toList
    val html = """<ul>
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ul>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with simple flow content" in {
    val elem = enumList() + "aaa" + "bbb"
    val html = """<ol class="arabic">
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with lower roman enumeration style" in {
    val elem = enumList(EnumFormat(EnumType.LowerRoman, "", ".")) + "aaa" + "bbb"
    val html = """<ol class="lowerroman">
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with upper roman enumeration style" in {
    val elem = enumList(EnumFormat(EnumType.UpperRoman, "", ".")) + "aaa" + "bbb"
    val html = """<ol class="upperroman">
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with lower alpha enumeration style" in {
    val elem = enumList(EnumFormat(EnumType.LowerAlpha, "", ".")) + "aaa" + "bbb"
    val html = """<ol class="loweralpha">
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with upper alpha enumeration style" in {
    val elem = enumList(EnumFormat(EnumType.UpperAlpha, "", ".")) + "aaa" + "bbb"
    val html = """<ol class="upperalpha">
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with the start value if it is not 1" in {
    val elem = enumList(EnumFormat(EnumType.Arabic, "", "."), 7) + "aaa" + "bbb"
    val html = """<ol class="arabic" start="7">
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  private def fp (content: String) = ForcedParagraph(List(Text(content)))
  
  it should "render a bullet list with forced paragraphs as list items" in {
    val elem = bulletList() + fp("aaa") + fp("bbb") toList
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
  
  it should "render an enumerated list with forced paragraphs as list items" in {
    val elem = enumList() + fp("aaa") + fp("bbb")
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
    val elem = defList + ("term 1", p("1"), p("1")) + ("term 2", p("2"), p("2"))
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
    val elem = defList + ("term 1", p("1")) + ("term 2", p("2"))
    val html = """<dl>
      |  <dt>term 1</dt>
      |  <dd>1</dd>
      |  <dt>term 2</dt>
      |  <dd>2</dd>
      |</dl>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a footnote" in {
    val elem = Footnote("label", List(p("a"),p("b")), Id("id"))
    val html = """<table id="id" class="footnote">
      |  <colgroup>
      |    <col class="label"></col>
      |    <col></col>
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
    val elem = Citation("ref", List(p("a"),p("b")), Id("ref"))
    val html = """<table id="ref" class="citation">
      |  <colgroup>
      |    <col class="label"></col>
      |    <col></col>
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
    val elem = Table(TableHead(List(row(cell("a"), cell("b")))),
                     TableBody(List(row(cell("c"), cell("d")))))
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
  
  it should "render a table with a caption" in {
    val caption = Caption(List(Text("caption")))
    val elem = table(row(cell("a"),cell("b")),row(cell("c"),cell("d"))).copy(caption = caption)
    val html = """<table>
      |  <caption>caption</caption>
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
  
  it should "render a titled block" in {
    val elem = TitledBlock(List(txt("some "), em("em"), txt(" text")), List(p("aaa"), Rule(), p("bbb")))
    val html = """<div>
      |  <p class="title">some <em>em</em> text</p>
      |  <p>aaa</p>
      |  <hr>
      |  <p>bbb</p>
      |</div>""".stripMargin
    render(elem) should be (html)
  }
  
  it should "render a figure" in {
    val elem = Figure(Image("alt",URI("image.jpg")), List(txt("some "), em("caption"), txt(" text")), List(p("aaa"), Rule(), p("bbb")))
    val html = """<div class="figure">
      |  <img src="image.jpg" alt="alt">
      |  <p class="caption">some <em>caption</em> text</p>
      |  <div class="legend">
      |    <p>aaa</p>
      |    <hr>
      |    <p>bbb</p>
      |  </div>
      |</div>""".stripMargin
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
    val elem = root( p("aaa"), Rule(), p("bbb"))
    val html = """<p>aaa</p>
      |<hr>
      |<p>bbb</p>""".stripMargin
    render (elem) should be (html) 
  } 
  
  it should "render a document with two nested sections" in {
    val nested = Section(h(2, txt("Title 2")), List(p("Line 1"), p("Line 2")))
    val rootElem = root(Section(h(1, txt("Title 1")), List(p("Line 1"), p("Line 2"))), nested)
    val html = """
      |<h1>Title 1</h1>
      |<p>Line 1</p>
      |<p>Line 2</p>
      |
      |<h2>Title 2</h2>
      |<p>Line 1</p>
      |<p>Line 2</p>""".stripMargin
    render (rootElem) should be (html) 
  }
  
  it should "render a title containing emphasized text" in {
    val elem = Title(Seq(txt("some "), em("em"), txt(" text")))
    render (elem) should be ("<h1>some <em>em</em> text</h1>") 
  }
  
  it should "render a title containing a section number" in {
    val elem = Title(Seq(SectionNumber(Seq(1,2,3)), txt(" Title")))
    render (elem) should be ("""<h1><span class="sectionNumber">1.2.3 </span> Title</h1>""") 
  }
  
  it should "render a paragraph containing emphasized text" in {
    val elem = p(txt("some "), em("em"), txt(" text"))
    render (elem) should be ("<p>some <em>em</em> text</p>") 
  }
  
  it should "render a paragraph containing strong text" in {
    val elem = p(txt("some "), str("strong"), txt(" text")) 
    render (elem) should be ("<p>some <strong>strong</strong> text</p>") 
  }

  it should "render a paragraph containing a deleted span" in {
    val elem = p(txt("some "), Deleted(Seq(Text("deleted"))), txt(" text"))
    render (elem) should be ("<p>some <del>deleted</del> text</p>")
  }

  it should "render a paragraph containing an inserted span" in {
    val elem = p(txt("some "), Inserted(Seq(Text("inserted"))), txt(" text"))
    render (elem) should be ("<p>some <ins>inserted</ins> text</p>")
  }
  
  it should "render a paragraph containing a literal span" in {
    val elem = p(txt("some "), lit("code"), txt(" span"))
    render (elem) should be ("<p>some <code>code</code> span</p>") 
  }
  
  it should "render a paragraph containing a code span" in {
    val elem = p(txt("some "), Code("banana-script", List(Text("code"))), txt(" span"))
    render (elem) should be ("<p>some <code class=\"code banana-script\">code</code> span</p>") 
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
  
  it should "render a paragraph containing an internal link with emphasized text" in {
    val elem = p(txt("some "), InternalLink(List(txt("link"),em("text")),"foo"), txt(" span"))
    render (elem) should be ("""<p>some <a href="#foo">link<em>text</em></a> span</p>""") 
  }
  
  it should "render a paragraph containing a cross link with a fragment part" in {
    val elem = p(txt("some "), CrossLink(List(txt("link"),em("text")),"foo", PathInfo(Path("/bar"),Path("../bar.md"))), txt(" span"))
    render (elem) should be ("""<p>some <a href="../bar.html#foo">link<em>text</em></a> span</p>""") 
  }
  
  it should "render a paragraph containing a cross link without a fragment part" in {
    val elem = p(txt("some "), CrossLink(List(txt("link"),em("text")),"", PathInfo(Path("/bar"),Path("../bar.md"))), txt(" span"))
    render (elem) should be ("""<p>some <a href="../bar.html">link<em>text</em></a> span</p>""") 
  }
  
  it should "render a paragraph containing a cross link with a filename without suffix" in {
    val elem = p(txt("some "), CrossLink(List(txt("link"),em("text")),"", PathInfo(Path("/bar"),Path("../bar"))), txt(" span"))
    render (elem) should be ("""<p>some <a href="../bar">link<em>text</em></a> span</p>""") 
  }
  
  it should "render a paragraph containing a citation link" in {
    val elem = p(txt("some "), CitationLink("ref","label"), txt(" span"))
    render (elem) should be ("""<p>some <a class="citation" href="#ref">[label]</a> span</p>""") 
  }
  
  it should "render a paragraph containing a footnote link" in {
    val elem = p(txt("some "), FootnoteLink("id","label"), txt(" span"))
    render (elem) should be ("""<p>some <a class="footnote" href="#id">[label]</a> span</p>""") 
  }
  
  it should "render a paragraph containing an image without title" in {
    val elem = p(txt("some "), img("img", "foo.jpg"), txt(" span"))
    render (elem) should be ("""<p>some <img src="foo.jpg" alt="img"> span</p>""") 
  }
  
  it should "render a paragraph containing an image with title" in {
    val elem = p(txt("some "), img("img", "foo.jpg", title = Some("title")), txt(" span"))
    render (elem) should be ("""<p>some <img src="foo.jpg" alt="img" title="title"> span</p>""") 
  }

  it should "render a paragraph containing an image with width and height in pixels" in {
    val image = img("img", "foo.jpg", width = Some(Size(200,"px")), height = Some(Size(120,"px")))
    val elem = p(txt("some "), image, txt(" span"))
    render (elem) should be ("""<p>some <img src="foo.jpg" alt="img" width="200" height="120"> span</p>""")
  }

  it should "render a paragraph containing an image with width and height in a unit other than pixels" in {
    val image = img("img", "foo.jpg", width = Some(Size(12.4,"in")), height = Some(Size(6.8,"in")))
    val elem = p(txt("some "), image, txt(" span"))
    render (elem) should be ("""<p>some <img src="foo.jpg" alt="img" style="width:12.4in;height:6.8in"> span</p>""")
  }

  it should "render a paragraph containing an image with just width in a unit other than pixels" in {
    val image = img("img", "foo.jpg", width = Some(Size(12.4,"in")))
    val elem = p(txt("some "), image, txt(" span"))
    render (elem) should be ("""<p>some <img src="foo.jpg" alt="img" style="width:12.4in"> span</p>""")
  }
  
  it should "render a paragraph containing an unresolved link reference" in {
    val elem = p(txt("some "), linkRef(txt("link")).id("id").source("[link] [id]"), txt(" span"))
    render (elem) should be ("""<p>some [link] [id] span</p>""")
  }
  
  it should "render a paragraph containing an unresolved image reference" in {
    val elem = p(txt("some "), imgRef("img","id","![img] [id]"), txt(" span"))
    render (elem) should be ("""<p>some ![img] [id] span</p>""") 
  }
  
  it should "render a paragraph containing an internal link target" in {
    val elem = p(txt("some "), InternalLinkTarget(Id("target")), txt(" span"))
    render (elem) should be ("""<p>some <a id="target"></a> span</p>""") 
  }
  
  it should "render a template root containing string elements" in {
    val elem = tRoot(tt("aa"),tt("bb"),tt("cc"))
    render (elem) should be ("aabbcc")
  }
  
  it should "render a template span sequence containing string elements" in {
    val elem = TemplateSpanSequence(List(tt("aa"),tt("bb"),tt("cc")))
    render (elem) should be ("aabbcc")
  }
  
  it should "render a template string without creating html entities" in {
    val elem = tRoot(tt("aa & bb"))
    render (elem) should be ("aa & bb")
  }
  
  it should "render a template root containing a TemplateElement" in {
    val elem = tRoot(tt("aa"),tElem(BlockSequence(List(p("aaa"), p("bbb")),Styles("foo"))),tt("cc"))
    val html = """aa<div class="foo">
      |  <p>aaa</p>
      |  <p>bbb</p>
      |</div>cc""".stripMargin
    render (elem) should be (html)
  }

  import MessageLevel._
  
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
  
  it should "render a literal block" in {
    val code = """line 1
      |
      |    <line 2
      |
      |line 3""".stripMargin
    val elem = litBlock(code)
    render (elem) should be ("<pre><code>" + code.replaceAllLiterally("<", "&lt;") + "</code></pre>") 
  }
  
  it should "render a parsed literal block" in {
    val code = """line 1
      |
      |    #<line 2
      |
      |line 3""".stripMargin.split("#")
    val elem = ParsedLiteralBlock(List(txt(code(0)), em("em"), txt(code(1))))
    val html = "<pre><code>" + code(0) + "<em>em</em>" + code(1).replaceAllLiterally("<", "&lt;") + "</code></pre>"
    render (elem) should be (html) 
  }
  
  it should "render a code block" in {
    val code = """line 1
      |
      |    <line 2
      |
      |line 3""".stripMargin
    val elem = CodeBlock("banana-script", List(Text(code)))
    render (elem) should be ("<pre class=\"code banana-script\"><code>" + code.replaceAllLiterally("<", "&lt;") + "</code></pre>") 
  }
  
  it should "render a literal block inside a blockquote without indentation" in {
    val code = """line 1
      |
      |    line 2
      |
      |line 3""".stripMargin
    val html = """<blockquote>
      |  <pre><code>%s</code></pre>
      |</blockquote>""".stripMargin.format(code)
    val elem = quote(litBlock(code))
    render (elem) should be (html) 
  }
  
  it should "render a parsed literal block inside a blockquote without indentation" in {
    val code = """line 1
      |
      |    line 2
      |
      |line 3""".stripMargin
    val html = """<blockquote>
      |  <pre><code>:<em>%s</em>:</code></pre>
      |</blockquote>""".stripMargin.format(code)
    val elem = quote(ParsedLiteralBlock(List(txt(":"),em(code),txt(":"))))
    render (elem) should be (html) 
  }
  
  it should "render a code block inside a blockquote without indentation" in {
    val code = """line 1
      |
      |    line 2
      |
      |line 3""".stripMargin
    val html = """<blockquote>
      |  <pre class="code banana-script"><code>:<em>%s</em>:</code></pre>
      |</blockquote>""".stripMargin.format(code)
    val elem = quote(CodeBlock("banana-script", List(txt(":"),em(code),txt(":"))))
    render (elem) should be (html) 
  }
  
  it should "render a table cell unformatted" in {
    val elem = cell(p("a"),p("b"))
    val html = """<td>
      |<p>a</p>
      |<p>b</p>
      |</td>""".stripMargin
    renderUnformatted(elem) should be (html)
  } 
  
  it should "render raw content unchanged if the html format is specified" in {
    val raw = "<span>foo</span>"
    val elem = RawContent(List("html", "spooky"), raw)
    render (elem) should be (raw) 
  }
  
  it should "ignore raw content if the html format is not specified" in {
    val raw = "<span>foo</span>"
    val elem = RawContent(List("dodgy", "spooky"), raw)
    render (elem) should be ("") 
  }
  
  it should "render an embedded root with correct indentation" in {
    val elem = root(tRoot(
      tt("<div>\n  "),
      EmbeddedRoot(List(p("aaa"),p("bbb")), 2),
      tt("\n</div>")
    ))
    val html = """<div>
      |  <p>aaa</p>
      |  <p>bbb</p>
      |</div>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an embedded root without indentation" in {
    val elem = root(tRoot(
      tt("<div>\n"),
      EmbeddedRoot(List(p("aaa"),p("bbb")), 0),
      tt("\n</div>")
    ))
    val html = """<div>
      |<p>aaa</p>
      |<p>bbb</p>
      |</div>""".stripMargin
    render (elem) should be (html) 
  }
  
  
}
