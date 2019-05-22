/*
 * Copyright 2013-2016 the original author or authors.
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

package laika.rst

import laika.api.Transformer
import laika.ast._
import laika.format.{HTML, ReStructuredText}
import laika.render.HTMLFormatter
import laika.transform.helper.FileTransformerUtil
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Codec

/**
 * @author Jens Halm
 */
class ReStructuredTextToHTMLSpec extends FlatSpec 
                                 with Matchers
                                 with FileTransformerUtil {
  
  implicit val codec: Codec = Codec.UTF8
    
  /** Uses JTidy to remove cosmetic differences between Laika and reStructuredText output,
   *  plus a few additional, manual cleaning operations for purely cosmetic differences
   *  not covered by JTidy.
   */
  def tidyAndAdjust (html: String): String = {
    val prepared = html
      .replace("\r\n", "\n")
      .replace("\r", "\n")
      .replaceAll("([^\n])</", "$1\n</") // rst often inserts a new line before a closing tag
      .replace("$Revision: 7629 $","7629").replace("$Date: 2013-03-11 21:01:03 +0000 (Mon, 11 Mar 2013) $","2013-03-11") // RCS field substitution not supported
      .replace("""class="field-list"""", """class="docinfo"""").replace("""class="field-name"""","""class="docinfo-name"""").replace("""class="field-body"""","""class="docinfo-content"""") // docinfo field lists deferred to 0.4
    
    tidy(prepared)
      .replace("<col></colgroup>", "<col>\n</colgroup>") // fix for JTidy oddity
      .replace(">\n\n<",">\n<") // rst often adds blank lines between tags
      .replace("§§§§","&mdash;") // JTidy converts it back to utf-8 char otherwise
      .trim
  }
  

  def transformAndCompare (name: String): Unit = {
    val path = classPathResourcePath("/rstSpec") + "/" + name
    val input = readFile(path + ".rst")

    def quotedBlockContent (content: Seq[Block], attr: Seq[Span]) =
      if (attr.isEmpty) content
      else content :+ Paragraph(RawContent(List("html"), "§§§§") +: attr, Styles("attribution"))

    def renderBlocks (fmt: HTMLFormatter, tagName: String, options: Options, content: Seq[Block], attrs: (String, String)*): String = content match {
      case Seq(ss: SpanSequence)     => fmt.element(tagName, options, Seq(ss), attrs: _*)
      case Seq(Paragraph(spans,opt)) => fmt.element(tagName, options, Seq(SpanSequence(spans,opt)), attrs: _*)
      case other                     => fmt.indentedElement(tagName, options, other, attrs: _*)
    }
    
    // TODO - some of these rules might get promoted to the real renderer (e.g. the one for InternalLink)
    val actual = Transformer.from(ReStructuredText).to(HTML).rendering { 
      case (fmt, Emphasized(content,opt)) if opt.styles.contains("title-reference") => fmt.element("cite", NoOpt, content) 
      case (fmt, ExternalLink(content, url, title, opt))  => fmt.element("a", opt + Styles("reference","external"), content, fmt.optAttributes("href" -> Some(url), "title" -> title):_*)
      case (fmt, InternalLink(content, url, title, opt))  => fmt.element("a", opt + Styles("reference","internal"), content, fmt.optAttributes("href" -> Some("#"+url), "title"->title):_*)
      case (fmt, LiteralBlock(content,opt))     => fmt.withoutIndentation(_.textElement("pre", opt + Styles("literal-block"), content))
      case (fmt, Literal(content,opt))          => fmt.withoutIndentation(_.textElement("tt", opt + Styles("docutils","literal"), content))
      case (fmt, FootnoteLink(id,label,opt))    => fmt.textElement("a", opt + Styles("footnote-reference"), s"[$label]", "href"-> ("#"+id)) 
      case (fmt, Section(header, content, opt)) => fmt.indentedElement("div", opt+Id(header.options.id.getOrElse(""))+(if(header.level == 1) Styles("document") else Styles("section")), header +: content)
      case (fmt, Header(level, (it: InternalLinkTarget) :: rest, opt)) => fmt.childPerLine(Seq(it, Header(level, rest, opt))) // move target out of the header content
      case (fmt, Header(level, content, opt))   => fmt.element("h" + (level-1), NoOpt, content) // rst special treatment of first header
      case (fmt, Title(content, opt))           => fmt.element("h1", NoOpt, content, "class" -> "title")
      case (fmt, TitledBlock(title, content, opt)) => fmt.indentedElement("div", opt, Paragraph(title,Styles("admonition-title")) +: content)
      case (fmt, QuotedBlock(content,attr,opt)) => renderBlocks(fmt, "blockquote", opt, quotedBlockContent(content,attr))
      case (fmt, InternalLinkTarget(opt))       => fmt.textElement("span", opt, "")
      case (_, i: InvalidBlock)                 => ""
    }.build.transform(input)
    
    val expected = readFile(path + "-tidy.html")
    tidyAndAdjust(actual) should be (expected)
  }
  
  
  "The adjusted transformer for reStructuredText" should "transform the reStructuredText specification to HTML equivalent to the output of the reference parser" in {
    transformAndCompare("rst-spec")
  }
  
  
}
