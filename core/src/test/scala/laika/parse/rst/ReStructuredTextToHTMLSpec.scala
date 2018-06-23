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

package laika.parse.rst

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import laika.api.Transform
import laika.parse.markdown.html.VerbatimHTML
import laika.render.HTML
import laika.transform.helper.FileTransformerUtil
import laika.tree.Elements._
import scala.io.Codec
import laika.io.Input

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
      .replace("<col></colgroup>", "<col>\n</colgroup>") // TODO - why is it rendered like this? JTidy oddity
      .replace(">\n\n<",">\n<") // rst often adds blank lines between tags
      .replace("§§§§","&mdash;") // JTidy converts it back to utf-8 char otherwise
      .trim
  }
  

  def transformAndCompare (name: String): Unit = {
    val path = classPathResource("/rstSpec") + "/" + name
    val actual = Transform from ReStructuredText to HTML rendering { out => 
      
      def quotedBlockContent (content: Seq[Block], attr: Seq[Span]) = 
        if (attr.isEmpty) content
        else content :+ Paragraph(RawContent(List("html"), "§§§§") +: attr, Styles("attribution"))

      def renderBlocks (blocks: Seq[Block], close: String) = blocks match {
        case ss @ SpanSequence(_,_) :: Nil => out << ss << close
        case Paragraph(content,opt) :: Nil => out << SpanSequence(content,opt) << close
        case other                         => out <<|> other <<| close
      }
      
      // TODO - some of these rules might get promoted to the real renderer (e.g. the one for InternalLink)
      {
      case Emphasized(content,opt) if opt.styles.contains("title-reference") => out << "<cite>" <<  content << "</cite>" 
      case ExternalLink(content, url, title, opt)  => out <<@ ("a", opt + Styles("reference","external"), "href"->url,       "title"->title) << content << "</a>"
      case InternalLink(content, url, title, opt)  => out <<@ ("a", opt + Styles("reference","internal"), "href"->("#"+url), "title"->title) << content << "</a>"
      case LiteralBlock(content,opt)     => out <<@ ("pre",opt + Styles("literal-block"))  <<<&  content << "</pre>"
      case Literal(content,opt)          => out <<@ ("tt",opt+Styles("docutils","literal"))  <<<& content << "</tt>" 
      case FootnoteLink(id,label,opt)    => out <<@ ("a",opt + Styles("footnote-reference"),"href"->("#"+id))    << "[" << label << "]</a>" 
      case Section(header, content, opt) => out <<@ ("div", opt+Id(header.options.id.getOrElse(""))+(if(header.level == 1) Styles("document") else Styles("section"))) <<|> (header +: content) <<| "</div>"
      case Header(level, (it: InternalLinkTarget) :: rest, opt) => out <<| it <<| Header(level, rest, opt) // move target out of the header content
      case Header(level, content, opt)   => out <<| "<h" << (level-1).toString << ">" << content << "</h" << (level-1).toString << ">" // rst special treatment of first header
      case Title(content, opt)           => out <<| "<h1 class=\"title\">" << content << "</h1>"
      case TitledBlock(title, content, opt) => out <<@ ("div",opt) <<|> (Paragraph(title,Styles("admonition-title")) +: content) <<| "</div>"
      case QuotedBlock(content,attr,opt) => out <<@ ("blockquote",opt); renderBlocks(quotedBlockContent(content,attr), "</blockquote>")
      case InternalLinkTarget(opt)       => out <<@ ("span",opt) << "</span>"
      case i: InvalidBlock               => ()
    }} fromFile (path + ".rst") toString
    
    val expected = readFile(path + "-tidy.html")
    tidyAndAdjust(actual) should be (expected)
  }
  
  
  "The adjusted transformer for reStructuredText" should "transform the reStructuredText specification to HTML equivalent to the output of the reference parser" in {
    transformAndCompare("rst-spec")
  }
  
  
}
