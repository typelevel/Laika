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

import laika.tree.Elements._
import laika.io.Output
  
/** A renderer for HTML output. May be directly passed to the `Render` or `Transform` APIs:
 * 
 *  {{{
 *  Render as HTML from document toFile "hello.html"
 *  
 *  Transform from Markdown to HTML fromFile "hello.md" toFile "hello.html"
 *  }}}
 * 
 *  @author Jens Halm
 */
class HTML private (messageLevel: Option[MessageLevel]) extends ((Output, Element => Unit) => (HTMLWriter, Element => Unit)) {
 
  /** Specifies the minimum required level for a system message
   *  to get included into the output by this renderer.
   */
  def withMessageLevel (level: MessageLevel) = new HTML(Some(level))
  
  /** The actual setup method for providing both the writer API for customized
   *  renderers as well as the actual default render function itself. The default render
   *  function always only renders a single element and then delegates to the composite
   *  renderer passed to this function as a parameter when rendering children. This way
   *  user customizations are possible on a per-element basis.
   *  
   *  @param output the output to write to
   *  @param render the composite render function to delegate to when elements need to render their children
   *  @return a tuple consisting of the writer API for customizing
   *  the renderer as well as the actual default render function itself
   */
  def apply (output: Output, render: Element => Unit) = {
    val out = new HTMLWriter(output asFunction, render)  
    (out, renderElement(out))
  }

  
  private def renderElement (out: HTMLWriter)(elem: Element): Unit = {
    
    def include (msg: SystemMessage) = {
      messageLevel flatMap {lev => if (lev <= msg.level) Some(lev) else None} isDefined
    }
    
    def noneIfDefault [T](actual: T, default: T) = if (actual == default) None else Some(actual.toString)
    
    def renderBlocks (blocks: Seq[Block], close: String) = blocks match {
      case ss @ SpanSequence(_,_) :: Nil => out << ss << close
      case other                         => out <<|> other <<| close
    }
    
    def renderTable (table: Table) = {
      val children = List(table.columns,table.head,table.body) filterNot (_.content.isEmpty)
      
      out <<@ ("table", table.options) <<|> children <<| "</table>"
    }
    
    def renderBlockContainer [T <: BlockContainer[T]](con: BlockContainer[T]) = {
  
      def toTable (id: String, label: String, content: Seq[Block]): Table = {
        val left = Cell(BodyCell, List(SpanSequence(List(Text("["+label+"]")))))
        val right = Cell(BodyCell, content)
        val row = Row(List(left,right))
        Table(TableHead(Nil), TableBody(List(row)),
            Columns.options(Styles("label"),NoOpt), Id(id) + Styles("footnote"))
      }
      
      def quotedBlockContent (content: Seq[Block], attr: Seq[Span]) = 
        if (attr.isEmpty) content
        else content :+ Paragraph(attr, Styles("attribution"))
      
      con match {
        case Document(content)                => out <<        "<div>" <<|>  content <<| "</div>"       
        case Section(header, content,_)       => out <<         header <<|   content
        case QuotedBlock(content,attr,opt)    => out <<@ ("blockquote",opt); renderBlocks(quotedBlockContent(content,attr), "</blockquote>")
        case BulletListItem(content,_,opt)    => out <<@ ("li",opt);         renderBlocks(content, "</li>") 
        case EnumListItem(content,_,_,opt)    => out <<@ ("li",opt);         renderBlocks(content, "</li>") 
        case DefinitionListItem(term,defn,_)  => out << "<dt>" << term << "</dt>" <<| "<dd>"; renderBlocks(defn, "</dd>")
        case LineBlock(content,opt)           => out <<@ ("div",opt + Styles("line-block")) <<|> content <<| "</div>"
        
        case Footnote(id,label,content,_)   => renderTable(toTable(id,label,content))
        case Citation(id,content,_)         => renderTable(toTable(id,id,content))
        
        case BlockSequence(content, NoOpt)  => out << content
        case c: Customizable                => out <<@ ("div",c.options) <<|> c.content <<| "</div>"
        case unknown                        => out << "<div>" <<|> unknown.content <<| "</div>"
      }
    }
    
    def renderSpanContainer [T <: SpanContainer[T]](con: SpanContainer[T]) = con match {
      case Paragraph(content,opt)         => out <<@ ("p",opt)      <<    content <<  "</p>"  
      case Emphasized(content,opt)        => out <<@ ("em",opt)     <<    content <<  "</em>" 
      case Strong(content,opt)            => out <<@ ("strong",opt) <<    content <<  "</strong>" 
      case Line(content,opt)              => out <<@ ("div",opt + Styles("line")) << content <<  "</div>"
      case Header(level, content, opt)    => out <<| "<h" << level.toString << ">" << content << "</h" << level.toString << ">"

      case ExternalLink(content, url, title, opt)  => out <<@ ("a", opt, "href"->url, "title"->title) << content << "</a>"
      case InternalLink(content, url, title, opt)  => out <<@ ("a", opt, "href"->url, "title"->title) << content << "</a>"
      
      case SpanSequence(content, NoOpt)   => out << content
      case c: Customizable                => out <<@ ("span",c.options) << c.content << "</span>"
      case unknown                        => out << "<span>" << unknown.content << "</span>"
    }
    
    def renderListContainer [T <: ListContainer[T]](con: ListContainer[T]) = con match {
      case EnumList(content,format,start,opt) => 
          out <<@ ("ol", opt, ("class", format.enumType.toString.toLowerCase), ("start", noneIfDefault(start,1))) <<|> content <<| "</ol>"
      case BulletList(content,_,opt)   => out <<@ ("ul",opt) <<|> content <<| "</ul>"
      case DefinitionList(content,opt) => out <<@ ("dl",opt) <<|> content <<| "</dl>"
      
      case c: Customizable             => out <<@ ("div",c.options) <<|> c.content <<| "</div>"
      case unknown                     => out << "<div>" <<|> unknown.content <<| "</div>"
    }
    
    def renderTextContainer (con: TextContainer) = con match {
      case Text(content,opt)           => out                   <<&   content
      case Literal(content,opt)        => out <<@ ("code",opt)  <<<&  content << "</code>" 
      case LiteralBlock(content,opt)   => out <<@ ("code",opt) << "<pre>" <<<&  content << "</pre></code>"
      case Comment(content,opt)        => out << "<!-- "        <<    content << " -->"
      
      case c: Customizable             => out <<@ ("span",c.options) << c.content << "</span>"
      case unknown                     => out <<& unknown.content
    }
    
    def renderSimpleBlock (block: Block) = block match {
      case Rule(opt)                   => out <<@ ("hr",opt) 
      case InternalLinkTarget(id,opt)  => out <<@ ("a",opt,"id"->id) << "</a>"
      
      case unknown                     => ()
    }
    
    def renderSimpleSpan (span: Span) = span match {
      case CitationLink(label,opt)     => out <<@ ("a",opt + Styles("citation"),"href"->("#"+label)) << "[" << label << "]</a>" 
      case FootnoteLink(id,label,opt)  => out <<@ ("a",opt + Styles("footnote"),"href"->("#"+id))    << "[" << label << "]</a>" 
      case Image(text,url,title,opt)   => out <<@ ("img",opt,"src"->url,"alt"->text,"title"->title)
      case LineBreak(opt)              => out << "<br>"
      
      case unknown                     => ()
    }
    
    def renderTableElement (elem: TableElement) = elem match {
      case TableHead(rows,opt)         => out <<@ ("thead",opt) <<|> rows <<| "</thead>"
      case TableBody(rows,opt)         => out <<@ ("tbody",opt) <<|> rows <<| "</tbody>"     
      case Columns(columns,opt)        => out <<@ ("colgroup",opt) <<|> columns <<| "</colgroup>"  
      case Column(opt)            => out <<@ ("col",opt) << "</col>"  
      case Row(cells,opt)         => out <<@ ("tr",opt) <<|> cells <<| "</tr>"
      case Cell(HeadCell, content, colspan, rowspan, opt) => out <<@ 
            ("th", opt, "colspan"->noneIfDefault(colspan,1), "rowspan"->noneIfDefault(rowspan,1)); renderBlocks(content, "</th>") 
      case Cell(BodyCell, content, colspan, rowspan, opt) => out <<@ 
            ("td", opt, "colspan"->noneIfDefault(colspan,1), "rowspan"->noneIfDefault(rowspan,1)); renderBlocks(content, "</td>") 
    }
    
    def renderUnresolvedReference (ref: Reference) = {
      out << InvalidSpan(SystemMessage(Error,"unresolved reference: " + ref), Text(ref.source)) 
    }
    
    def renderInvalidElement (elem: Invalid[_ <: Element]) = elem match {
      case InvalidBlock(msg, fallback, opt) => if (include(msg)) out << List(Paragraph(List(msg),opt), fallback)
                                               else out << fallback
      case e                                => if (include(e.message)) out << e.message << " " << e.fallback
                                               else out << e.fallback 
    }
    
    def renderSystemMessage (message: SystemMessage) = {
      if (include(message)) 
        out <<@ ("span", message.options + Styles("system-message", message.level.toString.toLowerCase)) << message.content << "</span>"
    }
    
    
    elem match {
      case e: SystemMessage       => renderSystemMessage(e)
      case e: Table               => renderTable(e)
      case e: TableElement        => renderTableElement(e)
      case e: Reference           => renderUnresolvedReference(e)
      case e: Invalid[_]          => renderInvalidElement(e)
      case e: BlockContainer[_]   => renderBlockContainer(e)
      case e: SpanContainer[_]    => renderSpanContainer(e)
      case e: ListContainer[_]    => renderListContainer(e)
      case e: TextContainer       => renderTextContainer(e)
      case e: Block               => renderSimpleBlock(e)
      case e: Span                => renderSimpleSpan(e)

      case unknown                => ()  
    }  
  } 
}

/** The default instance of the HTML renderer.
 */
object HTML extends HTML(None)
