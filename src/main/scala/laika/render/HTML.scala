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

  
  // TODO - align default tree model with this model once support for styles and ids has been unified
  case class StyledTable (head: TableHead, 
                          body: TableBody, 
                          tableStyles: Seq[String] = Nil, 
                          columnStyles: ColumnStyles = ColumnStyles(Nil),
                          id: Option[String] = None,
                          options: Options = NoOpt) extends Block 
  
  trait TableElement {
    def isEmpty: Boolean
  }                        
                          
  case class ColumnStyles (styles: Seq[ColumnStyle]) extends Element with TableElement {
    val isEmpty = styles.isEmpty
  } 
  case class ColumnStyle (styles: Seq[String]) extends Element
  
  case class TableHead (rows: Seq[Row]) extends Element with TableElement {
    val isEmpty = rows.isEmpty
  }
  case class TableBody (rows: Seq[Row]) extends Element with TableElement {
    val isEmpty = rows.isEmpty
  }
  
  def toTable (cit: Citation): StyledTable = toTable(cit.id,cit.id,cit.content)
  
  def toTable (id: String, label: String, content: Seq[Block]): StyledTable = {
    val left = Cell(BodyCell, List(SpanSequence(List(Text("["+label+"]")))))
    val right = Cell(BodyCell, content)
    val row = Row(List(left,right))
    StyledTable(TableHead(Nil), TableBody(List(row)), List("footnote"), 
        ColumnStyles(List(ColumnStyle(List("label")),ColumnStyle(Nil))), Some(id))
  }
  
  def toTable (t: Table) = {
    StyledTable(TableHead(t.head), TableBody(t.content), Nil, 
        ColumnStyles(Nil), None)
  }
  
  def toClass (styles: Seq[String]) = if (styles.isEmpty) None else Some(styles.mkString(" "))
  
  def noneIfDefault [T](actual: T, default: T) = if (actual == default) None else Some(actual.toString)
  
  def include (msg: SystemMessage) = {
    messageLevel flatMap {lev => if (lev <= msg.level) Some(lev) else None} isDefined
  }
  
  private def renderElement (out: HTMLWriter)(elem: Element): Unit = {
    
    def renderTable (table: StyledTable) = {
      val classes = toClass(table.tableStyles)
      val children = List(table.columnStyles,table.head,table.body) filterNot (_.isEmpty)
      
      out << "<table" <<@ ("class",classes) <<@ ("id",table.id) << ">" <<|> children <<| "</table>"
    }
    
    def renderBlockContainer (con: BlockContainer[_]) = {
      def blocks (blocks: Seq[Block], close: String) = blocks match {
        case ss @ SpanSequence(_,_) :: Nil => out << ss << close
        case other                         => out <<|> other <<| close
      }
      con match {
        case Document(content)              => out <<        "<div>" <<|>  content <<| "</div>"       
        case Section(header, content,_)     => out <<         header <<|   content
        case QuotedBlock(content, _,_)      => out << "<blockquote>"; blocks(content, "</blockquote>")
        case BulletListItem(content,_,_)    => out <<         "<li>"; blocks(content, "</li>") 
        case EnumListItem(content,_,_,_)    => out <<         "<li>"; blocks(content, "</li>") 
        case DefinitionListItem(term,defn,_)=> out << "<dt>" << term << "</dt>" <<| "<dd>"; blocks(defn, "</dd>")
        case LineBlock(content,_)           => out << """<div class="line-block">""" <<|> content <<| "</div>"
        
        case Footnote(id,label,content,_)   => renderTable(toTable(id,label,content))
        case c: Citation                    => renderTable(toTable(c))
        
        case Cell(HeadCell, content, colspan, rowspan) => out << 
            "<th" <<@ ("colspan",noneIfDefault(colspan,1)) <<@ ("rowspan",noneIfDefault(rowspan,1)) << ">"; blocks(content, "</th>") 
        case Cell(BodyCell, content, colspan, rowspan) => out << 
            "<td" <<@ ("colspan",noneIfDefault(colspan,1)) <<@ ("rowspan",noneIfDefault(rowspan,1)) << ">"; blocks(content, "</td>") 
        
        case BlockSequence(content,_)       => out << content
        case unknown                        => out << "<div>" <<|> unknown.content <<| "</div>"
      }
    }
    
    def renderSpanContainer (con: SpanContainer[_]) = con match {
      case Paragraph(content,opt)         => out <<          "<p>" <<    content <<  "</p>"  
      case Emphasized(content,opt)        => out <<         "<em>" <<    content <<  "</em>" 
      case Strong(content,opt)            => out <<     "<strong>" <<    content <<   "</strong>" 
      case Line(content,opt)              => out << """<div class="line">""" << content <<  "</div>"
      case Header(level, content, opt)    => out <<| "<h" << level.toString << ">" << content << "</h" << level.toString << ">"

      case ExternalLink(content, url, title, opt)  => out << "<a"   <<@ ("href",url) <<@ ("title",title) << ">" << content << "</a>"
      case InternalLink(content, url, title, opt)  => out << "<a"   <<@ ("href",url) <<@ ("title",title) << ">" << content << "</a>"
      
      case SpanSequence(content, opt)     => out << content
      case unknown                        => out << "<span>" <<|> unknown.content << "</span>"
    }
    
    def renderListContainer (con: ListContainer[_]) = con match {
      case EnumList(content,format,start,opt) => 
          out << "<ol" <<@ ("class", format.enumType.toString.toLowerCase) <<@ ("start", noneIfDefault(start,1)) << ">" <<|> content <<| "</ol>"
      case BulletList(content,_,opt)   => out << "<ul>" <<|> content <<| "</ul>"
      case DefinitionList(content,opt) => out << "<dl>" <<|> content <<| "</dl>"
      
      case unknown                     => out << "<div>" <<|> unknown.content <<| "</div>"
    }
    
    def renderTextContainer (con: TextContainer) = con match {
      case Text(content,opt)           => out                   <<&   content
      case Literal(content,opt)        => out <<       "<code>" <<<&  content << "</code>" 
      case LiteralBlock(content,opt)   => out <<  "<code><pre>" <<<&  content << "</pre></code>"
      case Comment(content,opt)        => out << "<!-- "        <<    content << " -->"
      
      case unknown                     => out <<& unknown.content
    }
    
    def renderSimpleBlock (block: Block) = block match {
      case Rule(opt)                   => out << "<hr>"
      case InternalLinkTarget(id,opt)  => out << "<a" <<@ ("id",id) << " />"
      
      case unknown                     => ()
    }
    
    def renderSimpleSpan (span: Span) = span match {
      case CitationLink(label,opt)     => out << "<a"   <<@ ("href", "#"+label) <<@ ("class","citation") << ">[" << label << "]</a>" 
      case FootnoteLink(id,label,opt)  => out << "<a"   <<@ ("href", "#"+id) <<@ ("class","footnote") << ">[" << label << "]</a>" 
      case Image(text, url, title, opt)=> out << "<img" <<@ ("src", url) <<@ ("alt",text) <<@ ("title",title) << ">"
      case LineBreak(opt)              => out << "<br>"
      
      case unknown                     => ()
    }
    
    def renderTableElement (elem: TableElement) = elem match {
      case TableHead(rows)         => out << "<thead>" <<|> rows <<| "</thead>"
      case TableBody(rows)         => out << "<tbody>" <<|> rows <<| "</tbody>"     
      case ColumnStyles(styles)    => out << "<colgroup>" <<|> styles <<| "</colgroup>"  
    }
    
    def renderUnresolvedReference (ref: Reference) = {
      out << InvalidSpan(SystemMessage(Error,"unresolved reference: " + ref), Text(ref.source)) 
    }
    
    def renderInvalidElement (elem: Invalid[_ <: Element]) = elem match {
      case InvalidBlock(msg, fallback, opt) => if (include(msg)) out << List(Paragraph(List(msg)), fallback)
                                               else out << fallback
      case e                                => if (include(e.message)) out << e.message << " " << e.fallback
                                               else out << e.fallback 
    }
    
    
    elem match {
      
      case msg @ SystemMessage(level,message,opt) => if (include(msg)) 
        out << "<span" <<@ ("class", "system-message "+level.toString.toLowerCase) << ">" << message << "</span>"
        
      case e: Reference           => renderUnresolvedReference(e)
      case e: Invalid[_]          => renderInvalidElement(e)

      case st: StyledTable        => renderTable(st)
      case t: Table               => renderTable(toTable(t))
      
      case e: BlockContainer[_]   => renderBlockContainer(e)
      case e: SpanContainer[_]    => renderSpanContainer(e)
      case e: ListContainer[_]    => renderListContainer(e)
      case e: TextContainer       => renderTextContainer(e)
      case e: Block               => renderSimpleBlock(e)
      case e: Span                => renderSimpleSpan(e)
      case e: TableElement        => renderTableElement(e)
      
      case Row(cells)             => out << "<tr>" <<|> cells <<| "</tr>"
      case ColumnStyle(styles)    => out << "<col" <<@ ("class", toClass(styles)) << " />"  

      case unknown                => ()  
    }  
  } 
}

/** The default instance of the HTML renderer.
 */
object HTML extends HTML(None)
