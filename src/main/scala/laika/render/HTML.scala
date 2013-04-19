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
                          id: Option[String] = None) extends Block 
  
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
  
  def toTable (cit: Citation): StyledTable = toTable(cit.label,cit.label,cit.content)
  
  def toTable (id: String, label: String, content: Seq[Block]): StyledTable = {
    val left = Cell(BodyCell, List(FlowContent(List(Text("["+label+"]")))))
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
    
    def blocks (blocks: Seq[Block], close: String) = blocks match {
      case fl @ FlowContent(_) :: Nil => out << fl << close
      case other                      => out <<|> other <<| close
    }
    
    elem match {
      case Document(content)          => out <<        "<div>" <<|>  content <<| "</div>"       
      case QuotedBlock(content, _)    => out << "<blockquote>"; blocks(content, "</blockquote>")
      case BulletList(content,_)      => out <<         "<ul>" <<|>  content <<| "</ul>"
      case EnumList(content,format,start) => 
        out << "<ol" <<@ ("class", format.enumType.toString.toLowerCase) <<@ ("start", noneIfDefault(start,1)) << ">" <<|> content <<| "</ol>"
      case CodeBlock(content)         => out <<  "<code><pre>" <<<&  content <<  "</pre></code>"
      case Section(header, content)   => out <<         header <<|   content
      case Paragraph(content)         => out <<          "<p>" <<    content <<  "</p>"  
      case BulletListItem(content,_)  => out <<         "<li>"; blocks(content, "</li>") 
      case EnumListItem(content,_,_)  => out <<         "<li>"; blocks(content, "</li>") 
      case Emphasized(content)        => out <<         "<em>" <<    content <<  "</em>" 
      case Strong(content)            => out <<     "<strong>" <<    content <<   "</strong>" 
      case CodeSpan(content)          => out <<       "<code>" <<<&  content <<   "</code>" 
      case Text(content)              => out                   <<&   content
      case FlowContent(content)       => out                   <<    content
      case BlockSequence(content)     => out                   <<    content
      case Rule                       => out << "<hr>"
      case LineBreak                  => out << "<br>"
      case Header(level, content)     => out <<| "<h" << level.toString << ">" << content << "</h" << level.toString << ">"
      case Link(content, url, title)  => out << "<a"   <<@ ("href",url) <<@ ("title",title) << ">" << content << "</a>"
      case CitationReference(label)   => out << "<a"   <<@ ("href", "#"+label) <<@ ("class","citation") << ">[" << label << "]</a>" 
      case FootnoteReference(ResolvedFootnoteLabel(id,label)) => 
        out << "<a"   <<@ ("href", "#"+id) <<@ ("class","footnote") << ">[" << label << "]</a>" 
      case Image(text, url, title)    => out << "<img" <<@ ("src", url) <<@ ("alt",text) <<@ ("title",title) << ">"

      case DefinitionList(items)        => out << "<dl>" <<|> items <<| "</dl>"
      case DefinitionListItem(term,defn)=> out << "<dt>" << term << "</dt>" <<| "<dd>"; blocks(defn, "</dd>")
      case Footnote(ResolvedFootnoteLabel(id,label),content)  => renderTable(toTable(id,label,content))
      case c: Citation                                        => renderTable(toTable(c))
      case t: Table                                           => renderTable(toTable(t))
      case st: StyledTable                                    => renderTable(st)
      
      case TableHead(rows)            => out << "<thead>" <<|> rows <<| "</thead>"
      case TableBody(rows)            => out << "<tbody>" <<|> rows <<| "</tbody>"
      case Row(cells)                 => out << "<tr>" <<|> cells <<| "</tr>"
      case Cell(HeadCell, content, colspan, rowspan) => out << 
        "<th" <<@ ("colspan",noneIfDefault(colspan,1)) <<@ ("rowspan",noneIfDefault(rowspan,1)) << ">"; blocks(content, "</th>") 
      case Cell(BodyCell, content, colspan, rowspan) => out << 
        "<td" <<@ ("colspan",noneIfDefault(colspan,1)) <<@ ("rowspan",noneIfDefault(rowspan,1)) << ">"; blocks(content, "</td>") 
      case ColumnStyles(styles)       => out << "<colgroup>" <<|> styles <<| "</colgroup>"  
      case ColumnStyle(styles)        => out << "<col" <<@ ("class", toClass(styles)) << " />"  
      
      case LineBlock(lines)           => out << """<div class="line-block">""" <<|> lines <<| "</div>"
      case Line(content)              => out << """<div class="line">"""       << content <<  "</div>"
      
      case InternalLinkTarget(id)     => out << "<a" <<@ ("id",id) << " />"
      case msg @ SystemMessage(level,message) => if (include(msg)) 
        out << "<span" <<@ ("class", "system-message "+level.toString.toLowerCase) << ">" << message << "</span>"
      case Comment(text)              => out << "<!-- " << text << " -->"
      
      case InvalidBlock(msg, fallback) => if (include(msg)) out << List(Paragraph(List(msg)), fallback)
                                          else out << fallback
      case InvalidSpan(msg, fallback)  => if (include(msg)) out << msg << " " << fallback
                                          else out << fallback 

      case LinkReference(content, id, inputPrefix, inputPostfix)  => out << inputPrefix << content << inputPostfix 
      case ImageReference(text, id, inputPrefix, inputPostfix)    => out << inputPrefix << text    << inputPostfix

      case sc: SpanContainer[_]       => out << "<span>" <<  sc.content << "</span>"
      case bc: BlockContainer[_]      => out << "<div>" <<|> bc.content << "</div>"
      case unknown                    => ()  
    }  
  } 
}

/** The default instance of the HTML renderer.
 */
object HTML extends HTML(None)
