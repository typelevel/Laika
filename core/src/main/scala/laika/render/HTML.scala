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

package laika.render

import laika.api.ext.Theme
import laika.directive.DefaultTemplateParser
import laika.tree.Documents.TemplateDocument
import laika.tree.Elements._
import laika.tree.Templates._
import laika.tree.Paths.Root
import laika.io.Input
import laika.io.Output
import laika.factory.RendererFactory
import laika.parse.css.Styles.StyleDeclarationSet

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
class HTML private (messageLevel: Option[MessageLevel], renderFormatted: Boolean) 
    extends RendererFactory[HTMLWriter] {
  
  val fileSuffix = "html"
 
  /** Specifies the minimum required level for a system message
   *  to get included into the output by this renderer.
   */
  def withMessageLevel (level: MessageLevel): HTML = new HTML(Some(level), renderFormatted)
  
  /** Renders HTML without any formatting (line breaks or indentation) around tags. 
   *  Useful when storing the output in a database for example. 
   */
  def unformatted: HTML = new HTML(messageLevel, false)
  
  /** The actual setup method for providing both the writer API for customized
   *  renderers as well as the actual default render function itself. The default render
   *  function always only renders a single element and then delegates to the composite
   *  renderer passed to this function as a parameter when rendering children. This way
   *  user customizations are possible on a per-element basis.
   *  
   *  @param output the output to write to
   *  @param root the root element the new renderer will be used for
   *  @param render the composite render function to delegate to when elements need to render their children
   *  @param styles the styles the new renderer should apply to the rendered elements
   *  @return a tuple consisting of the writer API for customizing
   *  the renderer as well as the actual default render function itself
   */
  def newRenderer (output: Output, root: Element, render: Element => Unit, styles: StyleDeclarationSet): (HTMLWriter, Element => Unit) = {
    val out = new HTMLWriter(output asFunction, render, root, formatted = renderFormatted)  
    (out, renderElement(out))
  }

  
  private def renderElement (out: HTMLWriter)(elem: Element): Unit = {
    
    def include (msg: SystemMessage): Boolean = {
      messageLevel flatMap {lev => if (lev <= msg.level) Some(lev) else None} isDefined
    }
    
    def noneIfDefault [T](actual: T, default: T): Option[String] = if (actual == default) None else Some(actual.toString)
    
    def renderBlocks (blocks: Seq[Block], close: String): HTMLWriter = blocks match {
      case ss @ SpanSequence(_,_) :: Nil => out << ss << close
      case Paragraph(content,opt) :: Nil => out << SpanSequence(content,opt) << close
      case other                         => out <<|> other <<| close
    }
    
    def renderTable (table: Table): HTMLWriter = {
      val children = List(table.caption,table.columns,table.head,table.body) filterNot (_.content.isEmpty)
      
      out <<@ ("table", table.options) <<|> children <<| "</table>"
    }
    
    object WithFallback {
      def unapply (value: Element): Option[Element] = value match {
        case f: Fallback => Some(f.fallback)
        case _ => None
      }
    }
    
    def renderBlockContainer [T <: BlockContainer[T]](con: BlockContainer[T]): Unit = {
  
      def toTable (label: String, content: Seq[Block], options: Options): Table = {
        val left = Cell(BodyCell, List(SpanSequence(List(Text(s"[$label]")))))
        val right = Cell(BodyCell, content)
        val row = Row(List(left,right))
        Table(TableHead(Nil), TableBody(List(row)), Caption(),
            Columns.options(Styles("label"),NoOpt), options)
      }
      
      def quotedBlockContent (content: Seq[Block], attr: Seq[Span]): Seq[Block] = 
        if (attr.isEmpty) content
        else content :+ Paragraph(attr, Styles("attribution"))
        
      def figureContent (img: Span, caption: Seq[Span], legend: Seq[Block]): List[Block] =
        List(SpanSequence(List(img)), Paragraph(caption, Styles("caption")), BlockSequence(legend, Styles("legend")))
      
      con match {
        case RootElement(content)             => if (content.nonEmpty) out << content.head <<| content.tail       
        case EmbeddedRoot(content,indent,_)   => out.indented(indent) { if (content.nonEmpty) out << content.head <<| content.tail }       
        case Section(header, content,_)       => out <<         header <<|   content
        case TitledBlock(title, content, opt) => out <<@ ("div",opt) <<|> (Paragraph(title,Styles("title")) +: content) <<| "</div>"
        case QuotedBlock(content,attr,opt)    => out <<@ ("blockquote",opt); renderBlocks(quotedBlockContent(content,attr), "</blockquote>")
        case BulletListItem(content,_,opt)    => out <<@ ("li",opt);         renderBlocks(content, "</li>") 
        case EnumListItem(content,_,_,opt)    => out <<@ ("li",opt);         renderBlocks(content, "</li>") 
        case DefinitionListItem(term,defn,_)  => out << "<dt>" << term << "</dt>" <<| "<dd>"; renderBlocks(defn, "</dd>")
        case LineBlock(content,opt)           => out <<@ ("div",opt + Styles("line-block")) <<|> content <<| "</div>"
        case Figure(img,caption,legend,opt)   => out <<@ ("div",opt + Styles("figure")) <<|> figureContent(img,caption,legend) <<| "</div>"
        
        case Footnote(label,content,opt)   => renderTable(toTable(label,content,opt + Styles("footnote")))
        case Citation(label,content,opt)   => renderTable(toTable(label,content,opt + Styles("citation")))
        
        case WithFallback(fallback)         => out << fallback
        case c: Customizable                => c match {
          case BlockSequence(content, NoOpt) => if (content.nonEmpty) out << content.head <<| content.tail // this case could be standalone above, but triggers a compiler bug then
          case _ => out <<@ ("div",c.options) <<|> c.content <<| "</div>"
        }
        case unknown                        => out << "<div>" <<|> unknown.content <<| "</div>"
      }
    }
    
    def renderSpanContainer [T <: SpanContainer[T]](con: SpanContainer[T]): Unit = {
      def escapeTitle (s: String) = s.replace("&","&amp;").replace("\"","&quot;").replace("'","$#39;")
      def codeStyles (language: String) = if (language.isEmpty) Styles("code") else Styles("code", language)
      def crossLinkRef (path: PathInfo, ref: String) = {
        val target = path.relative.name.lastIndexOf(".") match {
          case -1 => path.relative.toString
          case i  => (path.relative.parent / (path.relative.name.take(i) + ".html")).toString
        }
        if (ref.isEmpty) target else s"$target#$ref"
      }
      
      con match {
        
        case Paragraph(content,opt)         => out <<@ ("p",opt)       <<  content <<  "</p>"  
        case Emphasized(content,opt)        => out <<@ ("em",opt)      <<  content <<  "</em>" 
        case Strong(content,opt)            => out <<@ ("strong",opt)  <<  content <<  "</strong>" 
        case ParsedLiteralBlock(content,opt)=> out <<@ ("pre",opt) << "<code>" <<<  content << "</code></pre>"
        case CodeBlock(lang,content,opt)    => out <<@ ("pre",opt+codeStyles(lang)) << "<code>" <<<  content << "</code></pre>"
        case Code(lang,content,opt)         => out <<@ ("code",opt+codeStyles(lang)) <<  content << "</code>"
        case Line(content,opt)              => out <<@ ("div",opt + Styles("line")) << content <<  "</div>"
        case Title(content, opt)            => out <<@ ("h1",opt) << content << "</h1>"
        case Header(level, content, opt)    => out <|; out <<@ ("h"+level.toString,opt) << content << "</h" << level.toString << ">"
  
        case ExternalLink(content, url, title, opt)     => out <<@ ("a", opt, "href"->url,       "title"->title.map(escapeTitle)) << content << "</a>"
        case InternalLink(content, ref, title, opt)     => out <<@ ("a", opt, "href"->("#"+ref), "title"->title.map(escapeTitle)) << content << "</a>"
        case CrossLink(content, ref, path, title, opt)  => out <<@ ("a", opt, "href"-> crossLinkRef(path, ref), "title"->title.map(escapeTitle)) << content << "</a>"
        
        case WithFallback(fallback)         => out << fallback
        case c: Customizable                => c match {
          case SpanSequence(content, NoOpt) => out << content // this case could be standalone above, but triggers a compiler bug then
          case TemplateRoot(content, NoOpt) => out << content
          case TemplateSpanSequence(content, NoOpt) => out << content
          case _ => out <<@ ("span",c.options) << c.content << "</span>"
        }
        case unknown                        => out << "<span>" << unknown.content << "</span>"
      }
    }
    
    def renderListContainer [T <: ListContainer[T]](con: ListContainer[T]): Unit = con match {
      case EnumList(content,format,start,opt) => 
          out <<@ ("ol", opt, ("class", format.enumType.toString.toLowerCase), ("start", noneIfDefault(start,1))) <<|> content <<| "</ol>"
      case BulletList(content,_,opt)   => out <<@ ("ul",opt) <<|> content <<| "</ul>"
      case DefinitionList(content,opt) => out <<@ ("dl",opt) <<|> content <<| "</dl>"
      
      case WithFallback(fallback)      => out << fallback
      case c: Customizable             => out <<@ ("div",c.options) <<|> c.content <<| "</div>"
      case unknown                     => out << "<div>" <<|> unknown.content <<| "</div>"
    }
    
    def renderTextContainer (con: TextContainer): Unit = con match {
      case Text(content,opt)           => opt match {
        case NoOpt                     => out                   <<&  content
        case _                         => out <<@ ("span",opt)  <<&  content << "</span>"
      }
      case TemplateString(content,opt) => opt match {
        case NoOpt                     => out                   <<  content
        case _                         => out <<@ ("span",opt)  <<  content << "</span>"
      }
      case RawContent(formats, content, opt) => if (formats.contains("html")) { opt match {
        case NoOpt                     => out                   <<   content
        case _                         => out <<@ ("span",opt)  <<   content << "</span>"
      }} 
      case Literal(content,opt)        => out <<@ ("code",opt)  <<<& content << "</code>" 
      case LiteralBlock(content,opt)   => out <<@ ("pre",opt)  << "<code>" <<<&  content << "</code></pre>"
      case Comment(content,opt)        => out << "<!-- "        <<   content << " -->"
      
      case WithFallback(fallback)      => out << fallback
      case c: Customizable             => out <<@ ("span",c.options) << c.content << "</span>"
      case unknown                     => out <<& unknown.content
    }
    
    def renderSimpleBlock (block: Block): Unit = block match {
      case Rule(opt)                   => out <<@ ("hr",opt) 
      case InternalLinkTarget(opt)     => out <<@ ("a",opt) << "</a>"
      case TargetFormat("html",e,_)    => out << e
      
      case WithFallback(fallback)      => out << fallback
      case unknown                     => ()
    }
    
    def renderSimpleSpan (span: Span): Unit = span match {
      case CitationLink(ref,label,opt) => out <<@ ("a",opt + Styles("citation"),"href"->("#"+ref)) << "[" << label << "]</a>" 
      case FootnoteLink(ref,label,opt) => out <<@ ("a",opt + Styles("footnote"),"href"->("#"+ref)) << "[" << label << "]</a>" 
      case SectionNumber(pos, opt)     => out << Text(pos.mkString(".") + " ", opt + Styles("sectionNumber"))

      case Image(text,uri,width,height,title,opt) =>
        def sizeAttr (size: Option[Size], styleName: String): (Option[String],Option[String]) = size map {
          case Size(amount, "px") => (Some(amount.toInt.toString), None)
          case Size(amount, unit) => (None, Some(s"$styleName:$amount$unit"))
        } getOrElse (None, None)
        val (widthAttr, wStyle) = sizeAttr(width, "width")
        val (heightAttr, hStyle) = sizeAttr(height, "height")
        val styleAttr = (wStyle ++ hStyle).reduceLeftOption((a,b) => s"$a;$b")
        out <<@ ("img",opt,"src"->uri.uri,"alt"->text,"title"->title,
                 "width"->widthAttr,"height"->heightAttr,"style"->styleAttr)

      case LineBreak(opt)                 => out << "<br>"
      case TemplateElement(elem,indent,_) => out.indented(indent) { out << elem }
      
      case WithFallback(fallback)         => out << fallback
      case unknown                        => ()
    }
    
    def renderTableElement (elem: TableElement): Unit = elem match {
      case TableHead(rows,opt)         => out <<@ ("thead",opt) <<|> rows <<| "</thead>"
      case TableBody(rows,opt)         => out <<@ ("tbody",opt) <<|> rows <<| "</tbody>"    
      case Caption(content, opt)       => out <<@ ("caption",opt) <<  content <<  "</caption>" 
      case Columns(columns,opt)        => out <<@ ("colgroup",opt) <<|> columns <<| "</colgroup>"  
      case Column(opt)            => out <<@ ("col",opt) << "</col>"  
      case Row(cells,opt)         => out <<@ ("tr",opt) <<|> cells <<| "</tr>"
      case Cell(HeadCell, content, colspan, rowspan, opt) => out <<@ 
            ("th", opt, "colspan"->noneIfDefault(colspan,1), "rowspan"->noneIfDefault(rowspan,1)); renderBlocks(content, "</th>") 
      case Cell(BodyCell, content, colspan, rowspan, opt) => out <<@ 
            ("td", opt, "colspan"->noneIfDefault(colspan,1), "rowspan"->noneIfDefault(rowspan,1)); renderBlocks(content, "</td>") 
    }
    
    def renderUnresolvedReference (ref: Reference): Unit = {
      out << InvalidSpan(SystemMessage(Error,s"unresolved reference: $ref"), Text(ref.source)) 
    }
    
    def renderInvalidElement (elem: Invalid[_ <: Element]): Unit = elem match {
      case InvalidBlock(msg, fallback, opt) => if (include(msg)) out << List(Paragraph(List(msg),opt), fallback)
                                               else out << fallback
      case e                                => if (include(e.message)) out << e.message << " " << e.fallback
                                               else out << e.fallback 
    }
    
    def renderSystemMessage (message: SystemMessage): Unit = {
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
  
  override def defaultTheme: Theme[HTMLWriter] = Theme[HTMLWriter](
    defaultTemplate = Some(HTML.templateResource.content)
  )
  
}

/** The default instance of the HTML renderer.
 */
object HTML extends HTML(None, true) {
  
  lazy val templateResource: TemplateDocument =
    DefaultTemplateParser.parse(Input.fromClasspath("/templates/default.template.html", Root / "default.template.html"))
  
}
