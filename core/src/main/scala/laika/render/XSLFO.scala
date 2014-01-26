/*
 * Copyright 2014 the original author or authors.
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
import laika.tree.Templates._
import laika.io.Output
import laika.factory.RendererFactory
  
/** A renderer for XSL-FO output. May be directly passed to the `Render` or `Transform` APIs:
 * 
 *  {{{
 *  Render as XSLFO from document toFile "hello.html"
 *  
 *  Transform from Markdown to XSLFO fromFile "hello.md" toFile "hello.fo"
 *  }}}
 *  
 *  TODO - this is a new renderer and currently work in progress - do not use yet
 * 
 *  @author Jens Halm
 */
class XSLFO private (messageLevel: Option[MessageLevel], renderFormatted: Boolean) 
    extends RendererFactory[HTMLWriter] {
  
  val fileSuffix = "html"
 
  /** Specifies the minimum required level for a system message
   *  to get included into the output by this renderer.
   */
  def withMessageLevel (level: MessageLevel) = new XSLFO(Some(level), renderFormatted)
  
  /** Renders XSL-FO without any formatting (line breaks or indentation) around tags. 
   *  Useful when storing the output in a database for example. 
   */
  def unformatted = new XSLFO(messageLevel, false)
  
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
  def newRenderer (output: Output, render: Element => Unit) = {
    val out = new HTMLWriter(output asFunction, render, formatted = renderFormatted)  
    (out, renderElement(out))
  }

  
  private def renderElement (out: HTMLWriter)(elem: Element): Unit = {
    
    def listContainer (opt: Options, content: Seq[ListItem], attr: (String,String)*) = 
      out <<@ ("fo:block", NoOpt, attr: _*) <<|> content <<| "</fo:block>"
      
    def blockContainer (opt: Options, content: Seq[Block], attr: (String,String)*) = 
      out <<@ ("fo:block", NoOpt, attr: _*) <<|> content <<| "</fo:block>"
      
    def block (opt: Options, content: Seq[Span], attr: (String,String)*) = 
      out <<@ ("fo:block", NoOpt, attr: _*) << content << "</fo:block>"

    def blockWithWS (opt: Options, content: Seq[Span], attr: (String,String)*) = 
      out <<@ ("fo:block", NoOpt, attr: _*) <<< content << "</fo:block>"
    
    def inline (opt: Options, content: Seq[Span], attr: (String,String)*) = 
      out <<@ ("fo:inline", NoOpt, attr: _*) << content << "</fo:inline>"
      
    def internalLink (opt: Options, target: String, content: Seq[Span], attr: (String,String)*) = 
      out <<@ ("fo:basic-link", NoOpt, (attr :+ ("internal-destination"->target)): _*) << content << "</fo:basic-link>"
    
    def externalLink (opt: Options, url: String, content: Seq[Span], attr: (String,String)*) = 
      out <<@ ("fo:basic-link", NoOpt, (attr :+ ("external-destination"->url)): _*) << content << "</fo:basic-link>"
      
    def text (opt: Options, content: String, attr: (String,String)*) = 
      out <<@ ("fo:inline", NoOpt, attr: _*) <<& content << "</fo:inline>"
      
    def textWithWS (opt: Options, content: String, attr: (String,String)*) = 
      out <<@ ("fo:inline", NoOpt, attr: _*) <<<& content << "</fo:inline>"
      
    def rawText (opt: Options, content: String, attr: (String,String)*) = 
      out <<@ ("fo:inline", NoOpt, attr: _*) << content << "</fo:inline>"
      
    def include (msg: SystemMessage) = {
      messageLevel flatMap {lev => if (lev <= msg.level) Some(lev) else None} isDefined
    }
    
    def noneIfDefault [T](actual: T, default: T) = if (actual == default) None else Some(actual.toString)
    
    def renderBlocks (blocks: Seq[Block], close: String) = blocks match {
      case ss @ SpanSequence(_,_) :: Nil => out << ss << close
      case Paragraph(content,opt) :: Nil => out << SpanSequence(content,opt) << close
      case other                         => out <<|> other <<| close
    }
    
    def renderTable (table: Table) = {
      val children = List(table.caption,table.columns,table.head,table.body) filterNot (_.content.isEmpty)
      
      out <<@ ("???", table.options) <<|> children <<| "</???>"
    }
    
    object WithFallback {
      def unapply (value: Element) = value match {
        case f: Fallback => Some(f.fallback)
        case _ => None
      }
    }
    
    def renderBlockContainer [T <: BlockContainer[T]](con: BlockContainer[T]) = {
  
      def toTable (label: String, content: Seq[Block], options: Options): Table = {
        val left = Cell(BodyCell, List(SpanSequence(List(Text(s"[$label]")))))
        val right = Cell(BodyCell, content)
        val row = Row(List(left,right))
        Table(TableHead(Nil), TableBody(List(row)), Caption(),
            Columns.options(Styles("label"),NoOpt), options)
      }
      
      def quotedBlockContent (content: Seq[Block], attr: Seq[Span]) = 
        if (attr.isEmpty) content
        else content :+ Paragraph(attr, Styles("attribution"))
        
      def figureContent (img: Span, caption: Seq[Span], legend: Seq[Block]): List[Block] =
        List(Paragraph(List(img)), Paragraph(caption, Styles("caption")), BlockSequence(legend, Styles("legend")))
      
      con match {
        case RootElement(content)             => if (content.nonEmpty) out << content.head <<| content.tail       
        case EmbeddedRoot(content,indent,_)   => out.indented(indent) { if (content.nonEmpty) out << content.head <<| content.tail }       
        case Section(header, content,_)       => out <<| header <<| content
        case TitledBlock(title, content, opt) => blockContainer(opt, Paragraph(title,Styles("title")) +: content)
        case QuotedBlock(content,attr,opt)    => blockContainer(opt, quotedBlockContent(content,attr))
        case BulletListItem(content,_,opt)    => out <<@ ("???",opt);         renderBlocks(content, "</???>") 
        case EnumListItem(content,_,_,opt)    => out <<@ ("???",opt);         renderBlocks(content, "</???>") 
        case DefinitionListItem(term,defn,_)  => out << "<???>" << term << "</???>" <<| "<???>"; renderBlocks(defn, "</???>")
        case LineBlock(content,opt)           => out <<@ ("???",opt + Styles("line-block")) <<|> content <<| "</???>"
        case Figure(img,caption,legend,opt)   => blockContainer(opt, figureContent(img,caption,legend))
        
        case Footnote(label,content,opt)   => renderTable(toTable(label,content,opt + Styles("footnote")))
        case Citation(label,content,opt)   => renderTable(toTable(label,content,opt + Styles("citation")))
        
        case WithFallback(fallback)         => out << fallback
        case c: Customizable                => c match {
          case BlockSequence(content, NoOpt) => if (content.nonEmpty) out << content.head <<| content.tail // this case could be standalone above, but triggers a compiler bug then
          case unknown                      => blockContainer(unknown.options, unknown.content)
        }
        case unknown                        => blockContainer(NoOpt, unknown.content)
      }
    }
    
    def renderSpanContainer [T <: SpanContainer[T]](con: SpanContainer[T]) = {
      def codeStyles (language: String) = if (language.isEmpty) Styles("code") else Styles("code", language)
      def crossLinkRef (path: PathInfo, ref: String) = path.toString + "-" + ref
      
      con match {
        
        case Paragraph(content,opt)         => block(opt,content)
        case ParsedLiteralBlock(content,opt)=> blockWithWS(opt,content)
        case CodeBlock(lang,content,opt)    => blockWithWS(opt+codeStyles(lang),content)
        case Header(level, content, opt)    => block(opt+Styles("level"+level.toString),content)

        case Emphasized(content,opt)        => inline(opt,content,"font-style"->"italic")
        case Strong(content,opt)            => inline(opt,content,"font-weight"->"bold")
        case Code(lang,content,opt)         => inline(opt+codeStyles(lang),content,"font-family"->"monospace")
        case Line(content,opt)              => out <<@ ("???",opt + Styles("line")) << content <<  "</???>"
  
        case ExternalLink(content, url, _, opt)     => externalLink(opt, url, content)
        case InternalLink(content, ref, _, opt)     => internalLink(opt, ref, content) // TODO - need to integrate doc path
        case CrossLink(content, ref, path, _, opt)  => internalLink(opt, crossLinkRef(path,ref), content)
        
        case WithFallback(fallback)         => out << fallback
        case c: Customizable                => c match {
          case SpanSequence(content, NoOpt) => out << content // this case could be standalone above, but triggers a compiler bug then
          case TemplateRoot(content, NoOpt) => out << content
          case TemplateSpanSequence(content, NoOpt) => out << content
          case unknown: Block               => block(unknown.options, unknown.content)
          case unknown                      => inline(unknown.options, unknown.content)
        }
        case unknown: Block                 => block(NoOpt, unknown.content)
        case unknown                        => inline(NoOpt, unknown.content)
      }
    }
    
    def renderListContainer [T <: ListContainer[T]](con: ListContainer[T]) = con match {
      case EnumList(content,format,start,opt) => 
          out <<@ ("???", opt, ("class", format.enumType.toString.toLowerCase), ("start", noneIfDefault(start,1))) <<|> content <<| "</???>"
      case BulletList(content,_,opt)   => out <<@ ("???",opt) <<|> content <<| "</???>"
      case DefinitionList(content,opt) => out <<@ ("???",opt) <<|> content <<| "</???>"
      
      case WithFallback(fallback)      => out << fallback
      case c: Customizable             => listContainer(c.options, c.content)
      case unknown                     => listContainer(NoOpt, unknown.content)
    }
    
    def renderTextContainer (con: TextContainer) = con match {
      case Text(content,opt)           => opt match {
        case NoOpt                     => out <<& content
        case _                         => text(opt, content)
      }
      case TemplateString(content,opt) => opt match {
        case NoOpt                     => out << content
        case _                         => rawText(opt, content)
      }
      case RawContent(formats, content, opt) => if (formats.contains("fo")) { opt match {
        case NoOpt                     => out <<   content
        case _                         => rawText(opt, content)
      }} 
      case Literal(content,opt)        => textWithWS(opt, content)
      case LiteralBlock(content,opt)   => textWithWS(opt, content)
      case Comment(content,opt)        => out << "<!-- "       <<   content << " -->"
      
      case WithFallback(fallback)      => out << fallback
      case c: Customizable if c.options == NoOpt => out <<& c.content
      case c: Customizable             => text(c.options, c.content)
      case unknown                     => out <<& unknown.content
    }
    
    def renderSimpleBlock (block: Block) = block match {
      case Rule(opt)                   => out <<@ ("fo:leader",opt,"leader-pattern"->"rule") << "</fo:leader>" 
      case InternalLinkTarget(opt)     => inline(opt, Nil)
      
      case WithFallback(fallback)      => out << fallback
      case unknown                     => ()
    }
    
    def renderSimpleSpan (span: Span) = span match {
      case CitationLink(ref,label,opt) => out <<@ ("???",opt + Styles("citation"),"href"->("#"+ref)) << "[" << label << "]</???>" 
      case FootnoteLink(ref,label,opt) => out <<@ ("???",opt + Styles("footnote"),"href"->("#"+ref)) << "[" << label << "]</???>" 
      case Image(text,url,title,opt)   => out <<@ ("???",opt,"src"->url,"alt"->text,"title"->title)
      case LineBreak(opt)              => out << "&#x2028;"
      case TemplateElement(elem,indent,_) => out.indented(indent) { out << elem }
      
      case WithFallback(fallback)      => out << fallback
      case unknown                     => ()
    }
    
    def renderTableElement (elem: TableElement) = elem match {
      case TableHead(rows,opt)         => out <<@ ("???",opt) <<|> rows <<| "</???>"
      case TableBody(rows,opt)         => out <<@ ("???",opt) <<|> rows <<| "</???>"    
      case Caption(content, opt)       => out <<@ ("???",opt) <<  content <<  "</???>" 
      case Columns(columns,opt)        => out <<@ ("???",opt) <<|> columns <<| "</???>"  
      case Column(opt)            => out <<@ ("???",opt) << "</???>"  
      case Row(cells,opt)         => out <<@ ("???",opt) <<|> cells <<| "</???>"
      case Cell(HeadCell, content, colspan, rowspan, opt) => out <<@ 
            ("???", opt, "colspan"->noneIfDefault(colspan,1), "rowspan"->noneIfDefault(rowspan,1)); renderBlocks(content, "</???>") 
      case Cell(BodyCell, content, colspan, rowspan, opt) => out <<@ 
            ("???", opt, "colspan"->noneIfDefault(colspan,1), "rowspan"->noneIfDefault(rowspan,1)); renderBlocks(content, "</???>") 
    }
    
    def renderUnresolvedReference (ref: Reference) = {
      out << InvalidSpan(SystemMessage(Error,s"unresolved reference: $ref"), Text(ref.source)) 
    }
    
    def renderInvalidElement (elem: Invalid[_ <: Element]) = elem match {
      case InvalidBlock(msg, fallback, opt) => if (include(msg)) out << List(Paragraph(List(msg),opt), fallback)
                                               else out << fallback
      case e                                => if (include(e.message)) out << e.message << " " << e.fallback
                                               else out << e.fallback 
    }
    
    def renderSystemMessage (message: SystemMessage) = {
      if (include(message)) 
        text(message.options + Styles("system-message", message.level.toString.toLowerCase), message.content)
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

/** The default instance of the XSL-FO renderer.
 */
object XSLFO extends XSLFO(None, true)
