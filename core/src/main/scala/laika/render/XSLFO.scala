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

import laika.tree.Documents.Path
import laika.tree.Documents.Root
import laika.tree.Elements._
import laika.tree.ElementTraversal
import laika.tree.Templates._
import laika.io.Input
import laika.io.Output
import laika.factory.RendererFactory
import laika.util.RomanNumerals
import laika.template.ParseTemplate
import laika.parse.css.Styles.StyleDeclarationSet
import laika.parse.css.ParseStyleSheet
import scala.language.existentials
import FOWriter._ 
  
/** A renderer for XSL-FO output. May be directly passed to the `Render` or `Transform` APIs:
 * 
 *  {{{
 *  Render as XSLFO from document toFile "hello.html"
 *  
 *  Transform from Markdown to XSLFO fromFile "hello.md" toFile "hello.fo"
 *  }}}
 *  
 *  This renderer is usually used as an interim format for producing a PDF, 
 *  where you do not deal with this format directly. But it can alternatively
 *  also be used as the final output and then get processed by external tools.
 * 
 *  @author Jens Halm
 */
class XSLFO private (messageLevel: Option[MessageLevel], renderFormatted: Boolean) 
    extends RendererFactory[FOWriter] {
  
  
  val fileSuffix = "fo"
 
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
   *  @param root the root element the new renderer will be used for
   *  @param render the composite render function to delegate to when elements need to render their children
   *  @return a tuple consisting of the writer API for customizing
   *  the renderer as well as the actual default render function itself
   */
  def newRenderer (output: Output, root: Element, render: Element => Unit, styles: StyleDeclarationSet) = {
    val out = new FOWriter(output asFunction, render, output.path, styles, formatted = renderFormatted)
    val (footnotes, citations) = collectTargets(root)
    (out, renderElement(out,footnotes,citations,output.path))
  }
  
  private def collectTargets (root: Element): (Map[String,Footnote], Map[String,Citation]) = root match {
    case et: ElementTraversal[_] => (
        et collect { case f:Footnote if f.options.id.isDefined => (f.options.id.get, f) } toMap,
        et collect { case c:Citation if c.options.id.isDefined => (c.options.id.get, c) } toMap)
    case _ => (Map.empty, Map.empty)
  }

  private def renderElement (out: FOWriter, footnotes: Map[String,Footnote], 
      citations: Map[String,Citation], path: Path)(elem: Element): Unit = {
    
    def include (msg: SystemMessage) = {
      messageLevel flatMap {lev => if (lev <= msg.level) Some(lev) else None} isDefined
    }
    
    def noneIfDefault [T](actual: T, default: T) = if (actual == default) None else Some(actual.toString)
    
    def renderTable (table: Table) = {
      if (table.caption.content.nonEmpty) {
        // FOP does not support fo:table-caption
        out << TitledBlock(table.caption.content, List(table.copy(caption = Caption())))
      }
      else {
        val children = table.columns.content ++ (List(table.head, table.body) filterNot (_.content.isEmpty))
        out <<@ ("fo:table", table) <<|> children <<| "</fo:table>"
      }
    }
    
    object WithFallback {
      def unapply (value: Element) = value match {
        case f: Fallback => Some(f.fallback)
        case _ => None
      }
    }
    
    def renderBlockContainer [T <: BlockContainer[T]](con: BlockContainer[T]) = {
  
      def toList (label: String, content: Seq[Block]): Block = {
        val labelElement = List(Text(label, Styles("footnote-label")))
        val bodyElement = List(BlockSequence(content, Styles("footnote-body")))
        val item = DefinitionListItem(labelElement, bodyElement)
        DefinitionList(List(item), Styles("footnote"))
      }
      
      def quotedBlockContent (content: Seq[Block], attr: Seq[Span]) = 
        if (attr.isEmpty) content
        else content :+ Paragraph(attr, Styles("attribution"))
        
      def figureContent (img: Span, caption: Seq[Span], legend: Seq[Block]): List[Block] =
        List(Paragraph(List(img)), Paragraph(caption, Styles("caption")), BlockSequence(legend, Styles("legend")))
      
      def enumLabel (format: EnumFormat, num: Int) = {
        val pos = format.enumType match {
          case Arabic => num.toString
          case LowerAlpha => ('a' + num - 1).toString
          case UpperAlpha => ('A' + num - 1).toString
          case LowerRoman => RomanNumerals.intToRoman(num).toLowerCase
          case UpperRoman => RomanNumerals.intToRoman(num).toUpperCase
        } 
        format.prefix + pos + format.suffix
      } 
      
      def bulletLabel (format: BulletFormat) = format match {
        case StringBullet(_) => "&#x2022;"
        case other           => other.toString
      }
      
      con match {
        case RootElement(content)               => if (content.nonEmpty) out << content.head <<| content.tail       
        case EmbeddedRoot(content,indent,_)     => out.indented(indent) { if (content.nonEmpty) out << content.head <<| content.tail }       
        case Section(header, content,_)         => out <<| header <<| content
        case e @ TitledBlock(title, content, _) => out.blockContainer(e, Paragraph(title,Styles("title")) +: content)
        case e @ QuotedBlock(content,attr,_)    => out.blockContainer(e, quotedBlockContent(content,attr))
        
        case e @ BulletListItem(content,format,_)   => out.listItem(e, List(Text(bulletLabel(format))), content) 
        case e @ EnumListItem(content,format,num,_) => out.listItem(e, List(Text(enumLabel(format,num))), content)
        case e @ DefinitionListItem(term,defn,_)    => out.listItem(e, term, defn)
        case e @ ListItemBody(content,_)            => out.listItemBody(e, content)
        
        case e @ LineBlock(content,_)           => out.blockContainer(e, content)
        case e @ Figure(img,caption,legend,_)   => out.blockContainer(e, figureContent(img,caption,legend))
        
        case e @ Footnote(label,content,_)      => out <<@ ("fo:footnote-body",e) <<|> toList(label,content) <<| "</fo:footnote-body>" 
        case e @ Citation(label,content,_)      => out <<@ ("fo:footnote-body",e) <<|> toList(label,content) <<| "</fo:footnote-body>"  
        
        case WithFallback(fallback)         => out << fallback
        case c: Customizable                => c match {
          case BlockSequence(content, NoOpt) => if (content.nonEmpty) out << content.head <<| content.tail // this case could be standalone above, but triggers a compiler bug then
          case unknown                      => out.blockContainer(unknown, unknown.content)
        }
        case unknown                        => out.blockContainer(unknown, unknown.content)
      }
    }
    
    def renderSpanContainer [T <: SpanContainer[T]](con: SpanContainer[T]) = {
      def codeStyles (language: String) = if (language.isEmpty) NoOpt else Styles(language)
      def crossLinkRef (path: Path, ref: String) = path.toString + "-" + ref
      
      con match {
        
        case e @ Paragraph(content,_)         => out.block(e,content)
        case e @ ParsedLiteralBlock(content,_)=> out.blockWithWS(e,content)
        case e @ CodeBlock(lang,content,_)    => out.blockWithWS(e.copy(options=e.options + codeStyles(lang)),content)
        case e @ Header(level, content,_)     => out.block(e.copy(options=e.options + Styles("level"+level.toString)),content,"keep-with-next"->"always")

        case e @ Emphasized(content,_)        => out.inline(e,content,"font-style"->"italic")
        case e @ Strong(content,_)            => out.inline(e,content,"font-weight"->"bold")
        case e @ Code(lang,content,_)         => out.inline(e.copy(options=e.options + codeStyles(lang)),content,"font-family"->"monospace")
        case e @ Line(content,_)              => out.block(e, content)
  
        case e @ ExternalLink(content, url, _, _)     => out.externalLink(e, url, content)
        case e @ InternalLink(content, ref, _, _)     => out.internalLink(e, crossLinkRef(path, ref), content)
        case e @ CrossLink(content, ref, path, _, _)  => out.internalLink(e, crossLinkRef(path.absolute, ref), content)
        
        case WithFallback(fallback)         => out << fallback
        case c: Customizable                => c match {
          case SpanSequence(content, NoOpt) => out << content // this case could be standalone above, but triggers a compiler bug then
          case TemplateRoot(content, NoOpt) => out << content
          case TemplateSpanSequence(content, NoOpt) => out << content
          case unknown: Block               => out.block(unknown, unknown.content)
          case unknown                      => out.inline(unknown, unknown.content)
        }
        case unknown                        => out.inline(unknown, unknown.content)
      }
    }
    
    def renderListContainer [T <: ListContainer[T]](con: ListContainer[T]) = con match {
      case e @ EnumList(content,_,_,_)   => out.listBlock(e, content)
      case e @ BulletList(content,_,_)   => out.listBlock(e, content)
      case e @ DefinitionList(content,_) => out.listBlock(e, content)
      
      case WithFallback(fallback)      => out << fallback
      case c: Customizable             => out.listBlock(c, c.content)
      case unknown                     => out.listBlock(unknown, unknown.content)
    }
    
    def renderTextContainer (con: TextContainer) = con match {
      case e @ Text(content,_)           => out.text(e, content)
      case e @ TemplateString(content,_) => out.rawText(e, content)
      case e @ RawContent(formats, content, _) => if (formats.contains("fo")) out.rawText(e, content)
      case e @ Literal(content,_)        => out.textWithWS(e, content)
      case e @ LiteralBlock(content,_)   => out.textWithWS(e, content)
      case e: BookmarkTitle              => out.bookmarkTitle(e)
      case Comment(content,_)            => out << "<!-- " << content << " -->"
      
      case WithFallback(fallback)      => out << fallback
      case c: Customizable if c.options == NoOpt => out <<& c.content
      case c: Customizable             => out.text(c, c.content)
      case unknown                     => out <<& unknown.content
    }
    
    def renderSimpleBlock (block: Block) = block match {
      case e @ ListItemLabel(content,_) => out.listItemLabel(e, content)
      case e: Rule                      => out <<@ ("fo:leader",e,"leader-pattern"->"rule") << "</fo:leader>" 
      case e: InternalLinkTarget        => out.inline(e, Nil)
      case e: BookmarkTree              => out.bookmarkTree(e)
      case e: Bookmark                  => out.bookmark(e)
      
      case WithFallback(fallback)       => out << fallback
      case unknown                      => ()
    }
    
    def renderSimpleSpan (span: Span) = span match {
      case e @ CitationLink(ref,label,_)  => citations.get(ref).foreach(out.footnote(e,label,_))
      case e @ FootnoteLink(ref,label,_)  => footnotes.get(ref).foreach(out.footnote(e,label,_))
      case e @ Image(_,uri,_,_)           => out.externalGraphic(e, uri.localRef.fold(uri.uri)(_.absolute.toString)) // TODO - ignoring title and alt for now
      case LineBreak(_)                   => out << "&#x2028;"
      case TemplateElement(elem,indent,_) => out.indented(indent) { out << elem }
      
      case WithFallback(fallback)         => out << fallback
      case unknown                        => ()
    }
    
    def renderTableElement (elem: TableElement) = elem match {
      case e @ TableHead(rows,_)   => out <<@ ("fo:table-header", e) <<|> rows <<| "</fo:table-header>"
      case e @ TableBody(rows,_)   => out <<@ ("fo:table-body", e) <<|> rows <<| "</fo:table-body>"
      case Caption(_,_)            => () // replaced by Table renderer
      case Columns(_,_)            => () // replaced by Table renderer
      case e: Column               => out <<@ ("fo:table-column",e)
      case e @ Row(cells,_)        => out <<@ ("fo-table-row",e) <<|> cells <<| "</fo-table-row>"
      case e @ Cell(_, content, colspan, rowspan, _) => out <<@ 
            ("fo-table-cell", e, 
                "number-columns-spanned"->noneIfDefault(colspan,1), 
                "number-rows-spanned"->noneIfDefault(rowspan,1)) <<|> content <<| "</fo-table-cell>"
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
        out.text(message.copy(options=message.options + Styles(message.level.toString.toLowerCase)), message.content)
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
  
  override lazy val defaultStyles = XSLFO.styleResource
  override lazy val defaultTemplate = XSLFO.templateResource.content
  
}

/** The default instance of the XSL-FO renderer.
 */
object XSLFO extends XSLFO(None, true) {
  
  lazy val styleResource = ParseStyleSheet.fromInput(Input.fromClasspath("/styles/default.fo.css", Root / "default.fo.css"))
  
  lazy val templateResource = ParseTemplate.fromInput(Input.fromClasspath("/templates/default.template.fo", Root / "default.template.fo"))
  
}
