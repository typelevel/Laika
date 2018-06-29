/*
 * Copyright 2014-2016 the original author or authors.
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

import laika.tree.Paths.Path
import laika.tree.Paths.Root
import laika.tree.Documents.TemplateDocument
import laika.tree.Elements._
import laika.tree.ElementTraversal
import laika.tree.Templates._
import laika.rewrite.TreeUtil
import laika.io.Input
import laika.io.Output
import laika.factory.RendererFactory
import laika.util.RomanNumerals
import laika.parse.css.Styles.StyleDeclarationSet

import scala.language.existentials
import FOWriter._
import laika.api.ext.Theme
import laika.directive.DefaultTemplateParser
import laika.parse.core.combinator.Parsers
import laika.parse.css.CSSParsers

/** A renderer for XSL-FO output. May be directly passed to the `Render` or `Transform` APIs:
 * 
 *  {{{
 *  Render as XSLFO from document toFile "hello.fo"
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
class XSLFO private (styles: Option[StyleDeclarationSet], renderFormatted: Boolean)
    extends RendererFactory[FOWriter] {
  
  
  val fileSuffix = "fo"
 
  /** Renders XSL-FO without any formatting (line breaks or indentation) around tags.
   *  Useful when storing the output in a database for example. 
   */
  def unformatted: XSLFO = new XSLFO(styles, false)
  
  /** Adds the specified styles to the default styles this renderer applies.
   */
  def withStyles(additionalStyles: StyleDeclarationSet): XSLFO = new XSLFO(Some(defaultTheme.defaultStyles ++ additionalStyles), renderFormatted)
  
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
  def newRenderer (output: Output, root: Element, render: Element => Unit, styles: StyleDeclarationSet, messageLevel: MessageLevel): (FOWriter, Element => Unit) = {
    val out = new FOWriter(output asFunction, render, root, output.path, styles, formatted = renderFormatted)
    val (footnotes, citations) = collectTargets(root)
    (out, renderElement(out,footnotes,citations,output.path, messageLevel))
  }
  
  private def collectTargets (root: Element): (Map[String,Footnote], Map[String,Citation]) = root match {
    case et: ElementTraversal[_] => (
        et collect { case f:Footnote if f.options.id.isDefined => (f.options.id.get, f) } toMap,
        et collect { case c:Citation if c.options.id.isDefined => (c.options.id.get, c) } toMap)
    case _ => (Map.empty, Map.empty)
  }

  private def renderElement (out: FOWriter, footnotes: Map[String,Footnote], 
      citations: Map[String,Citation], path: Path, messageLevel: MessageLevel)(elem: Element): Unit = {
    
    def include (msg: SystemMessage): Boolean = messageLevel <= msg.level

    def noneIfDefault [T](actual: T, default: T): Option[String] = if (actual == default) None else Some(actual.toString)
    
    def renderTable (table: Table): Unit = {
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
      def unapply (value: Element): Option[Element] = value match {
        case f: Fallback => Some(f.fallback)
        case _ => None
      }
    }
    
    def renderBlockContainer [T <: BlockContainer[T]](con: BlockContainer[T]): Unit = {
  
      def quotedBlockContent (content: Seq[Block], attr: Seq[Span]): Seq[Block] = 
        if (attr.isEmpty) content
        else content :+ Paragraph(attr, Styles("attribution"))
        
      def figureContent (img: Span, caption: Seq[Span], legend: Seq[Block]): List[Block] =
        List(Paragraph(List(img)), Paragraph(caption, Styles("caption")), BlockSequence(legend, Styles("legend")))
      
      def enumLabel (format: EnumFormat, num: Int): String = {
        val pos = format.enumType match {
          case Arabic => num.toString
          case LowerAlpha => ('a' + num - 1).toChar.toString
          case UpperAlpha => ('A' + num - 1).toChar.toString
          case LowerRoman => RomanNumerals.intToRoman(num).toLowerCase
          case UpperRoman => RomanNumerals.intToRoman(num).toUpperCase
        } 
        format.prefix + pos + format.suffix
      } 
      
      def bulletLabel (format: BulletFormat): Span = format match {
        case StringBullet(_) => RawContent(Seq("fo"), "&#x2022;")
        case other           => Text(other.toString)
      }
      
      def replaceSpanContainers (content: Seq[Block]): Seq[Block] = content map {
        case sc: SpanContainer[_] => Paragraph(sc.content, sc.options)
        case other => other
      }
      
      con match {
        case RootElement(content)               => if (content.nonEmpty) out << content.head <<| content.tail       
        case EmbeddedRoot(content,indent,_)     => out.indented(indent) { if (content.nonEmpty) out << content.head <<| content.tail }       
        case Section(header, content,_)         => out <<| header <<| content
        case e @ TitledBlock(title, content, _) => out.blockContainer(e, Paragraph(title,Styles("title")) +: content)
        case e @ QuotedBlock(content,attr,_)    => out.blockContainer(e, quotedBlockContent(content,attr))
        
        case e @ BulletListItem(content,format,_)   => out.listItem(e, List(bulletLabel(format)), content) 
        case e @ EnumListItem(content,format,num,_) => out.listItem(e, List(Text(enumLabel(format,num))), content)
        case e @ DefinitionListItem(term,defn,_)    => out.listItem(e, term, defn)
        case e @ ListItemBody(content,_)            => out.listItemBody(e, replaceSpanContainers(content))
        
        case e @ LineBlock(content,_)           => out.blockContainer(e, content)
        case e @ Figure(img,caption,legend,_)   => out.blockContainer(e, figureContent(img,caption,legend))
        
        case e @ FootnoteBody(content,_)        => out <<@ ("fo:footnote-body",e) <<|> content <<| "</fo:footnote-body>" 
        case e @ Footnote(label,content,_)      => () // rendered in link position
        case e @ Citation(label,content,_)      => () // rendered in link position
        
        case WithFallback(fallback)         => out << fallback
        case c: Customizable                => c match {
          case BlockSequence(content, NoOpt) => if (content.nonEmpty) out << content.head <<| content.tail // this case could be standalone above, but triggers a compiler bug then
          case unknown                      => out.blockContainer(unknown, unknown.content)
        }
        case unknown                        => out.blockContainer(unknown, unknown.content)
      }
    }
    
    def renderSpanContainer [T <: SpanContainer[T]](con: SpanContainer[T]): Unit = {
      def codeStyles (language: String) = if (language.isEmpty) NoOpt else Styles(language)
      def crossLinkRef (path: Path, ref: String) = out.buildId(path, ref)
      
      con match {
        
        case e @ Paragraph(content,_)         => out.block(e,content)
        case e @ ParsedLiteralBlock(content,_)=> out.blockWithWS(e,content)
        case e @ CodeBlock(lang,content,_)    => out.blockWithWS(e.copy(options=e.options + codeStyles(lang)),content)
        case e @ Header(level, content,_)     => out.block(e.copy(options=e.options + Styles("level"+level.toString)),content,"keep-with-next"->"always")
        case e @ Title(content,_)             => out << s"""<fo:marker marker-class-name="chapter"><fo:block>""" <<& TreeUtil.extractText(content) << "</fo:block></fo:marker>"
                                                 out <|;
                                                 out.block(e.copy(options=e.options),content,"keep-with-next"->"always")

        case e @ Emphasized(content,_)        => out.inline(e,content)
        case e @ Strong(content,_)            => out.inline(e,content)
        case e @ Code(lang,content,_)         => out.inline(e.copy(options=e.options + codeStyles(lang)),content)
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
    
    def renderListContainer [T <: ListContainer[T]](con: ListContainer[T]): Unit = con match {
      case e @ EnumList(content,_,_,_)   => out.listBlock(e, content)
      case e @ BulletList(content,_,_)   => out.listBlock(e, content)
      case e @ DefinitionList(content,_) => out.listBlock(e, content)
      
      case WithFallback(fallback)      => out << fallback
      case c: Customizable             => out.listBlock(c, c.content)
      case unknown                     => out.listBlock(unknown, unknown.content)
    }
    
    def renderTextContainer (con: TextContainer): Unit = con match {
      case e @ Text(content,_)           => out.text(e, content)
      case e @ TemplateString(content,_) => out.rawText(e, content)
      case e @ RawContent(formats, content, _) => if (formats.contains("fo")) out.rawText(e, content)
      case e @ Literal(content,_)        => out.textWithWS(e, content)
      case e @ LiteralBlock(content,_)   => out.textBlockWithWS(e, content)
      case e: BookmarkTitle              => out.bookmarkTitle(e)
      case Comment(content,_)            => out << "<!-- " << content << " -->"
      
      case WithFallback(fallback)      => out << fallback
      case c: Customizable if c.options == NoOpt => out <<& c.content
      case c: Customizable             => out.text(c, c.content)
      case unknown                     => out <<& unknown.content
    }
    
    def renderSimpleBlock (block: Block): Unit = block match {
      case e @ ListItemLabel(content,_) => out.listItemLabel(e, content)
      case e: Rule                      => out <<@ ("fo:leader",e,"leader-pattern"->"rule") << "</fo:leader>" 
      case e: InternalLinkTarget        => out.internalLinkTarget(e)
      case e: BookmarkTree              => out.bookmarkTree(e)
      case e: Bookmark                  => out.bookmark(e)
      case e: PageBreak                 => out.block(e)
      case TargetFormat("xslfo",e,_)    => out << e
      
      case WithFallback(fallback)       => out << fallback
      case unknown                      => ()
    }
    
    def renderSimpleSpan (span: Span): Unit = span match {
      case e @ CitationLink(ref,label,_)  => citations.get(ref).foreach(c => out.footnote(e,label,c.content,c.options))
      case e @ FootnoteLink(ref,label,_)  => footnotes.get(ref).foreach(f => out.footnote(e,label,f.content,f.options))
      case SectionNumber(pos, opt)        => out << Text(pos.mkString(".") + " ", opt + Styles("sectionNumber"))
      case e @ Image(_,uri,width,height,_,_) => out.externalGraphic(e, uri.localRef.fold(uri.uri)(_.absolute.toString), width, height) // TODO - ignoring title for now
      case e: Leader                      => out <<@ ("fo:leader",e,"leader-pattern"->"dots") << "</fo:leader>" 
      case e @ PageNumberCitation(ref,path,_)  => out << s"""<fo:page-number-citation ref-id="${out.buildId(path.absolute, ref)}" />"""
      case LineBreak(_)                   => out << "&#x2028;"
      case TemplateElement(elem,indent,_) => out.indented(indent) { out << elem }
      
      case WithFallback(fallback)         => out << fallback
      case unknown                        => ()
    }
    
    def renderTableElement (elem: TableElement): Unit = elem match {
      case e @ TableHead(rows,_)   => out <<@ ("fo:table-header", e) <<|> rows <<| "</fo:table-header>"
      case e @ TableBody(rows,_)   => out <<@ ("fo:table-body", e) <<|> rows <<| "</fo:table-body>"
      case Caption(_,_)            => () // replaced by Table renderer
      case Columns(_,_)            => () // replaced by Table renderer
      case e: Column               => out <<@ ("fo:table-column",e)
      case e @ Row(cells,_)        => out <<@ ("fo:table-row",e) <<|> cells <<| "</fo:table-row>"
      case e @ Cell(_, content, colspan, rowspan, _) => out <<@ 
            ("fo:table-cell", e, 
                "number-columns-spanned"->noneIfDefault(colspan,1), 
                "number-rows-spanned"->noneIfDefault(rowspan,1)) <<|> content <<| "</fo:table-cell>"
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
  
  override def defaultTheme: Theme[FOWriter] = Theme[FOWriter](
    defaultTemplate = Some(XSLFO.templateResource.content),
    defaultStyles = styles.getOrElse(XSLFO.styleResource)
  )

}

/** The default instance of the XSL-FO renderer.
 */
object XSLFO extends XSLFO(styles = None, renderFormatted = true) {
  
  lazy val styleResource: StyleDeclarationSet = {
    val input = Input.fromClasspath("/styles/default.fo.css", Root / "default.fo.css")
    Parsers.documentParserFunction(CSSParsers.styleDeclarationSet, StyleDeclarationSet.forPath)(input)
  }

  lazy val templateResource: TemplateDocument =
    DefaultTemplateParser.parse(Input.fromClasspath("/templates/default.template.fo", Root / "default.template.fo"))
  
}
