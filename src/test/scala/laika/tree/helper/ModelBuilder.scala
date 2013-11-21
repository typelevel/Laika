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

package laika.tree.helper

import laika.tree.Elements._
import laika.tree.Templates._
   
trait ModelBuilder {

  
  def spans (elements: Span*) = elements.toList
  
  def em (elements: Span*) = Emphasized(elements.toList)

  def em (text: String) = Emphasized(List(txt(text)))

  def str (elements: Span*) = Strong(elements.toList)
  
  def str (text: String) = Strong(List(txt(text)))
  
  def txt (content: String) = Text(content)
  
  def lit (content: String) = Literal(content)
   
  def link (content: Span*) = new LinkBuilder(content.toList)
  
  class LinkBuilder private[ModelBuilder] (content: List[Span], url0: String = "", title0: Option[String] = None) {
    
    def url (value: String) = new LinkBuilder(content, value, title0)
    
    def title (value: String) = new LinkBuilder(content, url0, Some(value))
    
    def toLink = ExternalLink(content, url0, title0)
    
  }
  
  def linkRef (content: Span*) = new LinkRefBuilder(content.toList)
  
  class LinkRefBuilder private[ModelBuilder] (content: List[Span], id: String = "", source: String = "") {
    
    def id (value: String): LinkRefBuilder = new LinkRefBuilder(content, value, source)
    
    def source (value: String): LinkRefBuilder = new LinkRefBuilder(content, id, value)
    
    def toLink = LinkReference(content, id, source)
     
  }
  
  def img (text: String, url: String, title: Option[String] = None) = Image(text, url, title)

  def imgRef (text: String, id: String, source: String = "") = ImageReference(text, id, source)
  
  def citRef (label: String) = CitationReference(label, "["+label+"]_")
  
  def fnRef (label: FootnoteLabel) = FootnoteReference(label, toSource(label))
  
  def toSource (label: FootnoteLabel) = label match {
    case Autonumber => "[#]_"
    case Autosymbol => "[*]_"
    case AutonumberLabel(label) => "[#"+label+"]_"
    case NumericLabel(label) => "["+label+"]_"
  }
  
  def doc (blocks: Block*) = RootElement(blocks.toList)
  
  def tRoot (spans: TemplateSpan*) = TemplateRoot(spans)
  
  def eRoot (blocks: Block*) = EmbeddedRoot(blocks.toList)

  def tt (text: String) = TemplateString(text)

  def tElem (element: Element) = TemplateElement(element)
  
  def p (spans: Span*) = Paragraph(spans.toList)
  
  def p (text: String) = Paragraph(List(Text(text)))
  
  
  def bulletList (bullet: String = "*") = new BulletListBuilder(bullet)
  
  class BulletListBuilder (bullet: String, items: Seq[BulletListItem] = Nil) {
    
    def + (text: String) = new BulletListBuilder(bullet, items :+ newItem(p(text)))
    
    def + (blocks: Block*) = new BulletListBuilder(bullet, items :+ newItem(blocks:_*))
    
    private def newItem (blocks: Block*) = BulletListItem(blocks.toList, StringBullet(bullet))
    
    def toList = BulletList(items, StringBullet(bullet))
    
  }
  
  def enumList (format: EnumFormat = EnumFormat(), start: Int = 1) = new EnumListBuilder(format,  start)
  
  class EnumListBuilder (format: EnumFormat, start: Int, items: Seq[EnumListItem] = Nil) {
    
    def + (text: String) = new EnumListBuilder(format, start + 1, items :+ newItem(p(text)))
    
    def + (blocks: Block*) = new EnumListBuilder(format, start + 1, items :+ newItem(blocks:_*))
    
    private def newItem (blocks: Block*) = EnumListItem(blocks.toList, format, start)
    
    def toList = EnumList(items, format, items.headOption.map(_.position).getOrElse(1))
    
  }
  
  def defList = new DefinitionListBuilder
  
  class DefinitionListBuilder (items: Seq[DefinitionListItem] = Nil) {
    
    def + (term: String, blocks: Block*) = new DefinitionListBuilder(items :+ newItem(List(Text(term)), blocks:_*))
    
    def + (term: List[Span], blocks: Block*) = new DefinitionListBuilder(items :+ newItem(term, blocks:_*))
    
    private def newItem (term: List[Span], blocks: Block*) = DefinitionListItem(term, blocks.toList)
    
    def toList = DefinitionList(items.toList)
    
  }
  
  
  def table (rows: Row*) = Table(TableHead(Nil), TableBody(rows.toList))
  
  def row (cells: Cell*) = Row(cells.toList)
  
  def cell (content: String, colspan: Int, rowspan: Int) = Cell(BodyCell, List(p(txt(content))), colspan, rowspan)
  
  def cell (content: String): Cell = cell(p(txt(content)))
  
  def cell (content: Block*): Cell = Cell(BodyCell, content.toList)
  
  def strrow (cells: String*) = Row(cells map cell)
  
  
  def lb (items: LineBlockItem*) = LineBlock(items.toList)
  
  def line (text: String) = Line(List(Text(text)))

  def line (spans: Span*) = Line(spans.toList)
  
  
  def quote (items: Block*) = QuotedBlock(items.toList, Nil)
  
  def quote (text: String) = QuotedBlock(List(p(text)), Nil) 

  def quote (text: String, attribution: String) = QuotedBlock(List(p(text)), List(txt(attribution))) 
  
  
  def litBlock (content: String) = LiteralBlock(content)
  
  
  def h (level: Int, content: Span*) = Header(level, content.toList)

  def h (level: Int, content: String) = Header(level, List(txt(content)))

  def h (level: Int, content: String, id: String) = Header(level, List(txt(content)), Id(id))

  def dh (deco: HeaderDecoration, content: String, id: String) = DecoratedHeader(deco, List(txt(content)), Id(id))
  
  
  
  implicit def builderToEnumList (builder: EnumListBuilder) = builder.toList

  implicit def builderToBulletList (builder: BulletListBuilder) = builder.toList

  implicit def builderToDefList (builder: DefinitionListBuilder) = builder.toList

  implicit def builderToLink (builder: LinkBuilder) = builder.toLink

  implicit def builderToLinkRef (builder: LinkRefBuilder) = builder.toLink
  
  
}