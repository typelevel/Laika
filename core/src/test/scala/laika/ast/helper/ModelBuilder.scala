/*
 * Copyright 2012-2020 the original author or authors.
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

package laika.ast.helper

import laika.ast._

trait ModelBuilder {

  
  def spans (elements: Span*): List[Span] = elements.toList

  def root (blocks: Block*) = RootElement(blocks.toList)

  def t (text: String) = TemplateString(text)

  def p (spans: Span*) = Paragraph(spans.toList)

  def p (text: String) = Paragraph(text)

  def table (rows: Row*) = Table(TableHead(Nil), TableBody(rows.toList))

  def row (cells: Cell*) = Row(cells.toList)

  def cell (content: String, colspan: Int, rowspan: Int) = Cell(BodyCell, List(p(Text(content))), colspan, rowspan)

  def strrow (cells: String*) = Row(cells.map(BodyCell(_)))

  def lb (items: LineBlockItem*) = LineBlock(items.toList)

  def quote (text: String, attribution: String) = QuotedBlock(List(p(text)), List(Text(attribution)))

  def h (level: Int, content: String, id: String) = Header(level, List(Text(content)), Id(id))

  def title (text: String) = Title(Seq(Text(text)), Id(text.replaceAll("[^a-zA-Z0-9-]+","-").replaceFirst("^-","").replaceFirst("-$","").toLowerCase) + Styles("title"))

  def dh (deco: HeaderDecoration, content: String, id: String) = DecoratedHeader(deco, List(Text(content)), Id(id))



  def link (content: Span*): LinkBuilder = new LinkBuilder(content.toList)
  
  class LinkBuilder private[ModelBuilder] (content: List[Span], url: String = "", title: Option[String] = None) {
    
    def url (value: String): LinkBuilder = new LinkBuilder(content, value, title)
    
    def title (value: String): LinkBuilder = new LinkBuilder(content, url, Some(value))
    
    def toLink = ExternalLink(content, url, title)
    
  }
  
  def linkRef (content: Span*): LinkRefBuilder = new LinkRefBuilder(content.toList)
  
  class LinkRefBuilder private[ModelBuilder] (content: List[Span], id: String = "", source: String = "") {
    
    def id (value: String): LinkRefBuilder = new LinkRefBuilder(content, value, source)
    
    def source (value: String): LinkRefBuilder = new LinkRefBuilder(content, id, value)
    
    def toLink = LinkReference(content, id, source)
     
  }
  
  def img (text: String, uri: String, pathInfo: Option[PathInfo] = None, title: Option[String] = None,
           width: Option[Size] = None, height: Option[Size] = None) =
    Image(text, URI(uri, pathInfo), title = title, width = width, height = height)

  def imgRef (text: String, id: String, source: String = "") = ImageReference(text, id, source)
  
  def citRef (label: String) = CitationReference(label, s"[$label]_")
  
  def fnRef (label: FootnoteLabel) = FootnoteReference(label, toSource(label))
  
  def toSource (label: FootnoteLabel): String = label match {
    case Autonumber => "[#]_"
    case Autosymbol => "[*]_"
    case AutonumberLabel(label) => s"[#$label]_"
    case NumericLabel(label) => s"[$label]_"
  }
  
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
  
  
  implicit def builderToEnumList (builder: EnumListBuilder): EnumList = builder.toList

  implicit def builderToBulletList (builder: BulletListBuilder): BulletList = builder.toList

  implicit def builderToDefList (builder: DefinitionListBuilder): DefinitionList = builder.toList

  implicit def builderToLink (builder: LinkBuilder): Link = builder.toLink

  implicit def builderToLinkRef (builder: LinkRefBuilder): LinkReference = builder.toLink
  
  
}
