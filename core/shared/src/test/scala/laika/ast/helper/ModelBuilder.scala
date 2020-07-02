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
import laika.config.{Config, ConfigParser}

trait ModelBuilder {

  
  def spans (elements: Span*): List[Span] = elements.toList

  def root (blocks: Block*): RootElement = RootElement(blocks.toList)

  def t (text: String): TemplateString = TemplateString(text)

  def p (spans: Span*): Paragraph = Paragraph(spans.toList)

  def p (text: String): Paragraph = Paragraph(text)

  def cell (content: String, colspan: Int, rowspan: Int) = Cell(BodyCell, List(p(Text(content))), colspan, rowspan)

  def quote (text: String, attribution: String): QuotedBlock = QuotedBlock(List(p(text)), List(Text(attribution)))

  def titleWithId (text: String): Title = Title(Seq(Text(text)), Id(text.replaceAll("[^a-zA-Z0-9-]+","-").replaceFirst("^-","").replaceFirst("-$","").toLowerCase) + Style.title)


  def link (content: Span*): LinkBuilder = new LinkBuilder(content.toList)
  
  class LinkBuilder private[ModelBuilder] (content: List[Span], url: String = "", title: Option[String] = None) {
    
    def url (value: String): LinkBuilder = new LinkBuilder(content, value, title)
    
    def title (value: String): LinkBuilder = new LinkBuilder(content, url, Some(value))
    
    def toLink = SpanLink(content, ExternalTarget(url), title)
    
  }
  
  def linkRef (content: Span*): LinkRefBuilder = new LinkRefBuilder(content.toList)
  
  class LinkRefBuilder private[ModelBuilder] (content: List[Span], id: String = "", source: String = "") {
    
    def id (value: String): LinkRefBuilder = new LinkRefBuilder(content, value, source)
    
    def source (value: String): LinkRefBuilder = new LinkRefBuilder(content, id, value)
    
    def toLink = LinkIdReference(content, id, source)
     
  }
  
  def imgRef (text: String, id: String, source: String = "") = ImageIdReference(text, id, source)
  
  def citRef (label: String) = CitationReference(label, s"[$label]_")
  
  def fnRef (label: FootnoteLabel) = FootnoteReference(label, toSource(label))
  
  def toSource (label: FootnoteLabel): String = label match {
    case Autonumber => "[#]_"
    case Autosymbol => "[*]_"
    case AutonumberLabel(label) => s"[#$label]_"
    case NumericLabel(label) => s"[$label]_"
  }
  
  private val defaultBullet = StringBullet("*")
  
  def bulletList(format: BulletFormat)(textItems: String*): BulletList =
    BulletList(textItems.map(txt => BulletListItem(Seq(p(txt)), format)), format)

  def bulletList(textItem: String, textItems: String*): BulletList = bulletList(defaultBullet)(textItem +: textItems:_*)

  def bulletList(blocks: Block*): BulletList =
    BulletList(blocks.map(b => BulletListItem(Seq(b), defaultBullet)), defaultBullet)

  def enumList (textItem: String, textItems: String*): EnumList = enumList(EnumFormat())(textItem +: textItems:_*)

  def enumList (format: EnumFormat, start: Int = 1)(textItems: String*): EnumList =
    EnumList(textItems.zipWithIndex.map { case (txt, index) => 
      EnumListItem(Seq(p(txt)), format, start + index) 
    }, format, start)

  def enumList (blocks: Block*): EnumList =
    EnumList(blocks.zipWithIndex.map { case (block, index) =>
      EnumListItem(Seq(block), EnumFormat(), 1 + index)
    }, EnumFormat(), 1)
  
  def defListItem(term: String, blocks: Block*): DefinitionListItem = 
    DefinitionListItem(List(Text(term)), blocks.toList)
  
  val disableInternalLinkValidation: Config = 
    ConfigParser.parse("""{ laika.links.excludeFromValidation = ["/"]}""").resolve().toOption.get
  
  
  implicit def builderToLink (builder: LinkBuilder): Link = builder.toLink

  implicit def builderToLinkRef (builder: LinkRefBuilder): LinkIdReference = builder.toLink
  
  
}
