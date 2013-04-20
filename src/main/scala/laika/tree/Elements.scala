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

package laika.tree
 
import laika.api.Render
import laika.render.PrettyPrint
import scala.math.Ordered

/** Provides the elements of the document tree. The model is generic and not tied to any
 *  specific markup syntax like Markdown. Parsers may only support a subset of the provided
 *  element types in case the markup does not have matching syntax for some of them.
 *  
 *  The abstract base classes are not sealed as the tree model is extensible.
 *  Renderers should anticipate unknown elements and add fallback rules for those.
 *  
 *  The base class is `Element` which extends `Product`, a constraint usually satisfied
 *  through defining the concrete types as case classes. Most concrete types are not expected
 *  to extend `Element` directly though, but instead extend either `Block` or `Span`, the two
 *  major element types. This way they may be part of the content of `SpanContainer` or
 *  `BlockContainer` types, traits that any element may mix in.
 * 
 *  @author Jens Halm
 */
object Elements {


  /** The base class for all Elements forming the document tree.
   *  Usually not extended directly, instead either `Span` or 
   *  `Block` should be picked as the base type for new element
   *  types.
   */
  abstract class Element extends Product
  
  /** The base type for all block level elements.
   */
  trait Block extends Element

  /** The base type for all inline elements.
   */
  trait Span extends Element
  
  /** The base type for all list items.
   */
  trait ListItem extends Element
  
  /** The base type for all reference elements.
   * 
   *  A reference points to some other node in the document tree and needs
   *  to be resolved and replaced by a rewrite rule before rendering. 
   *  Therefore none of the available renderers include logic for 
   *  dealing with references.
   */
  trait Reference extends Span {
    def source: String
  }
  
  /** The base type for all link elements.
   * 
   *  In contrast to the reference type, it is only mixed in by
   *  elements representing resolved links that can be dealt
   *  with by renderers.
   */
  trait Link extends Span

  /** A generic container.
   *  Usually not mixed in directly, instead one of the sub-traits
   *  `TextContainer`, `ListContainer`, `SpanContainer` or `BlockContainer` should be used.
   */
  trait Container[+T] extends Element {
    def content: T
  }
  
  /** A container for plain text.
   */
  trait TextContainer extends Container[String]

  /** A generic container of other elements.
   *  Provides means to traverse, select and rewrite children of
   *  this container.
   *  
   *  Usually not mixed in directly, instead one of the sub-traits
   *  `ListContainer`, `SpanContainer` or `BlockContainer` should be used.
   */
  trait ElementContainer[+E <: Element, Self <: ElementContainer[E,Self]] extends Container[Seq[E]] with ElementTraversal[Self] {
    override def toString = "\n" + (Render as PrettyPrint from this toString) + "\n" 
  }

  /** A container of other Block elements. Such a container is usually
   *  also a Block itself.
   */
  trait BlockContainer[Self <: BlockContainer[Self]] extends ElementContainer[Block,Self]
  
  /** A container of other Span elements. Such a container may be a Block
   *  or a Span itself.
   */
  trait SpanContainer[Self <: SpanContainer[Self]] extends ElementContainer[Span,Self]
  
  /** A container of list items. Such a container is usually a Block itself.
   */
  trait ListContainer[Self <: ListContainer[Self]] extends ElementContainer[ListItem,Self]
  
  
  /** The root element of a document tree.
   */
  case class Document (content: Seq[Block]) extends Element with BlockContainer[Document]
  
  
  /** A section of the document, consisting of a header and content in the form
   *  of a list of Block elements. Sections may be nested inside other sections,
   *  they are arranged in a hierarchy based on the level of their header element.
   */
  case class Section (header: Header, content: Seq[Block]) extends Block with BlockContainer[Section]

  /** A header element with a level 
   * 
   */
  case class Header (level: Int, content: Seq[Span]) extends Block with SpanContainer[Header]
   
  
  /** A generic container element containing a list of blocks. Can be used where a sequence
   *  of blocks must be inserted in a place where a single element is required by the API.
   *  Usually renderers do not treat the container as a special element and render its children
   *  as s sub flow of the parent container.
   */
  case class BlockSequence (content: Seq[Block]) extends Block with BlockContainer[BlockSequence]
  
  /** A generic container element containing a list of spans. Can be used where a sequence
   *  of spans must be inserted in a place where a single element is required by the API.
   *  Usually renderers do not treat the container as a special element and render its children
   *  as s sub flow of the parent container. A span sequence is special in that in can be
   *  used as both a span and a block.
   */
  case class SpanSequence (content: Seq[Span]) extends Block with Span with SpanContainer[SpanSequence]

  
  /** A paragraph consisting of span elements.
   */
  case class Paragraph (content: Seq[Span]) extends Block with SpanContainer[Paragraph]
    
  /** A literal block with simple text content.
   */
  case class LiteralBlock (content: String) extends Block with TextContainer

  /** A quoted block consisting of a list of blocks that may contain other
   *  nested quoted blocks and an attribution which may be empty.
   */
  case class QuotedBlock (content: Seq[Block], attribution: Seq[Span]) extends Block with BlockContainer[QuotedBlock]

  /** An bullet list that may contain nested lists.
   */
  case class BulletList (content: Seq[ListItem], format: BulletFormat) extends Block with ListContainer[BulletList]
  
  /** An enumerated list that may contain nested lists.
   */
  case class EnumList (content: Seq[ListItem], format: EnumFormat, start: Int = 1) extends Block with ListContainer[EnumList]
  
  /** The format of a bullet list item.
   */
  trait BulletFormat

  /** Bullet format based on a simple string.
   */
  case class StringBullet (bullet: String) extends BulletFormat
  
  /** The format of enumerated list items.
   */
  case class EnumFormat (enumType: EnumType = Arabic, prefix: String = "", suffix: String = ".") {
    override def toString = "EnumFormat(" + enumType + "," + prefix + "N" + suffix + ")"
  }
  
  /** Represents the type of an ordered list.
   */
  sealed abstract class EnumType
  
  /** Arabic enumeration style (1, 2, 3...)
   */
  case object Arabic extends EnumType
  
  /** Lowercase letter enumeration style (a, b, c...)
   */
  case object LowerAlpha extends EnumType
  
  /** Uppercase letter enumeration style (A, B, C...)
   */
  case object UpperAlpha extends EnumType
  
  /** Lowercase Roman numeral enumeration style (i, ii, iii, iv...)
   */
  case object LowerRoman extends EnumType
  
  /** Uppercase Roman numeral enumeration style (I, II, III, IV...)
   */
  case object UpperRoman extends EnumType
    
  /** A single bullet list item consisting of one or more block elements.
   */
  case class BulletListItem (content: Seq[Block], format: BulletFormat) extends ListItem with BlockContainer[BulletListItem]
  
  /** A single enum list item consisting of one or more block elements.
   */
  case class EnumListItem (content: Seq[Block], format: EnumFormat, position: Int) extends ListItem with BlockContainer[EnumListItem]
  
  /** A list of terms and their definitions.
   */
  case class DefinitionList (content: Seq[DefinitionListItem]) extends Block with ListContainer[DefinitionList]

  /** A single definition item, containing the term and definition (as the content property).
   */
  case class DefinitionListItem (term: Seq[Span], content: Seq[Block]) 
    extends ListItem with BlockContainer[DefinitionListItem]
  
  /** A single item inside a line block.
   */
  abstract class LineBlockItem extends Block
  
  /** A single line inside a line block.
   */
  case class Line (content: Seq[Span]) extends LineBlockItem with SpanContainer[Line]
  
  /** A block containing lines which preserve line breaks and optionally nested line blocks.
   */
  case class LineBlock (content: Seq[LineBlockItem]) extends LineBlockItem with BlockContainer[LineBlock]
  
  /** A table consisting of a head and a body part represented by a sequence of rows.  
   *  Both the head and body sequence may be empty.
   */
  case class Table (head: Seq[Row], content: Seq[Row]) extends Block with ElementContainer[Row,Table]
  
  /** A single table row. In case some of the previous rows contain
   *  cells with a colspan greater than 1, this row may contain
   *  fewer cells than the number of columns in this table.
   */
  case class Row (content: Seq[Cell]) extends Element with ElementContainer[Cell,Row]
  
  /** A single cell, potentially spanning multiple rows or columns, containing
   *  one or more block elements.
   */
  case class Cell (cellType: CellType, content: Seq[Block], colspan: Int = 1, rowspan: Int = 1) extends Element with BlockContainer[Cell]

  /** The cell type specifies which part of the table the cell belongs to. 
   */
  sealed abstract class CellType
  
  /** A cell in the head part of the table.
   */
  case object HeadCell extends CellType

  /** A cell in the body part of the table.
   */
  case object BodyCell extends CellType
  
  /** Represents any kind of link target, mixed in by
   *  all the concrete target types.
   */
  trait LinkTarget extends Block with Span {
    def id: String
  }
  
  /** An external link target, usually only part of the raw document tree and then
   *  removed by the rewrite rule that resolves link and image references.
   */
  case class ExternalLinkTarget (id: String, url: String, title: Option[String] = None) extends LinkTarget
  
  /** Points to the following block or span element, making it a target for links.
   */
  case class InternalLinkTarget (id: String) extends LinkTarget
  
  /** A citation consisting of a label and one or more block elements.
   */
  case class Citation (label: String, content: Seq[Block]) extends Block with BlockContainer[Footnote]
  
  /** A footnote consisting of a label and one or more block elements.
   */
  case class Footnote (label: FootnoteLabel, content: Seq[Block]) extends Block with BlockContainer[Footnote]
  
  /** Base type for all types of footnote labels.
   */
  abstract class FootnoteLabel
  
  /** Label with automatic numbering.
   */
  case object Autonumber extends FootnoteLabel

  /** Label with automatic symbol assignment.
   */
  case object Autosymbol extends FootnoteLabel

  /** Explicit numeric label.
   */
  case class NumericLabel (number: Int) extends FootnoteLabel

  /** Label using automatic numbering and explicit label names together.
   */
  case class AutonumberLabel (label: String) extends FootnoteLabel
  
  /** Fully resolved label containing the id to use for referencing as well as
   *  the label to use for display.
   */
  case class ResolvedFootnoteLabel (id: String, label: String) extends FootnoteLabel
  
  /** A horizontal rule.
   */
  case object Rule extends Block
  
  
  
  /** A simple text element.
   */
  case class Text (content: String) extends Span with TextContainer

  /** A span of emphasized inline elements that may contain nested spans.
   */
  case class Emphasized (content: Seq[Span]) extends Span with SpanContainer[Emphasized]
  
  /** A span of strong inline elements that may contain nested spans.
   */
  case class Strong (content: Seq[Span]) extends Span with SpanContainer[Strong]
    
  /** A span containing plain, unparsed text.
   */
  case class Literal (content: String) extends Span with TextContainer
  
  
  /** An external link element, with the span content representing the text (description) of the link.
   */
  case class ExternalLink (content: Seq[Span], url: String, title: Option[String] = None) extends Link with SpanContainer[ExternalLink]

  /** A internal link element, with the span content representing the text (description) of the link.
   */
  case class InternalLink (content: Seq[Span], url: String, title: Option[String] = None) extends Link with SpanContainer[InternalLink]
  
  /** A resolved link to a footnote.
   */
  case class FootnoteLink (id: String, label: String) extends Link

  /** A resolved link to a citation.
   */
  case class CitationLink (label: String) extends Link
  
  /** An inline image with a text description and optional title.
   */
  case class Image (text: String, url: String, title: Option[String] = None) extends Link

 
  /** A link reference, the id pointing to the id of a `LinkTarget`. Only part of the
   *  raw document tree and then removed by the rewrite rule that resolves link and image references.
   */
  case class LinkReference (content: Seq[Span], id: String, source: String) extends Reference with SpanContainer[LinkReference]
  
  /** An image reference, the id pointing to the id of a `LinkTarget`. Only part of the
   *  raw document tree and then removed by the rewrite rule that resolves link and image references.
   */
  case class ImageReference (text: String, id: String, source: String) extends Reference
  
  /** A reference to a footnote with a matching label.  Only part of the
   *  raw document tree and then removed by the rewrite rule that resolves link and image references.
   */
  case class FootnoteReference (label: FootnoteLabel, source: String) extends Reference

  /** A reference to a citation with a matching label.  Only part of the
   *  raw document tree and then removed by the rewrite rule that resolves link and image references.
   */
  case class CitationReference (label: String, source: String) extends Reference

  

  
  /** An explicit hard line break.
   */
  case object LineBreak extends Span
  
  /** A comment that may be omitted by renderers.
   */
  case class Comment (content: String) extends Block with TextContainer
  
  /** Message generated by the parser, usually to signal potential parsing problems.
   *  They usually get inserted immediately after the block or span that caused the problem.
   *  It mixes in both the Span and Block trait so that it can appear in sequences of both types. 
   *  By default messages are ignored by most renderers (apart from PrettyPrint), but
   *  they can be explicitly activated for a particular level.
   */
  case class SystemMessage (level: MessageLevel, content: String) extends Span with Block with TextContainer
  
  /** Signals the severity of a system message.
   */
  sealed abstract class MessageLevel (private val level: Int) extends Ordered[MessageLevel] {
    def compare(that: MessageLevel): Int = level compare that.level
  }
  
  /** Debugging information that does not have any effect on the parser result.
   */
  case object Debug extends MessageLevel(0)
  
  /** A minor issue that has very little or no effect on the parser result.
   */
  case object Info extends MessageLevel(1)
  
  /** An issue that should be addressed, if ignored, there may be minor problems with the output.
   */
  case object Warning extends MessageLevel(2)
  
  /** A major issue that should be addressed, if ignored, the output will contain unpredictable errors.
   */
  case object Error extends MessageLevel(3)
  
  /** A critical error that must be addressed, if ignored, the output will contain severe errors.
   */
  case object Fatal extends MessageLevel(4)
  
  /** Groups a span that could not successfully parsed with a system message.
   *  Renderers may then choose to just render the fallback, the message or both.
   */
  case class InvalidSpan (message: SystemMessage, fallback: Span) extends Span

  /** Groups a block that could not successfully parsed with a system message.
   *  Renderers may then choose to just render the fallback, the message or both.
   */
  case class InvalidBlock (message: SystemMessage, fallback: Block) extends Block
  
  
}