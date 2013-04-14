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
  

  /** A generic container.
   *  Usually not mixed in directly, instead one of the sub-traits
   *  `TextContainer`, `SpanContainer` or `BlockContainer` should be used.
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
   *  `SpanContainer` or `BlockContainer` should be used.
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
   
  /** A generic flow content element containing a list of spans. Structurally similar
   *  to a paragraph, but semantically an element that does not necessarily represent a 
   *  full paragraph. Used inside simple list items for example, but usually not as
   *  a top level block element. It is also special in that it mixes in both the Span
   *  and Block trait, making it a hybrid element that can be used anywhere a SpanContainer
   *  is needed.
   */
  case class FlowContent (content: Seq[Span]) extends Block with Span with SpanContainer[FlowContent]
  
  /** A generic container element containing a list of blocks. Can be used where a sequence
   *  of blocks must be inserted in a place where a single element is required by the API for example.
   *  Usually renderers do not treat the container as a special element and render its children
   *  as s sub flow of the parent container.
   */
  case class BlockSequence (content: Seq[Block]) extends Block with BlockContainer[BlockSequence]
  
  /** A paragraph consisting of span elements.
   */
  case class Paragraph (content: Seq[Span]) extends Block with SpanContainer[Paragraph]
    
  /** A code block with simple text content.
   */
  case class CodeBlock (content: String) extends Block with TextContainer

  /** A quoted block consisting of a list of blocks that may contain other
   *  nested quoted blocks and an attribution which may be empty.
   */
  case class QuotedBlock (content: Seq[Block], attribution: Seq[Span]) extends Block with BlockContainer[QuotedBlock]

  /** An unordered list of block level items that may contain nested lists.
   */
  case class UnorderedList (content: Seq[ListItem]) extends Block with BlockContainer[UnorderedList]
  
  /** An ordered list of block level items that may contain nested lists.
   */
  case class OrderedList (content: Seq[ListItem], enumType: EnumType = Arabic, 
      prefix: String = "", suffix: String = ".", start: Int = 1) extends Block with BlockContainer[OrderedList]
  
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
    
  /** A single list item consisting of one or more block elements.
   */
  case class ListItem (content: Seq[Block]) extends Block with BlockContainer[ListItem]
  
  /** A list of terms and their definitions.
   */
  case class DefinitionList (content: Seq[DefinitionListItem]) extends Block with BlockContainer[DefinitionList]

  /** A single definition item, containing the term and definition (as the content property).
   */
  case class DefinitionListItem (term: Seq[Span], content: Seq[Block]) 
    extends Block with BlockContainer[DefinitionListItem]
  
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
  case class Table (head: Seq[Row], body: Seq[Row]) extends Block
  
  /** A single table row. In case some of the previous rows contain
   *  cells with a colspan greater than 1, this row may contain
   *  fewer cells than the number of columns in this table.
   */
  case class Row (cells: Seq[Cell]) extends Element
  
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
  
  /** A link definition, usually only part of the raw document tree and then
   *  removed by the rewrite rule that resolves link and image references.
   */
  case class LinkDefinition (id: String, url: String, title: Option[String] = None) extends LinkTarget
  
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
    
  /** A code span containing plain text.
   */
  case class CodeSpan (content: String) extends Span with TextContainer
  
  /** A link element, with the span content representing the text (description) of the link.
   */
  case class Link (content: Seq[Span], url: String, title: Option[String] = None) extends Span with SpanContainer[Link]

  /** A link reference, the id pointing to the id of a `LinkDefinition`. Usually only part of the
   *  raw document tree and then removed by the rewrite rule that resolves link and image references.
   *  But an unresolvable reference may be left in the tree to leave the decision how to deal with it
   *  to the renderer which should use the `inputPrefix` and `inputPostfix` attributes to enclose
   *  the content in the original markup.
   */
  case class LinkReference (content: Seq[Span], id: String, inputPrefix: String, inputPostfix: String) extends Span with SpanContainer[LinkReference]
  
  /** An inline image with a text description and optional title.
   */
  case class Image (text: String, url: String, title: Option[String] = None) extends Span
  
  /** An image reference, the id pointing to the id of a `LinkDefinition`. Usually only part of the
   *  raw document tree and then removed by the rewrite rule that resolves link and image references.
   *  But an unresolvable reference may be left in the tree to leave the decision how to deal with it
   *  to the renderer which should use the `inputPrefix` and `inputPostfix` attributes to enclose
   *  the text in the original markup.
   */
  case class ImageReference (text: String, id: String, inputPrefix: String, inputPostfix: String) extends Span
  
  /** A reference to a footnote with a matching label.
   */
  case class FootnoteReference (label: FootnoteLabel) extends Span

  /** A reference to a citation with a matching label.
   */
  case class CitationReference (label: String) extends Span
  
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