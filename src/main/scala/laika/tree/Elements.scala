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
import laika.tree.Documents.Path

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
  
  /** An element that can be customized. Represents options
   *  that are usually only used on few selected nodes and
   *  can control subtle differences often only relevant
   *  for renderers.
   */
  trait Customizable extends Element {
    def options: Options
  }
  
  /** Options for customizable elements.
   */
  sealed abstract class Options {
    /** The id of this element. Has to be unique
     *  across all element types of a document,
     *  including the ids of `LinkTarget` instances.
     */
    def id: Option[String]
    /** Style names that may have an influence
     *  on rendering of this element.
     */
    def styles: Seq[String]
    /** Merges these options with the specified
     *  options. If the id has been set in both 
     *  instances, the other instance overrides 
     *  this one.
     */
    def + (other: Options): Options
  }
  
  /** Provides a fallback for elements
   *  the renderer does not know how
   *  to deal with.
   */
  trait Fallback {
    /** Defines a fallback for this element in 
     *  case the renderer does not know how
     *  to deal with it.
     */
    def fallback: Element
  }
  
  /** The base type for all block level elements.
   */
  trait Block extends Customizable

  /** The base type for all inline elements.
   */
  trait Span extends Customizable
  
  /** The base type for all list items.
   */
  trait ListItem extends Customizable
  
  /** Represents a temporary element only present in the
   *  raw document tree and then removed or replaced
   *  by a rewrite rule before rendering.
   */
  trait Temporary extends Element
  
  /** Represents an invalid element. Renderers
   *  can choose to either render the fallback
   *  or the system message or both.
   */
  trait Invalid[+E <: Element] extends Element {
    def message: SystemMessage
    def fallback: E
  }
  
  /** The base type for all reference elements.
   * 
   *  A reference points to some other node in the document tree and needs
   *  to be resolved and replaced by a rewrite rule before rendering. 
   *  Therefore none of the available renderers include logic for 
   *  dealing with references.
   */
  trait Reference extends Span with Temporary {
    def source: String
  }
  
  /** Represents a definition that can be used to resolve references.
   * 
   *  Only part of the raw document tree and then removed or replaced
   *  by a rewrite rule before rendering.
   */
  trait Definition extends Block with Temporary

  /** The base type for all link elements.
   * 
   *  In contrast to the reference type, it is only mixed in by
   *  elements representing resolved links that can be dealt
   *  with by renderers.
   */
  trait Link extends Span
  
  /** The base type for all link targets. The id has to be
   *  unique for the whole document across all types
   *  of `LinkTarget` implementations.
   */
  trait LinkTarget extends Customizable
  

  
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
  case class RootElement (content: Seq[Block]) extends Element with BlockContainer[RootElement]
  
  /** A named document fragment that usually gets rendered separately from the main root element
   */ 
  case class DocumentFragment (name: String, root: Block, options: Options = NoOpt) extends Block with Temporary
  
  /** A section of the document, consisting of a header and content in the form
   *  of a list of Block elements. Sections may be nested inside other sections,
   *  they are arranged in a hierarchy based on the level of their header element.
   */
  case class Section (header: Header, content: Seq[Block], options: Options = NoOpt) extends Block with BlockContainer[Section]

  /** A header element with a level, with 1 being the top level of the document. 
   */
  case class Header (level: Int, content: Seq[Span], options: Options = NoOpt) extends Block with SpanContainer[Header]
  
  /** A decorated header where the level gets determined in the rewrite phase based
   *  on the decoration used and the order they appear in the document. The first
   *  decoration type encountered is used for level 1, the second for level 2, and
   *  so on.
   */
  case class DecoratedHeader (decoration: HeaderDecoration, content: Seq[Span], options: Options = NoOpt) extends Block 
                                                                                                          with Temporary 
                                                                                                          with SpanContainer[DecoratedHeader]
  /** Represents the decoration of a header.
   *  Concrete implementations need to be provided by the parser.
   */
  trait HeaderDecoration
  
  
  /** A generic container element containing a list of blocks. Can be used where a sequence
   *  of blocks must be inserted in a place where a single element is required by the API.
   *  Usually renderers do not treat the container as a special element and render its children
   *  as s sub flow of the parent container.
   */
  case class BlockSequence (content: Seq[Block], options: Options = NoOpt) extends Block with BlockContainer[BlockSequence]
  
  /** A generic container element containing a list of spans. Can be used where a sequence
   *  of spans must be inserted in a place where a single element is required by the API.
   *  Usually renderers do not treat the container as a special element and render its children
   *  as s sub flow of the parent container. A span sequence is special in that in can be
   *  used as both a span and a block.
   */
  case class SpanSequence (content: Seq[Span], options: Options = NoOpt) extends Block with Span with SpanContainer[SpanSequence]

  
  /** A paragraph consisting of span elements.
   */
  case class Paragraph (content: Seq[Span], options: Options = NoOpt) extends Block with SpanContainer[Paragraph]
    
  /** A literal block with unparsed text content.
   */
  case class LiteralBlock (content: String, options: Options = NoOpt) extends Block with TextContainer
  
  /** A literal block with parsed text content.
   */
  case class ParsedLiteralBlock (content: Seq[Span], options: Options = NoOpt) extends Block with SpanContainer[ParsedLiteralBlock]
  
  /** A block of program code. The content is a sequence of spans to support
   *  the later integration of syntax highlighting systems. Without this support
   *  the sequence will only consist of a single `Text` element.
   */
  case class CodeBlock (language: String, content: Seq[Span], options: Options = NoOpt) extends Block with SpanContainer[CodeBlock]

  /** A quoted block consisting of a list of blocks that may contain other
   *  nested quoted blocks and an attribution which may be empty.
   */
  case class QuotedBlock (content: Seq[Block], attribution: Seq[Span], options: Options = NoOpt) extends Block 
                                                                                                 with BlockContainer[QuotedBlock]
  
  /** Generic block element with a title.
   *  Often combined with the the `styles` attribute of the `options` parameter to provide
   *  additional render hints.
   */
  case class TitledBlock (title: Seq[Span], content: Seq[Block], options: Options = NoOpt) extends Block
                                                                                           with BlockContainer[TitledBlock]

  /** A figure consists of an image, an optional caption, and an optional legend as the `content` property.
   *  The `image` property is of type `Span` as the image might be wrapped inside a link reference.
   */
  case class Figure (image: Span, caption: Seq[Span], content: Seq[Block], options: Options = NoOpt) extends Block with BlockContainer[Figure]
  
  /** A bullet list that may contain nested lists.
   */
  case class BulletList (content: Seq[ListItem], format: BulletFormat, options: Options = NoOpt) extends Block 
                                                                                                 with ListContainer[BulletList]
  
  /** An enumerated list that may contain nested lists.
   */
  case class EnumList (content: Seq[ListItem], format: EnumFormat, start: Int = 1, options: Options = NoOpt) extends Block 
                                                                                                             with ListContainer[EnumList]
  
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
  case class BulletListItem (content: Seq[Block], format: BulletFormat, options: Options = NoOpt) extends ListItem 
                                                                                                  with BlockContainer[BulletListItem]
  
  /** A single enum list item consisting of one or more block elements.
   */
  case class EnumListItem (content: Seq[Block], format: EnumFormat, position: Int, options: Options = NoOpt) extends ListItem 
                                                                                                      with BlockContainer[EnumListItem]
  
  /** A list of terms and their definitions.
   *  Not related to the `Definition` base trait.
   */
  case class DefinitionList (content: Seq[DefinitionListItem], options: Options = NoOpt) extends Block with ListContainer[DefinitionList]

  /** A single definition item, containing the term and definition (as the content property).
   */
  case class DefinitionListItem (term: Seq[Span], content: Seq[Block], options: Options = NoOpt) extends ListItem 
                                                                                                 with BlockContainer[DefinitionListItem]
  
  /** A single item inside a line block.
   */
  abstract class LineBlockItem extends Block
  
  /** A single line inside a line block.
   */
  case class Line (content: Seq[Span], options: Options = NoOpt) extends LineBlockItem with SpanContainer[Line]
  
  /** A block containing lines which preserve line breaks and optionally nested line blocks.
   */
  case class LineBlock (content: Seq[LineBlockItem], options: Options = NoOpt) extends LineBlockItem with BlockContainer[LineBlock]
  
  
  /** A table consisting of a head and a body part and optional caption and column specification.  
   */
  case class Table (head: TableHead, body: TableBody, caption: Caption = Caption(), columns: Columns = Columns(Nil), options: Options = NoOpt) extends Block
                                                                                                                 with ElementTraversal[Table] {
    override def toString = "\n" + (Render as PrettyPrint from this toString) + "\n" 
  }
                                                                                                              
  /** A table element, like a row, cell or column.
   */
  trait TableElement extends Customizable       
  
  /** A container of table elements.
   */
  trait TableContainer[Self <: TableContainer[Self]] extends TableElement with ElementContainer[TableElement,Self]

  /** Contains the header rows of a table. 
   */
  case class TableHead (content: Seq[Row], options: Options = NoOpt) extends TableElement with TableContainer[TableHead]

  /** Contains the body rows of a table. 
   */
  case class TableBody (content: Seq[Row], options: Options = NoOpt) extends TableElement with TableContainer[TableBody]
  
  /** The table caption.
   */
  case class Caption (content: Seq[Span] = Nil, options: Options = NoOpt) extends TableElement with SpanContainer[Caption]
  
  /** Contains the (optional) column specification of a table.
   */
  case class Columns (content: Seq[Column], options: Options = NoOpt) extends TableElement with TableContainer[Columns]

  /** Convenient factory for creating a `Columns` instance based on the options
   *  for the individual columns.
   */
  object Columns {
    def options (options: Options*): Columns = Columns(options map Column)
  }

  /** The options (like styles) for a column table.
   */
  case class Column (options: Options = NoOpt) extends TableElement
  
  /** A single table row. In case some of the previous rows contain
   *  cells with a colspan greater than 1, this row may contain
   *  fewer cells than the number of columns in the table.
   */
  case class Row (content: Seq[Cell], options: Options = NoOpt) extends TableElement with TableContainer[Row]
  
  /** A single cell, potentially spanning multiple rows or columns, containing
   *  one or more block elements.
   */
  case class Cell (cellType: CellType, content: Seq[Block], colspan: Int = 1, rowspan: Int = 1, options: Options = NoOpt) extends TableElement 
                                                                                                                          with BlockContainer[Cell]

  /** The cell type specifies which part of the table the cell belongs to. 
   */
  sealed abstract class CellType
  
  /** A cell in the head part of the table.
   */
  case object HeadCell extends CellType

  /** A cell in the body part of the table.
   */
  case object BodyCell extends CellType
  
  
  /** An external link target, usually only part of the raw document tree and then
   *  removed by the rewrite rule that resolves link and image references.
   */
  case class ExternalLinkDefinition (id: String, url: String, title: Option[String] = None, options: Options = NoOpt) extends Definition 
                                                                                                                      with Span
                                                                                                                      
  /** A link target pointing to another link target, acting like an alias.
   */
  case class LinkAlias (id: String, target: String, options: Options = NoOpt) extends Definition with Span                                                                                                                    
  
  /** A footnote definition that needs to be resolved to a final footnote 
   *  by a rewrite rule based on the label type.
   */
  case class FootnoteDefinition (label: FootnoteLabel, content: Seq[Block], options: Options = NoOpt) extends Definition 
                                                                                                      with BlockContainer[Footnote]

  
  /** Points to the following block or span element, making it a target for links.
   */
  case class InternalLinkTarget (options: Options = NoOpt) extends Block with Span with LinkTarget
  
  /** A citation that can be referred to by a `CitationLink` by id.
   */
  case class Citation (label: String, content: Seq[Block], options: Options = NoOpt) extends Block 
                                                                                  with LinkTarget 
                                                                                  with BlockContainer[Footnote]
  
  /** A footnote with resolved id and label that can be referred to by a `FootnoteLink` by id.
   */
  case class Footnote (label: String, content: Seq[Block], options: Options = NoOpt) extends Block 
                                                                                                 with LinkTarget 
                                                                                                 with BlockContainer[Footnote]
  
  
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

  
  /** Raw content that is usually specific to the specified output formats.
   *  Can be used as both block and inline element. If supported by a
   *  parser it usually has to be explicitly enabled due to security concerns.
   */
  case class RawContent (formats: Seq[String], content: String, options: Options = NoOpt) extends Block with Span with TextContainer
  
  /** A horizontal rule.
   */
  case class Rule (options: Options = NoOpt) extends Block
  
  
  
  /** A simple text element.
   */
  case class Text (content: String, options: Options = NoOpt) extends Span with TextContainer

  /** A span of emphasized inline elements that may contain nested spans.
   */
  case class Emphasized (content: Seq[Span], options: Options = NoOpt) extends Span with SpanContainer[Emphasized]
  
  /** A span of strong inline elements that may contain nested spans.
   */
  case class Strong (content: Seq[Span], options: Options = NoOpt) extends Span with SpanContainer[Strong]
    
  /** A span containing plain, unparsed text.
   */
  case class Literal (content: String, options: Options = NoOpt) extends Span with TextContainer
  
  /** A span of program code. The content is a sequence of spans to support
   *  the later integration of syntax highlighting systems. Without this support
   *  the sequence will only consist of a single `Text` element.
   */
  case class Code (language: String, content: Seq[Span], options: Options = NoOpt) extends Span with SpanContainer[Code]
  
  /** Represents a single path in absolute and relative form. 
   */ 
  case class PathInfo (absolute: Path, relative: Path)
  
  /** An external link element, with the span content representing the text (description) of the link.
   */
  case class ExternalLink (content: Seq[Span], url: String, title: Option[String] = None, options: Options = NoOpt) extends Link 
                                                                                                                    with SpanContainer[ExternalLink]

  /** An internal link element, with the span content representing the text (description) of the link.
   */
  case class InternalLink (content: Seq[Span], ref: String, title: Option[String] = None, options: Options = NoOpt) extends Link 
                                                                                                                    with SpanContainer[InternalLink]
  
  /** A link element pointing to a location in a different document, with the span content representing the text (description) of the link.
   */
  case class CrossLink (content: Seq[Span], ref: String, path: PathInfo, title: Option[String] = None, options: Options = NoOpt) extends Link 
                                                                                                                       with SpanContainer[CrossLink]
  
  /** A resolved link to a footnote.
   */
  case class FootnoteLink (ref: String, label: String, options: Options = NoOpt) extends Link

  /** A resolved link to a citation.
   */
  case class CitationLink (ref: String, label: String, options: Options = NoOpt) extends Link
  
  /** An inline image with a text description and optional title.
   */
  case class Image (text: String, url: String, title: Option[String] = None, options: Options = NoOpt) extends Link
  
  
  /** A link reference, the id pointing to the id of a `LinkTarget`. Only part of the
   *  raw document tree and then removed by the rewrite rule that resolves link and image references.
   */
  case class LinkReference (content: Seq[Span], id: String, source: String, options: Options = NoOpt) extends Reference 
                                                                                                      with SpanContainer[LinkReference]
  
  /** An image reference, the id pointing to the id of a `LinkTarget`. Only part of the
   *  raw document tree and then removed by the rewrite rule that resolves link and image references.
   */
  case class ImageReference (text: String, id: String, source: String, options: Options = NoOpt) extends Reference
  
  /** A reference to a footnote with a matching label.  Only part of the
   *  raw document tree and then removed by the rewrite rule that resolves link and image references.
   */
  case class FootnoteReference (label: FootnoteLabel, source: String, options: Options = NoOpt) extends Reference

  /** A reference to a citation with a matching label.  Only part of the
   *  raw document tree and then removed by the rewrite rule that resolves link and image references.
   */
  case class CitationReference (label: String, source: String, options: Options = NoOpt) extends Reference
  

  
  /** An explicit hard line break.
   */
  case class LineBreak (options: Options = NoOpt) extends Span
  
  /** A comment that may be omitted by renderers.
   */
  case class Comment (content: String, options: Options = NoOpt) extends Block with TextContainer
  
  /** Message generated by the parser, usually to signal potential parsing problems.
   *  They usually get inserted immediately after the block or span that caused the problem.
   *  It mixes in both the Span and Block trait so that it can appear in sequences of both types. 
   *  By default messages are ignored by most renderers (apart from PrettyPrint), but
   *  they can be explicitly activated for a particular level.
   */
  case class SystemMessage (level: MessageLevel, content: String, options: Options = NoOpt) extends Span with Block with TextContainer
  
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
  
  /** Groups a span that could not be successfully parsed with a system message.
   *  Renderers may then choose to just render the fallback, the message or both.
   */
  case class InvalidSpan (message: SystemMessage, fallback: Span, options: Options = NoOpt) extends Span with Invalid[Span]

  /** Groups a block that could not be successfully parsed with a system message.
   *  Renderers may then choose to just render the fallback, the message or both.
   */
  case class InvalidBlock (message: SystemMessage, fallback: Block, options: Options = NoOpt) extends Block with Invalid[Block]
  
  /** A special type of paragraph that serves as a render hint.
   *  Some renderers simplify the rendering of block elements containing only a single
   *  paragraph and render the span content inline (e.g. a `&lt;li&gt;` tag without a nested `&lt;p&gt;` tag
   *  for the paragraph). 
   *  Using this element as mandated by some edge cases in both the Markdown and reStructuredText 
   *  markup definitions prevents this.
   */
  case class ForcedParagraph (content: Seq[Span], options: Options = NoOpt) extends Block 
                                                  with SpanContainer[ForcedParagraph] with Fallback {
    def fallback = Paragraph(content, options)
  }
  
  
  /** `Options` implementation for non-empty instances.
   * 
   *  For creating new instances it is usually more convenient to use the various factory objects.
   *  Example for creating an instance with an id and two styles applied:
   * 
   *  {{{
   *  val options = Id("myId") + Styles("style1","style2")
   *  }}}
   * 
   *  Likewise it is also often more convenient to use the corresponding extractors for pattern matching.
   */
  case class SomeOpt (id: Option[String] = None, styles: Seq[String] = Nil) extends Options {
    def + (other: Options) = SomeOpt(other.id.orElse(id), (styles ++ other.styles) distinct)
  }
  
  /** Empty `Options` implementation.
   */
  case object NoOpt extends Options {
    def id: Option[String] = None
    def styles: Seq[String] = Nil
    def + (other: Options) = other
  }
  
  /** Factory and extractor for an `Options` instance
   *  with an id.
   */
  object Id {
    def apply (value: String) = SomeOpt(id = Some(value))
    def unapply (value: Options) = value.id 
  }
  
  /** Factory and extractor for an `Options` instance
   *  with style names.
   */
  object Styles {
    def apply (values: String*) = SomeOpt(styles = values)
    def unapplySeq (value: Options) = Some(value.styles) 
  }
  
  /** Companion for the Options trait.
   */
  object Options {
    def apply (id: Option[String] = None, styles: Seq[String] = Nil) =
      if (id.isEmpty && styles.isEmpty) NoOpt
      else SomeOpt(id,styles)
  }
  
}