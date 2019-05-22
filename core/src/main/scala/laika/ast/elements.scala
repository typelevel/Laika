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

package laika.ast

import java.text.DecimalFormat

import laika.api.{Render, Renderer}
import laika.format.AST

import scala.math.Ordered

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
  def styles: Set[String]
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
trait LinkTarget extends Block



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
trait ElementContainer[+E <: Element, Self <: ElementContainer[E,Self]] extends Container[Seq[E]] with ElementTraversal {
  override def toString: String = "\n" + ASTRenderer.get.render(this) + "\n"
}

/** A generic container of child elements which can have
  * rewrite rules applied to them in recursive tree rewriting.
  */
trait RewritableContainer[Self <: RewritableContainer[Self]] extends Element { this: Self =>

  /** Rewrites all children of this container based on the specified rules.
    * 
    * Concrete types are expected to support rewriting at least for all standard block, span and template span
    * elements they contain, plus optionally for any other elements that have custom support for rewriting.
    */
  def rewriteChildren (rules: RewriteRules): Self

  /** Rewrites all span children of this container based on the specified rules.
    *
    * Concrete types are expected to support rewriting at least for all standard block, span and template span
    * elements they contain, plus optionally for any other elements that have custom support for rewriting.
    */
  def rewriteSpans (rule: RewriteRule[Span]): Self = rewriteChildren(RewriteRules(spanRules = Seq(rule)))

}

/** A container of other Block elements. Such a container is usually
 *  also a Block itself.
 */
trait BlockContainer[Self <: BlockContainer[Self]] extends ElementContainer[Block,Self] with RewritableContainer[Self] { this: Self =>

  /** Rewrites all block children of this container based on the specified rules.
    *
    * Concrete types are expected to support rewriting at least for all standard block, span and template span
    * elements they contain, plus optionally for any other elements that have custom support for rewriting.
    */
  def rewriteBlocks (rules: RewriteRule[Block]): Self = rewriteChildren(RewriteRules(blockRules = Seq(rules)))

  def rewriteChildren (rules: RewriteRules): Self = withContent(rules.rewriteBlocks(content))

  /** Creates a copy of this instance with the specified new content.
    * 
    * Implementation note: This method exists to deal with the fact that there is no polymorphic copy method
    * and trades a small bit of boilerplate for avoiding the compile time hit of using shapeless for this.
    */
  def withContent (newContent: Seq[Block]): Self

}

/** A container of other Span elements. Such a container may be a Block
 *  or a Span itself.
 */
trait SpanContainer[Self <: SpanContainer[Self]] extends ElementContainer[Span,Self] with RewritableContainer[Self] { this: Self =>

  def rewriteChildren (rules: RewriteRules): Self = withContent(rules.rewriteSpans(content))

  /** Creates a copy of this instance with the specified new content.
    *
    * Implementation note: This method exists to deal with the fact that there is no polymorphic copy method
    * and trades a small bit of boilerplate for avoiding the compile time hit of using shapeless for this.
    */
  def withContent (newContent: Seq[Span]): Self
  
  /**  Extracts the text from the spans of this container, removing
    *  any formatting or links.
    */
  def extractText: String = content map {
    case tc: TextContainer    => tc.content
    case sc: SpanContainer[_] => sc.extractText
    case _ => ""
  } mkString

}

/** A container of list items. Such a container is usually a Block itself.
 */
trait ListContainer[Self <: ListContainer[Self]] extends ElementContainer[ListItem, Self]

/** The root element of a document tree.
 */
case class RootElement (content: Seq[Block], options: Options = NoOpt) extends Block with BlockContainer[RootElement] {
  def withContent (newContent: Seq[Block]): RootElement = copy(content = newContent)
}

/** A named document fragment that usually gets rendered separately from the main root element
 */
case class DocumentFragment (name: String, root: Element, options: Options = NoOpt) extends Block with Temporary

/** An element that only gets rendered for a specific output format.
 */
case class TargetFormat (format: String, element: Element, options: Options = NoOpt) extends Block

/** A single configuration value to be merged with the top document config.
 *  The value can be any type allowed by the Typesafe Config library (i.e. Boolean,
 *  Number, String, Map, Iterable).
 */
case class ConfigValue (name: String, value: AnyRef, options: Options = NoOpt) extends Block with Span with Temporary

/** A section of the document, consisting of a header and content in the form
 *  of a list of Block elements. Sections may be nested inside other sections,
 *  they are arranged in a hierarchy based on the level of their header element.
 */
case class Section (header: Header, content: Seq[Block], options: Options = NoOpt) extends Block with BlockContainer[Section] {

  override def rewriteChildren (rules: RewriteRules): Section = (rules.rewriteBlocks(content), rules.rewriteBlock(header)) match {
    case (newContent, newHeader: Header) => copy(content = newContent, header = newHeader)
    case (newContent, newHeader)         => copy(content = newHeader +: newContent, header = header.copy(content = Nil))
  }
  
  def withContent (newContent: Seq[Block]): Section = copy(content = newContent)
}

/** A header element with a level, with 1 being the top level of the document.
 */
case class Header (level: Int, content: Seq[Span], options: Options = NoOpt) extends Block with SpanContainer[Header] {
  def withContent (newContent: Seq[Span]): Header = copy(content = newContent)
}

/** The (optional) title of the document.
 */
case class Title (content: Seq[Span], options: Options = NoOpt) extends Block with SpanContainer[Title] {
  def withContent (newContent: Seq[Span]): Title = copy(content = newContent)
}

/** A decorated header where the level gets determined in the rewrite phase based
 *  on the decoration used and the order they appear in the document. The first
 *  decoration type encountered is used for level 1, the second for level 2, and
 *  so on.
 */
case class DecoratedHeader (decoration: HeaderDecoration, content: Seq[Span], options: Options = NoOpt) extends Block
                                                                                                        with Temporary
                                                                                                        with SpanContainer[DecoratedHeader] {
  def withContent (newContent: Seq[Span]): DecoratedHeader = copy(content = newContent)
}

/** Represents the decoration of a header.
 *  Concrete implementations need to be provided by the parser.
 */
trait HeaderDecoration

/** Represents a section number, usually used in header elements
 *  when autonumbering is applied.
 */
case class SectionNumber(position: Seq[Int], options: Options = NoOpt) extends Span with TextContainer {

  val content = position.mkString(".") + " "

  /** Creates a new instance for a child section
   *  of this section at the specified position.
   */
  def child(childPosition: Int) = SectionNumber(position :+ childPosition)
}


/** A generic container element containing a list of blocks. Can be used where a sequence
 *  of blocks must be inserted in a place where a single element is required by the API.
 *  Usually renderers do not treat the container as a special element and render its children
 *  as s sub flow of the parent container.
 */
case class BlockSequence (content: Seq[Block], options: Options = NoOpt) extends Block with BlockContainer[BlockSequence] {
  def withContent (newContent: Seq[Block]): BlockSequence = copy(content = newContent)
}

/** A generic container element containing a list of spans. Can be used where a sequence
 *  of spans must be inserted in a place where a single element is required by the API.
 *  Usually renderers do not treat the container as a special element and render its children
 *  as s sub flow of the parent container. A span sequence is special in that in can be
 *  used as both a span and a block.
 */
case class SpanSequence (content: Seq[Span], options: Options = NoOpt) extends Block with Span with SpanContainer[SpanSequence] {
  def withContent (newContent: Seq[Span]): SpanSequence = copy(content = newContent)
}


/** A paragraph consisting of span elements.
 */
case class Paragraph (content: Seq[Span], options: Options = NoOpt) extends Block with SpanContainer[Paragraph] {
  def withContent (newContent: Seq[Span]): Paragraph = copy(content = newContent)
}

/** A literal block with unparsed text content.
 */
case class LiteralBlock (content: String, options: Options = NoOpt) extends Block with TextContainer

/** A literal block with parsed text content.
 */
case class ParsedLiteralBlock (content: Seq[Span], options: Options = NoOpt) extends Block with SpanContainer[ParsedLiteralBlock] {
  def withContent (newContent: Seq[Span]): ParsedLiteralBlock = copy(content = newContent)
}

/** A block of program code. The content is a sequence of spans to support
 *  the later integration of syntax highlighting systems. Without this support
 *  the sequence will only consist of a single `Text` element.
 */
case class CodeBlock (language: String, content: Seq[Span], options: Options = NoOpt) extends Block with SpanContainer[CodeBlock] {
  def withContent (newContent: Seq[Span]): CodeBlock = copy(content = newContent)
}

/** A quoted block consisting of a list of blocks that may contain other
 *  nested quoted blocks and an attribution which may be empty.
 */
case class QuotedBlock (content: Seq[Block], attribution: Seq[Span], options: Options = NoOpt) extends Block
                                                                                               with BlockContainer[QuotedBlock] {

  override def rewriteChildren (rules: RewriteRules): QuotedBlock = 
    copy(content = rules.rewriteBlocks(content), attribution = rules.rewriteSpans(attribution))
  
  def withContent (newContent: Seq[Block]): QuotedBlock = copy(content = newContent)
}

/** Generic block element with a title.
 *  Often combined with the the `styles` attribute of the `options` parameter to provide
 *  additional render hints.
 */
case class TitledBlock (title: Seq[Span], content: Seq[Block], options: Options = NoOpt) extends Block
                                                                                         with BlockContainer[TitledBlock] {

  override def rewriteChildren (rules: RewriteRules): TitledBlock = 
    copy(content = rules.rewriteBlocks(content), title = rules.rewriteSpans(title))
  
  def withContent (newContent: Seq[Block]): TitledBlock = copy(content = newContent)
}

/** A figure consists of an image, an optional caption, and an optional legend as the `content` property.
 *  The `image` property is of type `Span` as the image might be wrapped inside a link reference.
 */
case class Figure (image: Span, caption: Seq[Span], content: Seq[Block], options: Options = NoOpt) extends Block with BlockContainer[Figure] {

  override def rewriteChildren (rules: RewriteRules): Figure = copy(
    content = rules.rewriteBlocks(content), 
    caption = rules.rewriteSpans(caption),
    image = rules.rewriteSpan(image)
  )
  
  def withContent (newContent: Seq[Block]): Figure = copy(content = newContent)
}

/** A bullet list that may contain nested lists.
 */
case class BulletList (content: Seq[BulletListItem], format: BulletFormat, options: Options = NoOpt) extends Block
                                                                                               with ListContainer[BulletList]
                                                                                               with RewritableContainer[BulletList] {

  override def rewriteChildren (rules: RewriteRules): BulletList =
    copy(content = content.map(_.rewriteChildren(rules)))
}

/** An enumerated list that may contain nested lists.
 */
case class EnumList (content: Seq[EnumListItem], format: EnumFormat, start: Int = 1, options: Options = NoOpt) extends Block
                                                                                                           with ListContainer[EnumList]
                                                                                                           with RewritableContainer[EnumList] {

  override def rewriteChildren (rules: RewriteRules): EnumList =
    copy(content = content.map(_.rewriteChildren(rules)))
}

/** The format of a bullet list item.
 */
trait BulletFormat

/** Bullet format based on a simple string.
 */
case class StringBullet (bullet: String) extends BulletFormat

/** The format of enumerated list items.
 */
case class EnumFormat (enumType: EnumType = EnumType.Arabic, prefix: String = "", suffix: String = ".") {
  override def toString = s"EnumFormat($enumType,${prefix}N$suffix)"
}

/** Represents the type of an ordered list.
 */
sealed abstract class EnumType

/** Enumeration of supported enum types.
  */
object EnumType {

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
}


/** A single bullet list item consisting of one or more block elements.
 */
case class BulletListItem (content: Seq[Block], format: BulletFormat, options: Options = NoOpt) extends ListItem
                                                                                                with BlockContainer[BulletListItem] {
  def withContent (newContent: Seq[Block]): BulletListItem = copy(content = newContent)
}

/** A single enum list item consisting of one or more block elements.
 */
case class EnumListItem (content: Seq[Block], format: EnumFormat, position: Int, options: Options = NoOpt) extends ListItem
                                                                                                    with BlockContainer[EnumListItem] {
  def withContent (newContent: Seq[Block]): EnumListItem = copy(content = newContent)
}

/** A list of terms and their definitions.
 *  Not related to the `Definition` base trait.
 */
case class DefinitionList (content: Seq[DefinitionListItem], options: Options = NoOpt) extends Block with ListContainer[DefinitionList]
                                                                                                     with RewritableContainer[DefinitionList] {

  def rewriteChildren (rules: RewriteRules): DefinitionList = copy(content = content.map(_.rewriteChildren(rules)))
}

/** A single definition item, containing the term and definition (as the content property).
 */
case class DefinitionListItem (term: Seq[Span], content: Seq[Block], options: Options = NoOpt) extends ListItem
                                                                                               with BlockContainer[DefinitionListItem] {

  override def rewriteChildren (rules: RewriteRules): DefinitionListItem = 
    copy(content = rules.rewriteBlocks(content), term = rules.rewriteSpans(term))
  
  def withContent (newContent: Seq[Block]): DefinitionListItem = copy(content = newContent)
  
}

/** A single item inside a line block.
 */
abstract class LineBlockItem extends Block

/** A single line inside a line block.
 */
case class Line (content: Seq[Span], options: Options = NoOpt) extends LineBlockItem with SpanContainer[Line] {
  def withContent (newContent: Seq[Span]): Line = copy(content = newContent)
}

/** A block containing lines which preserve line breaks and optionally nested line blocks.
 */
case class LineBlock (content: Seq[LineBlockItem], options: Options = NoOpt) extends LineBlockItem with BlockContainer[LineBlock] {
  def withContent (newContent: Seq[Block]): LineBlock = copy(content = newContent.asInstanceOf[Seq[LineBlockItem]]) // TODO - 0.12 - type mismatch
}


/** A table consisting of a head and a body part and optional caption and column specification.
 */
case class Table (head: TableHead, body: TableBody, caption: Caption = Caption(), columns: Columns = Columns(Nil), options: Options = NoOpt) extends Block
                                                                                                               with ElementTraversal with RewritableContainer[Table] {
  
  def rewriteChildren (rules: RewriteRules): Table = copy(head = head.rewriteChildren(rules), body = body.rewriteChildren(rules), caption = caption.rewriteChildren(rules))
 
  override def toString = "\n" + ASTRenderer.get.render(this) + "\n"
}

/** A table element, like a row, cell or column.
 */
trait TableElement extends Customizable

/** A container of table elements.
 */
trait TableContainer[Self <: TableContainer[Self]] extends TableElement with ElementContainer[TableElement,Self]

/** Contains the header rows of a table.
 */
case class TableHead (content: Seq[Row], options: Options = NoOpt) extends TableElement with TableContainer[TableHead] with RewritableContainer[TableHead] {
  def rewriteChildren (rules: RewriteRules): TableHead = copy(content = content.map(_.rewriteChildren(rules)))
}

/** Contains the body rows of a table.
 */
case class TableBody (content: Seq[Row], options: Options = NoOpt) extends TableElement with TableContainer[TableBody] with RewritableContainer[TableBody] {
  def rewriteChildren (rules: RewriteRules): TableBody = copy(content = content.map(_.rewriteChildren(rules)))
}

/** The table caption.
 */
case class Caption (content: Seq[Span] = Nil, options: Options = NoOpt) extends TableElement with SpanContainer[Caption] {
  def withContent (newContent: Seq[Span]): Caption = copy(content = newContent)
}

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
case class Row (content: Seq[Cell], options: Options = NoOpt) extends TableElement with TableContainer[Row] with RewritableContainer[Row] {
  def rewriteChildren (rules: RewriteRules): Row = copy(content = content.map(_.rewriteChildren(rules)))
}

/** A single cell, potentially spanning multiple rows or columns, containing
 *  one or more block elements.
 */
case class Cell (cellType: CellType, content: Seq[Block], colspan: Int = 1, rowspan: Int = 1, options: Options = NoOpt) extends TableElement
                                                                                                                        with BlockContainer[Cell] {
  def withContent (newContent: Seq[Block]): Cell = copy(content = newContent)
}

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
                                                                                                    with BlockContainer[FootnoteDefinition] {
  def withContent (newContent: Seq[Block]): FootnoteDefinition = copy(content = newContent)
}


/** Points to the following block or span element, making it a target for links.
 */
case class InternalLinkTarget (options: Options = NoOpt) extends Block with Span with LinkTarget

/** A citation that can be referred to by a `CitationLink` by id.
 */
case class Citation (label: String, content: Seq[Block], options: Options = NoOpt) extends Block
                                                                                with LinkTarget
                                                                                with BlockContainer[Citation] {
  def withContent (newContent: Seq[Block]): Citation = copy(content = newContent)
}

/** A footnote with resolved id and label that can be referred to by a `FootnoteLink` by id.
 */
case class Footnote (label: String, content: Seq[Block], options: Options = NoOpt) extends Block
                                                                                               with LinkTarget
                                                                                               with BlockContainer[Footnote] {
  def withContent (newContent: Seq[Block]): Footnote = copy(content = newContent)
}


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
case class Emphasized (content: Seq[Span], options: Options = NoOpt) extends Span with SpanContainer[Emphasized] {
  def withContent (newContent: Seq[Span]): Emphasized = copy(content = newContent)
}

/** A span of strong inline elements that may contain nested spans.
 */
case class Strong (content: Seq[Span], options: Options = NoOpt) extends Span with SpanContainer[Strong] {
  def withContent (newContent: Seq[Span]): Strong = copy(content = newContent)
}

/** A span containing plain, unparsed text.
 */
case class Literal (content: String, options: Options = NoOpt) extends Span with TextContainer

/** A span of program code. The content is a sequence of spans to support
 *  the later integration of syntax highlighting systems. Without this support
 *  the sequence will only consist of a single `Text` element.
 */
case class Code (language: String, content: Seq[Span], options: Options = NoOpt) extends Span with SpanContainer[Code] {
  def withContent (newContent: Seq[Span]): Code = copy(content = newContent)
}

/** A span representing deleted inline elements that may contain nested spans.
  */
case class Deleted (content: Seq[Span], options: Options = NoOpt) extends Span with SpanContainer[Deleted] {
  def withContent (newContent: Seq[Span]): Deleted = copy(content = newContent)
}

/** A span representing inserted inline elements that may contain nested spans.
  */
case class Inserted (content: Seq[Span], options: Options = NoOpt) extends Span with SpanContainer[Inserted] {
  def withContent (newContent: Seq[Span]): Inserted = copy(content = newContent)
}

/** Represents a URI which might also optionally be expressed as a local reference within the processed tree.
 */
case class URI (uri: String, localRef: Option[PathInfo] = None)

/** Represents a single path in absolute and relative form.
 */
case class PathInfo (absolute: Path, relative: Path)

object PathInfo {

  /** Creates an instance for the specified path relative to
   *  the provided reference path.
   */
  def fromPath (path: Path, refPath: Path): PathInfo =
    if (path.isAbsolute) PathInfo(path, path.relativeTo(refPath))
    else PathInfo(Path.Root / path.relativeTo(refPath), path)

  /** Creates an instance for the specified URI relative to
   *  the provided reference path. Returns `None` if the specified
   *  URI is not a file or relative URI.
   */
  def fromURI (uri: String, refPath: Path): Option[PathInfo] = {
    val jURI = new java.net.URI(uri)
    if (jURI.getScheme != null && jURI.getScheme != "file") None
    else Some(fromPath(Path(jURI.getPath), refPath))
  }

}

/** An external link element, with the span content representing the text (description) of the link.
 */
case class ExternalLink (content: Seq[Span], url: String, title: Option[String] = None, options: Options = NoOpt) extends Link
                                                                                                                  with SpanContainer[ExternalLink] {
  def withContent (newContent: Seq[Span]): ExternalLink = copy(content = newContent)
}

/** An internal link element, with the span content representing the text (description) of the link.
 */
case class InternalLink (content: Seq[Span], ref: String, title: Option[String] = None, options: Options = NoOpt) extends Link
                                                                                                                  with SpanContainer[InternalLink] {
  def withContent (newContent: Seq[Span]): InternalLink = copy(content = newContent)
}

/** A link element pointing to a location in a different document, with the span content representing the text (description) of the link.
 */
case class CrossLink (content: Seq[Span], ref: String, path: PathInfo, title: Option[String] = None, options: Options = NoOpt) extends Link
                                                                                                                     with SpanContainer[CrossLink] {
  def withContent (newContent: Seq[Span]): CrossLink = copy(content = newContent)
}

/** A resolved link to a footnote.
 */
case class FootnoteLink (ref: String, label: String, options: Options = NoOpt) extends Link

/** A resolved link to a citation.
 */
case class CitationLink (ref: String, label: String, options: Options = NoOpt) extends Link

/** An inline image with a text description and optional title.
 */
case class Image (text: String, uri: URI, width: Option[Size] = None, height: Option[Size] = None, title: Option[String] = None, options: Options = NoOpt) extends Link

/** Encapsulates size information with a unit.
  */
case class Size (amount: Double, unit: String) {
  val formatter = new DecimalFormat("#.##")
  def scale (percent: Double): Size = copy(amount = amount * percent / 100)
  def displayValue: String = s"${formatter.format(amount)}$unit"
}


/** A link reference, the id pointing to the id of a `LinkTarget`. Only part of the
 *  raw document tree and then removed by the rewrite rule that resolves link and image references.
 */
case class LinkReference (content: Seq[Span], id: String, source: String, options: Options = NoOpt) extends Reference
                                                                                                    with SpanContainer[LinkReference] {
  def withContent (newContent: Seq[Span]): LinkReference = copy(content = newContent)
}

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

/** An explicit hard page break.
 */
case class PageBreak (options: Options = NoOpt) extends Block

/** A comment that may be omitted by renderers.
 */
case class Comment (content: String, options: Options = NoOpt) extends Block with TextContainer

/** Message generated by the parser, usually to signal potential parsing problems.
 *  They usually get inserted immediately after the block or span that caused the problem.
 *  It mixes in both the Span and Block trait so that it can appear in sequences of both types.
 *  By default messages are ignored by most renderers (apart from AST), but
 *  they can be explicitly activated for a particular level.
 */
case class SystemMessage (level: MessageLevel, content: String, options: Options = NoOpt) extends Span with Block with TextContainer

/** Signals the severity of a system message.
 */
sealed abstract class MessageLevel (private val level: Int) extends Ordered[MessageLevel] {
  def compare(that: MessageLevel): Int = level compare that.level
}

/** Enumeration of available message levels.
  */
object MessageLevel {

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
}

/** Groups a span that could not be successfully parsed with a system message.
 *  Renderers may then choose to just render the fallback, the message or both.
 */
case class InvalidSpan (message: SystemMessage, fallback: Span, options: Options = NoOpt) extends Span with Invalid[Span]

/** Groups a block that could not be successfully parsed with a system message.
 *  Renderers may then choose to just render the fallback, the message or both.
 */
case class InvalidBlock (message: SystemMessage, fallback: Block, options: Options = NoOpt) extends Block with Invalid[Block]

/** Represents an invalid element in any position, block, span or template.
  * Provides convenience converters to produce instances for any of these three positions.
  */
case class InvalidElement (message: SystemMessage, source: String) extends Element {

  def asBlock: InvalidBlock = InvalidBlock(message, LiteralBlock(source))

  def asSpan: InvalidSpan = InvalidSpan(message, Text(source))

  def asTemplateSpan: TemplateSpan = TemplateElement(asSpan)

}

/** Companion for InvalidElement. */
object InvalidElement {
  def apply (message: String, source: String): InvalidElement =
    apply(SystemMessage(MessageLevel.Error, message), source)
}

/** A special type of paragraph that serves as a render hint.
 *  Some renderers simplify the rendering of block elements containing only a single
 *  paragraph and render the span content inline (e.g. a `&lt;li&gt;` tag without a nested `&lt;p&gt;` tag
 *  for the paragraph).
 *  Using this element as mandated by some edge cases in both the Markdown and reStructuredText
 *  markup definitions prevents this.
 */
case class ForcedParagraph (content: Seq[Span], options: Options = NoOpt) extends Block
                                                with SpanContainer[ForcedParagraph] with Fallback {
  
  def withContent (newContent: Seq[Span]): ForcedParagraph = copy(content = newContent)
  
  def fallback: Element = Paragraph(content, options)
}

case class Reverse (length: Int, target: Span, fallback: Span, options: Options = NoOpt) extends Span


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
case class SomeOpt (id: Option[String] = None, styles: Set[String] = Set()) extends Options {
  def + (other: Options): Options = SomeOpt(other.id.orElse(id), styles ++ other.styles)
}

/** Empty `Options` implementation.
 */
case object NoOpt extends Options {
  val id: Option[String] = None
  val styles: Set[String] = Set()
  def + (other: Options): Options = other
}

/** Factory and extractor for an `Options` instance
 *  with an id.
 */
object Id {
  def apply (value: String): Options = SomeOpt(id = Some(value))
  def unapply (value: Options): Option[String] = value.id
}

/** Factory and extractor for an `Options` instance
 *  with style names.
 */
object Styles {
  def apply (values: String*): Options = SomeOpt(styles = values.toSet)
  def unapplySeq (value: Options): Option[Seq[String]] = Some(value.styles.toSeq)
}

private[ast] object ASTRenderer {
  lazy val get: Renderer = Render.as(AST).build
}

/** Companion for the Options trait.
 */
object Options {
  def apply (id: Option[String] = None, styles: Set[String] = Set()): Options =
    if (id.isEmpty && styles.isEmpty) NoOpt
    else SomeOpt(id,styles)

  /** Returns a new instance of the customizable element
    *  without its id.
    */
  def removeId [C <: Customizable] (c: C): C = modifyOptions(c, opt => Options(None,opt.styles))

  /** Returns a new instance of the customizable element
    *  with its id set to the specified value, overriding any existing value.
    */
  def setId [C <: Customizable] (c: C, id: String): C = modifyOptions(c, opt => Options(Some(id), opt.styles))

  /** Returns a new instance of the customizable element
    *  with its options merged with the specified options/
    */
  def merge [C <: Customizable] (c: C, opt: Options): C = modifyOptions(c, _ + opt)

  /** Returns a new instance of the customizable element
    *  with its options modified according to the specified function.
    */
  private def modifyOptions [C <: Customizable] (c: C, f: Options => Options): C = {
    val newElements = (c.productIterator map {
      case opt:Options => f(opt)
      case other => other
    }).toArray

    c.getClass.getConstructors()(0)
      .newInstance(newElements.asInstanceOf[Array[AnyRef]]:_*).asInstanceOf[C]
  }
}
