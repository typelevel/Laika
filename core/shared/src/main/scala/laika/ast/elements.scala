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

package laika.ast

import cats.data.NonEmptySet
import laika.api.Renderer
import laika.ast.Path.Root
import laika.ast.RelativePath.CurrentDocument
import laika.config.{ConfigEncoder, ConfigValue}
import laika.format.AST
import laika.parse.code.CodeCategory

import scala.math.Ordered

/** The base class for all Elements forming the document tree.
 *  Usually not extended directly, instead either `Span` or
 *  `Block` should be picked as the base type for new element
 *  types.
 */
abstract class Element extends Product with Serializable

/** An element that can be customized. Represents options
 *  that are usually only used on few selected nodes and
 *  can control subtle differences often only relevant
 *  for renderers.
 */
trait Customizable extends Element {
  
  type Self <: Customizable

  def options: Options

  /** Returns a new instance of this customizable element
    * without its id.
    */
  def withoutId: Self = modifyOptions(opt => Options(None, opt.styles))

  /** Returns a new instance of this customizable element
    * with its id set to the specified value, overriding any existing value.
    */
  def withId (id: String): Self = modifyOptions(opt => Options(Some(id), opt.styles))

  /** Returns a new instance of this customizable element
    * with its options merged with the specified options.
    */
  def mergeOptions (opt: Options): Self = modifyOptions(_ + opt)

  /** Returns a new instance of this customizable element
    * with the new options obtained from applying the specified function
    * to the existing value.
    */
  def modifyOptions (f: Options => Options): Self = withOptions(f(options))

  /** Returns a new instance of this customizable element
    * with the specified options replacing the current value.
    */
  def withOptions (options: Options): Self
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
trait Block extends Customizable { type Self <: Block }

/** The base type for all inline elements.
 */
trait Span extends Customizable { type Self <: Span }

/** The base type for all list items.
 */
trait ListItem extends Customizable { type Self <: ListItem }

/** Represents a hidden element that will be ignored by renderers.
  * 
  * These kind of nodes usually provide information that will be extracted
  * from the tree before AST transformations and renderers get applied.
 */
trait Hidden extends Element

/** Represents an element that needs to be resolved in an AST transformation step.
  * 
  * Passing documents that still contain elements of this kind to a renderer
  * will usually be treated as errors.
  */
trait Unresolved extends Element {

  /** An error message to display when this element remain unresolved until after the final
    * AST transformation step.
    */
  def unresolvedMessage: String
}

/** Represents an invalid element. 
  * Renderers can choose to either render the fallback or the runtime message or both.
  */
trait Invalid[+E <: Element] extends Element {
  def message: RuntimeMessage
  def fallback: E
}

/** The base type for all reference elements.
 *
 *  A reference points to some other node in the document tree and needs
 *  to be resolved and replaced by a rewrite rule before rendering.
 *  Therefore none of the available renderers include logic for dealing with references.
 */
trait Reference extends Span with Unresolved {
  type Self <: Reference
  def source: String
}

/** Represents a definition that can be used to resolve references.
 *
 *  Only part of the raw document tree and then removed or replaced
 *  by a rewrite rule before rendering.
 */
trait Definition extends Block { type Self <: Definition }

/** The base type for all link elements.
 *
 *  In contrast to the reference type, it is only mixed in by
 *  elements representing resolved links that can be dealt
 *  with by renderers.
 */
trait Link extends Span { type Self <: Link }

/** The base type for all link targets. The id has to be
 *  unique for the whole document across all types
 *  of `LinkTarget` implementations.
 */
trait LinkTarget extends Block { type Self <: LinkTarget }



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
trait ElementContainer[+E <: Element] extends Container[Seq[E]] with ElementTraversal {
  override def toString: String = "\n" + ASTRenderer.get.render(this) + "\n"
}

/** A generic container of child elements which can have
  * rewrite rules applied to them in recursive tree rewriting.
  */
trait RewritableContainer extends Customizable {

  type Self <: RewritableContainer
  
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
trait BlockContainer extends ElementContainer[Block] with RewritableContainer {

  type Self <: BlockContainer
  
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
trait SpanContainer extends ElementContainer[Span] with RewritableContainer {

  type Self <: SpanContainer
  
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
    case tc: TextContainer => tc.content
    case sc: SpanContainer => sc.extractText
    case _ => ""
  } mkString

}

/** A container of list items. Such a container is usually a Block itself.
 */
trait ListContainer extends ElementContainer[ListItem]

/** Common methods for simple span containers (without additional parameters). */
trait SpanContainerCompanion {
  
  type ContainerType
  
  /** Creates an empty instance */
  def empty: ContainerType = createSpanContainer(Nil)
  
  /** Create an instance only containing a single Text span */
  def apply(text: String): ContainerType = createSpanContainer(Seq(Text(text)))

  /** Create an instance containing a one or more spans */
  def apply(span: Span, spans: Span*): ContainerType = createSpanContainer(span +: spans.toList)

  protected def createSpanContainer (spans: Seq[Span]): ContainerType
}

/** Common methods for simple block containers (without additional parameters). */
trait BlockContainerCompanion extends SpanContainerCompanion {

  override def empty: ContainerType = createBlockContainer(Nil)
  
  protected def createSpanContainer (spans: Seq[Span]): ContainerType = createBlockContainer(Seq(Paragraph(spans)))

  /** Create an instance containing a one or more blocks */
  def apply(block: Block, blocks: Block*): ContainerType = createBlockContainer(block +: blocks.toList)

  protected def createBlockContainer (blocks: Seq[Block]): ContainerType
}

/** The root element of a document tree.
 */
case class RootElement (content: Seq[Block], options: Options = NoOpt) extends Block with BlockContainer {
  type Self = RootElement
  def withContent (newContent: Seq[Block]): RootElement = copy(content = newContent)
  def withOptions (options: Options): RootElement = copy(options = options)
}
object RootElement extends BlockContainerCompanion {
  type ContainerType = RootElement
  override protected def createBlockContainer (blocks: Seq[Block]) = RootElement(blocks)
}

/** A named document fragment that usually gets rendered separately from the main root element
 */
case class DocumentFragment (name: String, root: Element, options: Options = NoOpt) extends Block with Hidden {
  type Self = DocumentFragment
  def withOptions (options: Options): DocumentFragment = copy(options = options)
}

/** An element that only gets rendered for a specific output format.
 */
case class TargetFormat (formats: NonEmptySet[String], element: Element, options: Options = NoOpt) extends Block {
  type Self = TargetFormat
  def withOptions (options: Options): TargetFormat = copy(options = options)
}

/** Represents a single choice in a `ChoiceGroup`.
  */
case class Choice(name: String, label: String, content: Seq[Block], options: Options = NoOpt) extends BlockContainer {
  type Self = Choice
  def withContent(newContent: Seq[Block]): Choice = copy(content = newContent)
  def withOptions(options: Options): Choice = copy(options = options)
}

/** Represents a selection of choices (alternatives) that represent the same content in different ways,
  * e.g. a code sample in Scala or Java or a build setup in sbt vs. Maven.
  * In the final output these will usually be rendered in a way to allow for a convenient selection.
  */
case class Selection(name: String, choices: Seq[Choice], options: Options = NoOpt) extends Block with RewritableContainer {
  type Self = Selection
  def withOptions(options: Options): Selection = copy(options = options)
  def rewriteChildren(rules: RewriteRules): Selection = 
    copy(choices = choices.map(c => c.withContent(rules.rewriteBlocks(c.content))))
}

/** A single configuration value to be merged with the top document config.
 *  This is particularly useful for directive implementations that need to contribute
 *  to the configuration of the document.
 *  During rendering these embedded configuration values will be discarded.
 */
case class EmbeddedConfigValue (key: String, value: ConfigValue, options: Options = NoOpt) extends Block with Span with Hidden {
  type Self = EmbeddedConfigValue
  def withOptions (options: Options): EmbeddedConfigValue = copy(options = options)
}
object EmbeddedConfigValue {
  def apply[T](name: String, value: T)(implicit encoder: ConfigEncoder[T]): EmbeddedConfigValue = 
    EmbeddedConfigValue(name, encoder(value), NoOpt)
}

/** A section of the document, consisting of a header and content in the form
 *  of a list of Block elements. Sections may be nested inside other sections,
 *  they are arranged in a hierarchy based on the level of their header element.
 */
case class Section (header: Header, content: Seq[Block], options: Options = NoOpt) extends Block with BlockContainer {
  type Self = Section
  
  override def rewriteChildren (rules: RewriteRules): Section = (rules.rewriteBlocks(content), rules.rewriteBlock(header)) match {
    case (newContent, newHeader: Header) => copy(content = newContent, header = newHeader)
    case (newContent, newHeader)         => copy(content = newHeader +: newContent, header = header.copy(content = Nil))
  }
  
  def withContent (newContent: Seq[Block]): Section = copy(content = newContent)
  def withOptions (options: Options): Section = copy(options = options)
}

/** A header element with a level, with 1 being the top level of the document.
 */
case class Header (level: Int, content: Seq[Span], options: Options = NoOpt) extends Block with SpanContainer {
  type Self = Header
  def withContent (newContent: Seq[Span]): Header = copy(content = newContent)
  def withOptions (options: Options): Header = copy(options = options)
}
object Header {
  /** Create an instance only containing a single Text span */
  def apply(level: Int, text: String): Header = Header(level, Seq(Text(text)))

  /** Create an instance containing a one or more spans */
  def apply(level: Int, span: Span, spans: Span*): Header = Header(level, span +: spans)
}

/** The (optional) title of the document.
 */
case class Title (content: Seq[Span], options: Options = NoOpt) extends Block with SpanContainer {
  type Self = Title
  def withContent (newContent: Seq[Span]): Title = copy(content = newContent)
  def withOptions (options: Options): Title = copy(options = options)
}
object Title extends SpanContainerCompanion {
  type ContainerType = Title
  protected def createSpanContainer (spans: Seq[Span]): Title = Title(spans)
}

/** A decorated header where the level gets determined in the rewrite phase based
 *  on the decoration used and the order they appear in the document. The first
 *  decoration type encountered is used for level 1, the second for level 2, and
 *  so on.
 */
case class DecoratedHeader (decoration: HeaderDecoration, content: Seq[Span], options: Options = NoOpt) extends Block
                                                                                                        with SpanContainer
                                                                                                        with Unresolved {
  type Self = DecoratedHeader
  def withContent (newContent: Seq[Span]): DecoratedHeader = copy(content = newContent)
  def withOptions (options: Options): DecoratedHeader = copy(options = options)
  lazy val unresolvedMessage: String = s"Unresolved decorated header with decoration '${decoration.toString}'"
}
object DecoratedHeader {
  /** Create an instance only containing a single Text span */
  def apply(decoration: HeaderDecoration, text: String): DecoratedHeader = DecoratedHeader(decoration, Seq(Text(text)))

  /** Create an instance containing a one or more spans */
  def apply(decoration: HeaderDecoration, span: Span, spans: Span*): DecoratedHeader = DecoratedHeader(decoration, span +: spans)
}


/** Represents the decoration of a header.
 *  Concrete implementations need to be provided by the parser.
 */
trait HeaderDecoration

/** Represents a section number, usually used in header elements
 *  when autonumbering is applied.
 */
case class SectionNumber(position: Seq[Int], options: Options = NoOpt) extends Span with TextContainer {

  type Self = SectionNumber
  
  val content = position.mkString(".") + " "

  /** Creates a new instance for a child section
   *  of this section at the specified position.
   */
  def child(childPosition: Int) = SectionNumber(position :+ childPosition)
  
  def withOptions (options: Options): SectionNumber = copy(options = options)
}

/** The root node of a navigation structure */
case class NavigationList (content: Seq[NavigationItem], options: Options = NoOpt) extends Block with ListContainer with RewritableContainer {
  type Self = NavigationList
  def rewriteChildren (rules: RewriteRules): NavigationList = copy(
    content = content.map(_.rewriteChildren(rules))
  )
  def withOptions (options: Options): NavigationList = copy(options = options)
}

/** Represents a recursive book navigation structure.
  */
trait NavigationItem extends Block with ListItem with ElementContainer[NavigationItem] with RewritableContainer {
  type Self <: NavigationItem
  def title: SpanSequence
}

/** Represents a book navigation entry that only serves as a section header without linking to content.
  */
case class NavigationHeader (title: SpanSequence, content: Seq[NavigationItem], options: Options = NoOpt) extends NavigationItem with ListContainer {
  
  type Self = NavigationHeader
  
  /** Returns the first link from the children of this navigation header.
    * This is useful for navigation systems where each entry must contain a concrete link.  */
  def firstLink: Option[NavigationLink] = content.collectFirst {
    case l: NavigationLink => Some(l)
    case h: NavigationHeader => h.firstLink
  }.flatten
  
  def rewriteChildren (rules: RewriteRules): NavigationHeader = copy(
    title = title.rewriteChildren(rules),
    content = content.map(_.rewriteChildren(rules))
  )
  def withOptions (options: Options): NavigationHeader = copy(options = options)
}

/** Represents a book navigation entry that links to content in the document tree.
  */
case class NavigationLink (title: SpanSequence, 
                           target: Target, 
                           content: Seq[NavigationItem],
                           selfLink: Boolean = false, 
                           options: Options = NoOpt) extends NavigationItem with ListContainer {
  type Self = NavigationLink
  def rewriteChildren (rules: RewriteRules): NavigationLink = copy(
    title = title.rewriteChildren(rules),
    content = content.map(_.rewriteChildren(rules))
  )
  def withOptions (options: Options): NavigationLink = copy(options = options)
}

/** A generic container element containing a list of blocks. Can be used where a sequence
 *  of blocks must be inserted in a place where a single element is required by the API.
 *  Usually renderers do not treat the container as a special element and render its children
 *  as s sub flow of the parent container.
 */
case class BlockSequence (content: Seq[Block], options: Options = NoOpt) extends Block with BlockContainer {
  type Self = BlockSequence
  def withContent (newContent: Seq[Block]): BlockSequence = copy(content = newContent)
  def withOptions (options: Options): BlockSequence = copy(options = options)
}
object BlockSequence extends BlockContainerCompanion {
  type ContainerType = BlockSequence
  override protected def createBlockContainer (blocks: Seq[Block]) = BlockSequence(blocks)
}

/** A generic container element containing a list of spans. Can be used where a sequence
 *  of spans must be inserted in a place where a single element is required by the API.
 *  Usually renderers do not treat the container as a special element and render its children
 *  as s sub flow of the parent container. A span sequence is special in that in can be
 *  used as both a span and a block.
 */
case class SpanSequence (content: Seq[Span], options: Options = NoOpt) extends Block with Span with SpanContainer {
  type Self = SpanSequence
  def withContent (newContent: Seq[Span]): SpanSequence = copy(content = newContent)
  def withOptions (options: Options): SpanSequence = copy(options = options)
}
object SpanSequence extends SpanContainerCompanion {
  type ContainerType = SpanSequence
  protected def createSpanContainer (spans: Seq[Span]): SpanSequence = SpanSequence(spans)
}


/** A paragraph consisting of span elements.
 */
case class Paragraph (content: Seq[Span], options: Options = NoOpt) extends Block with SpanContainer {
  type Self = Paragraph
  def withContent (newContent: Seq[Span]): Paragraph = copy(content = newContent)
  def withOptions (options: Options): Paragraph = copy(options = options)
}
object Paragraph extends SpanContainerCompanion {
  type ContainerType = Paragraph
  protected def createSpanContainer (spans: Seq[Span]): Paragraph = Paragraph(spans)
}

/** A literal block with unparsed text content.
 */
case class LiteralBlock (content: String, options: Options = NoOpt) extends Block with TextContainer {
  type Self = LiteralBlock
  def withOptions (options: Options): LiteralBlock = copy(options = options)
}

/** A literal block with parsed text content.
 */
case class ParsedLiteralBlock (content: Seq[Span], options: Options = NoOpt) extends Block with SpanContainer {
  type Self = ParsedLiteralBlock
  def withContent (newContent: Seq[Span]): ParsedLiteralBlock = copy(content = newContent)
  def withOptions (options: Options): ParsedLiteralBlock = copy(options = options)
}
object ParsedLiteralBlock extends SpanContainerCompanion {
  type ContainerType = ParsedLiteralBlock
  protected def createSpanContainer (spans: Seq[Span]): ParsedLiteralBlock = ParsedLiteralBlock(spans)
}

/** A block of program code. The content is a sequence of spans to support
 *  the integration of syntax highlighting systems. For unsupported languages
 *  the sequence will only consist of a single `Text` element.
 */
case class CodeBlock (language: String, content: Seq[Span], options: Options = NoOpt) extends Block with SpanContainer {
  type Self = CodeBlock
  def withContent (newContent: Seq[Span]): CodeBlock = copy(content = newContent)
  def withOptions (options: Options): CodeBlock = copy(options = options)
  def hasSyntaxHighlighting: Boolean = content match {
    case Seq(Text(_, _)) => false
    case _ => true
  }
}

/** A single span inside a code block that has been
  * categorized by a syntax highlighter.
  */
sealed trait CategorizedCode extends Span

/** A span of code associated with zero or more code categories.
 */
case class CodeSpan (content: String, categories: Set[CodeCategory], options: Options = NoOpt) extends CategorizedCode with TextContainer {
  type Self = CodeSpan
  def withOptions (options: Options): CodeSpan = copy(options = options)
}

object CodeSpan {

  def apply (content: String, category: CodeCategory): CodeSpan = apply(content, Set(category))

  def apply (content: String): CodeSpan = apply(content, Set(), NoOpt)

}

object CodeSpans {

  /** Extracts all code spans from the given span while at the same time
    * converting all regular text nodes to code spans associated with the specified
    * set of categories.
    * 
    * This is a fairly low-level operation, usually performed after using a generic
    * inline parser (like `InlineParsers.spans`) for syntax highlighting.
    */
  def extract (defaultCategories: Set[CodeCategory] = Set())(span: Span): Seq[CodeSpan] = span match {
    case Text(content, _)          => Seq(CodeSpan(content, defaultCategories))
    case codeSpan: CodeSpan        => Seq(codeSpan)
    case codeSeq: CodeSpanSequence => codeSeq.collect { case cs: CodeSpan => cs }
    case _                         => Nil
  }

  /** Merges all occurrences of two or more adjacent spans with the exact same set of 
    * associated code categories.
    */
  def merge (spans: Seq[CodeSpan]): Seq[CodeSpan] = {
    val filtered = spans.filterNot(_.content.isEmpty)
    if (filtered.isEmpty) Nil else {
      filtered.tail.foldLeft(List(filtered.head)) { case (acc, next) =>
        if (acc.last.categories == next.categories) acc.init :+ CodeSpan(acc.last.content + next.content, next.categories)
        else acc :+ next
      }
    }
  }

}

/** A sequence of code spans where most of them are usually associated with zero or more code categories.
  */
case class CodeSpanSequence (content: Seq[Span], options: Options = NoOpt) extends CategorizedCode with SpanContainer {
  type Self = CodeSpanSequence
  def withContent (newContent: Seq[Span]): CodeSpanSequence = copy(content = newContent)
  def withOptions (options: Options): CodeSpanSequence = copy(options = options)
}
object CodeSpanSequence extends SpanContainerCompanion {
  type ContainerType = CodeSpanSequence
  protected def createSpanContainer (spans: Seq[Span]): CodeSpanSequence = CodeSpanSequence(spans)
}

/** A quoted block consisting of a list of blocks that may contain other
 *  nested quoted blocks and an attribution which may be empty.
 */
case class QuotedBlock (content: Seq[Block], attribution: Seq[Span] = Nil, options: Options = NoOpt) extends Block
                                                                                               with BlockContainer {
  type Self = QuotedBlock
  
  override def rewriteChildren (rules: RewriteRules): QuotedBlock = 
    copy(content = rules.rewriteBlocks(content), attribution = rules.rewriteSpans(attribution))
  
  def withContent (newContent: Seq[Block]): QuotedBlock = copy(content = newContent)
  def withOptions (options: Options): QuotedBlock = copy(options = options)
}
object QuotedBlock extends BlockContainerCompanion {
  type ContainerType = QuotedBlock
  override protected def createBlockContainer (blocks: Seq[Block]) = QuotedBlock(blocks)
}

/** Generic block element with a title.
 *  Often combined with the the `styles` attribute of the `options` parameter to provide
 *  additional render hints.
 */
case class TitledBlock (title: Seq[Span], content: Seq[Block], options: Options = NoOpt) extends Block
                                                                                         with BlockContainer {
  type Self = TitledBlock

  override def rewriteChildren (rules: RewriteRules): TitledBlock = 
    copy(content = rules.rewriteBlocks(content), title = rules.rewriteSpans(title))
  
  def withContent (newContent: Seq[Block]): TitledBlock = copy(content = newContent)
  def withOptions (options: Options): TitledBlock = copy(options = options)
}

/** A figure consists of an image, an optional caption, and an optional legend as the `content` property.
 *  The `image` property is of type `Span` as the image might be wrapped inside a link reference.
 */
case class Figure (image: Span, caption: Seq[Span], content: Seq[Block], options: Options = NoOpt) extends Block with BlockContainer {

  type Self = Figure

  override def rewriteChildren (rules: RewriteRules): Figure = copy(
    content = rules.rewriteBlocks(content), 
    caption = rules.rewriteSpans(caption),
    image = rules.rewriteSpan(image)
  )
  
  def withContent (newContent: Seq[Block]): Figure = copy(content = newContent)
  def withOptions (options: Options): Figure = copy(options = options)
}

/** A bullet list that may contain nested lists.
 */
case class BulletList (content: Seq[BulletListItem], format: BulletFormat, options: Options = NoOpt) extends Block
                                                                                               with ListContainer
                                                                                               with RewritableContainer {
  type Self = BulletList
  
  override def rewriteChildren (rules: RewriteRules): BulletList =
    copy(content = content.map(_.rewriteChildren(rules)))

  def withOptions (options: Options): BulletList = copy(options = options)
}

/** An enumerated list that may contain nested lists.
 */
case class EnumList (content: Seq[EnumListItem], format: EnumFormat, start: Int = 1, options: Options = NoOpt) extends Block
                                                                                                           with ListContainer
                                                                                                           with RewritableContainer {
  type Self = EnumList
  
  override def rewriteChildren (rules: RewriteRules): EnumList =
    copy(content = content.map(_.rewriteChildren(rules)))

  def withOptions (options: Options): EnumList = copy(options = options)
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
                                                                                                with BlockContainer {
  type Self = BulletListItem
  def withContent (newContent: Seq[Block]): BulletListItem = copy(content = newContent)
  def withOptions (options: Options): BulletListItem = copy(options = options)
}

/** A single enum list item consisting of one or more block elements.
 */
case class EnumListItem (content: Seq[Block], format: EnumFormat, position: Int, options: Options = NoOpt) extends ListItem
                                                                                                    with BlockContainer {
  type Self = EnumListItem
  def withContent (newContent: Seq[Block]): EnumListItem = copy(content = newContent)
  def withOptions (options: Options): EnumListItem = copy(options = options)
}

/** A list of terms and their definitions.
 *  Not related to the `Definition` base trait.
 */
case class DefinitionList (content: Seq[DefinitionListItem], options: Options = NoOpt) extends Block with ListContainer
                                                                                                     with RewritableContainer {
  type Self = DefinitionList
  def rewriteChildren (rules: RewriteRules): DefinitionList = copy(content = content.map(_.rewriteChildren(rules)))
  def withOptions (options: Options): DefinitionList = copy(options = options)
}

/** A single definition item, containing the term and definition (as the content property).
 */
case class DefinitionListItem (term: Seq[Span], content: Seq[Block], options: Options = NoOpt) extends ListItem
                                                                                               with BlockContainer {
  type Self = DefinitionListItem

  override def rewriteChildren (rules: RewriteRules): DefinitionListItem = 
    copy(content = rules.rewriteBlocks(content), term = rules.rewriteSpans(term))
  
  def withContent (newContent: Seq[Block]): DefinitionListItem = copy(content = newContent)
  def withOptions (options: Options): DefinitionListItem = copy(options = options)
}

/** A single item inside a line block.
 */
abstract class LineBlockItem extends Block with RewritableContainer {
  type Self <: LineBlockItem
}

/** A single line inside a line block.
 */
case class Line (content: Seq[Span], options: Options = NoOpt) extends LineBlockItem with SpanContainer {
  type Self = Line
  def withContent (newContent: Seq[Span]): Line = copy(content = newContent)
  def withOptions (options: Options): Line = copy(options = options)
}
object Line extends SpanContainerCompanion {
  type ContainerType = Line
  protected def createSpanContainer (spans: Seq[Span]): Line = Line(spans)
}

/** A block containing lines which preserve line breaks and optionally nested line blocks.
 */
case class LineBlock (content: Seq[LineBlockItem], options: Options = NoOpt) extends LineBlockItem with ElementTraversal with RewritableContainer {
  type Self = LineBlock
  def rewriteChildren (rules: RewriteRules): LineBlock = copy(content = content.map(_.rewriteChildren(rules)))
  def withOptions (options: Options): LineBlock = copy(options = options)
}
object LineBlock {
  def apply(item: LineBlockItem, items: LineBlockItem*): LineBlock = LineBlock(item +: items.toList)
}


/** A table consisting of a head and a body part and optional caption and column specification.
 */
case class Table (head: TableHead, body: TableBody, caption: Caption = Caption(), columns: Columns = Columns(Nil), options: Options = NoOpt) extends Block
                                                                                                               with ElementTraversal with RewritableContainer {
  type Self = Table

  def rewriteChildren (rules: RewriteRules): Table = copy(head = head.rewriteChildren(rules), body = body.rewriteChildren(rules), caption = caption.rewriteChildren(rules))
 
  override def toString = "\n" + ASTRenderer.get.render(this) + "\n"

  def withOptions (options: Options): Table = copy(options = options)
}
object Table {

  /** Creates a table without header rows, all specified rows will 
    * be treated as rows of the table body.
    */
  def apply(row: Row, rows: Row*): Table = Table(TableHead(Nil), TableBody(row +: rows.toList))
}

/** A table element, like a row, cell or column.
 */
trait TableElement extends Customizable { type Self <: TableElement }

/** A container of table elements.
 */
trait TableContainer extends TableElement with ElementContainer[TableElement]  { type Self <: TableContainer }

/** Contains the header rows of a table.
 */
case class TableHead (content: Seq[Row], options: Options = NoOpt) extends TableElement with TableContainer with RewritableContainer {
  type Self = TableHead
  def rewriteChildren (rules: RewriteRules): TableHead = copy(content = content.map(_.rewriteChildren(rules)))
  def withOptions (options: Options): TableHead = copy(options = options)
}

/** Contains the body rows of a table.
 */
case class TableBody (content: Seq[Row], options: Options = NoOpt) extends TableElement with TableContainer with RewritableContainer {
  type Self = TableBody
  def rewriteChildren (rules: RewriteRules): TableBody = copy(content = content.map(_.rewriteChildren(rules)))
  def withOptions (options: Options): TableBody = copy(options = options)
}

/** The table caption.
 */
case class Caption (content: Seq[Span] = Nil, options: Options = NoOpt) extends TableElement with SpanContainer {
  type Self = Caption
  def withContent (newContent: Seq[Span]): Caption = copy(content = newContent)
  def withOptions (options: Options): Caption = copy(options = options)
}
object Caption extends SpanContainerCompanion {
  type ContainerType = Caption
  protected def createSpanContainer (spans: Seq[Span]): Caption = Caption(spans)
}

/** Contains the (optional) column specification of a table.
 */
case class Columns (content: Seq[Column], options: Options = NoOpt) extends TableElement with TableContainer {
  type Self = Columns
  def withOptions (options: Options): Columns = copy(options = options)
}

/** Convenient factory for creating a `Columns` instance based on the options
 *  for the individual columns.
 */
object Columns {
  def options (options: Options*): Columns = Columns(options map Column)
}

/** The options (like styles) for a column table.
 */
case class Column (options: Options = NoOpt) extends TableElement {
  type Self = Column
  def withOptions (options: Options): Column = copy(options = options)
}

/** A single table row. In case some of the previous rows contain
 *  cells with a colspan greater than 1, this row may contain
 *  fewer cells than the number of columns in the table.
 */
case class Row (content: Seq[Cell], options: Options = NoOpt) extends TableElement with TableContainer with RewritableContainer {
  type Self = Row
  def rewriteChildren (rules: RewriteRules): Row = copy(content = content.map(_.rewriteChildren(rules)))
  def withOptions (options: Options): Row = copy(options = options)
}
object Row {
  def apply(cell: Cell, cells: Cell*): Row = Row(cell +: cells.toList)
}

/** A single cell, potentially spanning multiple rows or columns, containing
 *  one or more block elements.
 */
case class Cell (cellType: CellType, content: Seq[Block], colspan: Int = 1, rowspan: Int = 1, options: Options = NoOpt) extends TableElement
                                                                                                                        with BlockContainer {
  type Self = Cell
  def withContent (newContent: Seq[Block]): Cell = copy(content = newContent)
  def withOptions (options: Options): Cell = copy(options = options)
}

/** The cell type specifies which part of the table the cell belongs to.
 */
sealed abstract class CellType

/** A cell in the head part of the table.
 */
case object HeadCell extends CellType with BlockContainerCompanion {
  type ContainerType = Cell
  def apply(blocks: Seq[Block]): Cell = Cell(this, blocks)
  protected def createBlockContainer (blocks: Seq[Block]) = Cell(this, blocks)
}

/** A cell in the body part of the table.
 */
case object BodyCell extends CellType with BlockContainerCompanion {
  type ContainerType = Cell
  def apply(blocks: Seq[Block]): Cell = Cell(this, blocks)
  protected def createBlockContainer (blocks: Seq[Block]) = Cell(this, blocks)
}

/** An internal or external link target that can be referenced by id, usually only part of the raw document tree and then
  * removed by the rewrite rule that resolves link and image references.
  */
case class LinkDefinition (id: String, target: Target, title: Option[String] = None, options: Options = NoOpt) extends Definition with Hidden
  with Span {
  type Self = LinkDefinition
  def withOptions (options: Options): LinkDefinition = copy(options = options)
}

/** A link target pointing to another link target, acting like an alias.
 */
case class LinkAlias (id: String, target: String, options: Options = NoOpt) extends Definition with Span with Hidden {
  type Self = LinkAlias
  def withOptions (options: Options): LinkAlias = copy(options = options)
}

/** A footnote definition that needs to be resolved to a final footnote
 *  by a rewrite rule based on the label type.
 */
case class FootnoteDefinition (label: FootnoteLabel, content: Seq[Block], options: Options = NoOpt) extends Definition
                                                                                                    with BlockContainer
                                                                                                    with Unresolved {
  type Self = FootnoteDefinition
  def withContent (newContent: Seq[Block]): FootnoteDefinition = copy(content = newContent)
  def withOptions (options: Options): FootnoteDefinition = copy(options = options)
  lazy val unresolvedMessage: String = s"Unresolved footnote definition with label '$label'"
}


/** Points to the following block or span element, making it a target for links.
 */
case class InternalLinkTarget (options: Options = NoOpt) extends Block with Span with LinkTarget {
  type Self = InternalLinkTarget
  def withOptions (options: Options): InternalLinkTarget = copy(options = options)
}

/** A citation that can be referred to by a `CitationLink` by id.
 */
case class Citation (label: String, content: Seq[Block], options: Options = NoOpt) extends Block
                                                                                with LinkTarget
                                                                                with BlockContainer {
  type Self = Citation
  def withContent (newContent: Seq[Block]): Citation = copy(content = newContent)
  def withOptions (options: Options): Citation = copy(options = options)
}

/** A footnote with resolved id and label that can be referred to by a `FootnoteLink` by id.
 */
case class Footnote (label: String, content: Seq[Block], options: Options = NoOpt) extends Block
                                                                                               with LinkTarget
                                                                                               with BlockContainer {
  type Self = Footnote
  def withContent (newContent: Seq[Block]): Footnote = copy(content = newContent)
  def withOptions (options: Options): Footnote = copy(options = options)
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
  * Can be used as both block and inline element. 
  * If supported by a parser it usually has to be explicitly enabled due to security concerns.
  */
case class RawContent (formats: NonEmptySet[String], content: String, options: Options = NoOpt) extends Block with Span with TextContainer {
  type Self = RawContent
  def withOptions (options: Options): RawContent = copy(options = options)
}

/** A horizontal rule.
 */
case class Rule (options: Options = NoOpt) extends Block {
  type Self = Rule
  def withOptions (options: Options): Rule = copy(options = options)
}



/** A simple text element.
 */
case class Text (content: String, options: Options = NoOpt) extends Span with TextContainer {
  type Self = Text
  def withOptions (options: Options): Text = copy(options = options)  
}

/** A span of emphasized inline elements that may contain nested spans.
 */
case class Emphasized (content: Seq[Span], options: Options = NoOpt) extends Span with SpanContainer {
  type Self = Emphasized
  def withContent (newContent: Seq[Span]): Emphasized = copy(content = newContent)
  def withOptions (options: Options): Emphasized = copy(options = options)
}
object Emphasized extends SpanContainerCompanion {
  type ContainerType = Emphasized
  protected def createSpanContainer (spans: Seq[Span]): Emphasized = Emphasized(spans)
}

/** A span of strong inline elements that may contain nested spans.
 */
case class Strong (content: Seq[Span], options: Options = NoOpt) extends Span with SpanContainer {
  type Self = Strong
  def withContent (newContent: Seq[Span]): Strong = copy(content = newContent)
  def withOptions (options: Options): Strong = copy(options = options)
}
object Strong extends SpanContainerCompanion {
  type ContainerType = Strong
  protected def createSpanContainer (spans: Seq[Span]): Strong = Strong(spans)
}

/** A span containing plain, unparsed text.
 */
case class Literal (content: String, options: Options = NoOpt) extends Span with TextContainer {
  type Self = Literal
  def withOptions (options: Options): Literal = copy(options = options)
}

/** A span of program code. The content is a sequence of spans to support
 *  the integration of syntax highlighting systems. Without this support
 *  the sequence will only consist of a single `Text` element.
 */
case class InlineCode (language: String, content: Seq[Span], options: Options = NoOpt) extends Span with SpanContainer {
  type Self = InlineCode
  def withContent (newContent: Seq[Span]): InlineCode = copy(content = newContent)
  def withOptions (options: Options): InlineCode = copy(options = options)
}

/** A span representing deleted inline elements that may contain nested spans.
  */
case class Deleted (content: Seq[Span], options: Options = NoOpt) extends Span with SpanContainer {
  type Self = Deleted
  def withContent (newContent: Seq[Span]): Deleted = copy(content = newContent)
  def withOptions (options: Options): Deleted = copy(options = options)
}
object Deleted extends SpanContainerCompanion {
  type ContainerType = Deleted
  protected def createSpanContainer (spans: Seq[Span]): Deleted = Deleted(spans)
}

/** A span representing inserted inline elements that may contain nested spans.
  */
case class Inserted (content: Seq[Span], options: Options = NoOpt) extends Span with SpanContainer {
  type Self = Inserted
  def withContent (newContent: Seq[Span]): Inserted = copy(content = newContent)
  def withOptions (options: Options): Inserted = copy(options = options)
}
object Inserted extends SpanContainerCompanion {
  type ContainerType = Inserted
  protected def createSpanContainer (spans: Seq[Span]): Inserted = Inserted(spans)
}

/** Represents a target that can be referred to by links,
  * either within the virtual tree or external.
  */
sealed trait Target {
  def render (internalTargetsAbsolute: Boolean = false): String
}

/** An external link, outside of the virtual tree of the current transformation.
  */
case class ExternalTarget (url: String) extends Target {
  def render (internalTargetsAbsolute: Boolean = false): String = url
}

/** Represents an internal target with an absolute and relative path, the latter
  * relative to the document that referred to the target. 
  * 
  * The optional `externalUrl` property can be set for links which are internal
  * on a rendered site, but external for e-books like EPUB and PDF.
  */
case class InternalTarget (absolutePath: Path, relativePath: RelativePath, externalUrl: Option[String] = None) extends Target {
  def relativeTo (refPath: Path): InternalTarget = InternalTarget.fromPath(relativePath, refPath)
  def render (internalTargetsAbsolute: Boolean = false): String = 
    if (internalTargetsAbsolute) absolutePath.toString
    else relativePath.toString
}
object Target {
  
  /** Creates a new target from the specified URL.
    * 
    * If the target is an absolute URL (starting with '/' or 'http'/'https') the
    * result will be an external target. 
    * 
    * Relative URLs will be interpreted as pointing to the target within the virtual tree of input and output
    * documents and will be validated during transformation, 
    * resulting in errors if the target does not exist. 
    * 
    * External targets on the other hand are not validated, 
    * as the availability of the external resource during validation cannot be guaranteed.
    */
  def create (url: String): Target = 
    if (url.startsWith("http:") || url.startsWith("https:") || url.startsWith("/")) ExternalTarget(url)
    else InternalTarget(Root, RelativePath.parse(url))
}
object InternalTarget {
  
  /** Creates an instance for the specified path relative to
    * the provided reference path.
    */
  def fromPath (path: PathBase, refPath: Path): InternalTarget = path match {
    case p: Path            => InternalTarget(p, p.relativeTo(refPath))
    case p: CurrentDocument => InternalTarget(refPath / p, p)
    case p: RelativePath    => InternalTarget(refPath.parent / p, p)
  }
}

/** An link element, with the span content representing the text (description) of the link.
  */
case class SpanLink (content: Seq[Span], target: Target, title: Option[String] = None, options: Options = NoOpt) extends Link
  with SpanContainer {
  type Self = SpanLink
  def withContent (newContent: Seq[Span]): SpanLink = copy(content = newContent)
  def withOptions (options: Options): SpanLink = copy(options = options)
}

/** A resolved link to a footnote.
 */
case class FootnoteLink (ref: String, label: String, options: Options = NoOpt) extends Link {
  type Self = FootnoteLink
  def withOptions (options: Options): FootnoteLink = copy(options = options)
}

/** A resolved link to a citation.
 */
case class CitationLink (ref: String, label: String, options: Options = NoOpt) extends Link {
  type Self = CitationLink
  def withOptions (options: Options): CitationLink = copy(options = options)
}

/** An inline image with a text description and optional title.
 */
case class Image (text: String,
                  target: Target,
                  width: Option[Size] = None,
                  height: Option[Size] = None,
                  title: Option[String] = None,
                  options: Options = NoOpt) extends Link {
  type Self = Image
  def withOptions (options: Options): Image = copy(options = options)
}

object Image {
  def create (text: String, url: String, source: String, width: Option[Size] = None,
              height: Option[Size] = None, title: Option[String] = None): Span =
    Target.create(url) match {
      case et: ExternalTarget => Image(text, et, width, height, title)
      case it: InternalTarget => ImagePathReference(text, it.relativePath, source, width, height, title)
    }
}

/** Encapsulates size information with a CSS-compatible length unit.
  */
case class Size (amount: Double, unit: LengthUnit) {
  def scale (percent: Double): Size = copy(amount * percent / 100)
  def displayValue: String = amount.toString.stripSuffix(".0") + unit.displayValue
}

/** A base for builder of CSS-compatible length units.
  */
sealed abstract class LengthUnit (val displayValue: String) {
  def apply(amount: Double): Size = Size(amount, this)
}
object LengthUnit {
  object px extends LengthUnit("px")
  object mm extends LengthUnit("mm")
  object cm extends LengthUnit("cm")
  object in extends LengthUnit("in")
  object pc extends LengthUnit("pc")
  object pt extends LengthUnit("pt")
  object ch extends LengthUnit("ch")
  object em extends LengthUnit("em")
  object ex extends LengthUnit("ex")
  object rem extends LengthUnit("rem")
  object vh extends LengthUnit("vh")
  object vw extends LengthUnit("vw")
  object vmin extends LengthUnit("vmin")
  object vmax extends LengthUnit("vmax")
  object percent extends LengthUnit("%")
  
  private val all: Map[String, LengthUnit] = 
    Seq(px,mm,cm,in,pc,pt,ch,em,ex,rem,vh,vw,vmin,vmax,percent).map(u => (u.displayValue, u)).toMap
  def fromString (value: String): Option[LengthUnit] = all.get(value)
}

/** Represents a font-based icon, identified by its code point.
  * Ideally theme authors provide constants for icons provided out of the box,
  * so that the user does not have to look up or memorize the hex code point.
  * 
  * This avoids the indirection of common approaches where the rendered HTML contains
  * an empty tag with a class which specifies the code point with a `:before` pseudo-class.
  * This approach would currently not work well with Laika's PDF support which is
  * not based on an interim HTML renderer.
  */
case class Icon (codePoint: Char, options: Options = NoOpt) extends Span {
  def codePointAsEntity: String = s"&#x${Integer.toHexString(codePoint)};"
  type Self = Icon
  def withOptions(newOptions: Options): Icon = copy(options = newOptions)
}

object Link {
  /** Creates a new span that acts as a link reference based on the specified
    * URL which will be parsed and interpreted as an internal or external target.
    */
  def create (linkText: Seq[Span], url: String, source: String, title: Option[String] = None): Span =
    Target.create(url) match {
      case et: ExternalTarget => SpanLink(linkText, et, title)
      case it: InternalTarget => LinkPathReference(linkText, it.relativePath, source, title)
    }
}

object LinkDefinition {
  /** Creates a new link definition that other references can point to based on the specified
    * URL which will be parsed and interpreted as an internal or external target.
    */
  def create (id: String, url: String, title: Option[String] = None): Block with Span = 
    LinkDefinition(id, Target.create(url), title)
}

/** A reference to content within the virtual input tree, the path pointing to the source path.
  * Only part of the unresolved document tree and then removed by the rewrite rule that 
  * replace the source path with the final target path of the output document, which might
  * differ in more than just the file suffix, depending on configuration.
  */
trait PathReference extends Reference {
  /** The content (section or document or image) this reference points to. */
  def path: RelativePath
  /** Creates the final AST element based on the resolved target. */
  def resolve(target: Target): Span
}

/** A reference to content within the virtual input tree, the path pointing to the source path.
  * Only part of the unresolved document tree and then removed by the rewrite rule that 
  * replace the source path with the final target path of the output document, which might
  * differ in more than just the file suffix, depending on configuration.
  */
case class LinkPathReference(content: Seq[Span],
                             path: RelativePath,
                             source: String,
                             title: Option[String] = None,
                             options: Options = NoOpt) extends PathReference with SpanContainer {
  type Self = LinkPathReference
  def withContent (newContent: Seq[Span]): LinkPathReference = copy(content = newContent)
  def withOptions (options: Options): LinkPathReference = copy(options = options)
  def resolve(target: Target): Span = SpanLink(content, target, title, options)
  lazy val unresolvedMessage: String = s"Unresolved internal reference to '${path.toString}'"
}

/** An image reference to content within the virtual input tree, the path pointing to the source path.
  * Only part of the unresolved document tree and then removed by the rewrite rule that 
  * replace the source path with the final target path of the output document, resolving any
  * relative path references in the process.
  */
case class ImagePathReference (text: String,
                               path: RelativePath,
                               source: String,
                               width: Option[Size] = None,
                               height: Option[Size] = None,
                               title: Option[String] = None,
                               options: Options = NoOpt) extends PathReference {
  type Self = ImagePathReference
  def withOptions (options: Options): ImagePathReference = copy(options = options)
  def resolve(target: Target): Span = Image(text, target, width, height, title, options)
  lazy val unresolvedMessage: String = s"Unresolved internal reference to image with path '$path'"
}

/** An image reference, the id pointing to the id of a `LinkTarget`. Only part of the
 *  raw document tree and then removed by the rewrite rule that resolves link and image references.
 */
case class ImageIdReference (text: String, id: String, source: String, options: Options = NoOpt) extends Reference {
  type Self = ImageIdReference
  def withOptions (options: Options): ImageIdReference = copy(options = options)
  lazy val unresolvedMessage: String = s"Unresolved reference to image definition with id '$id'"
}

/** A reference to a footnote with a matching label.  Only part of the
 *  raw document tree and then removed by the rewrite rule that resolves link and image references.
 */
case class FootnoteReference (label: FootnoteLabel, source: String, options: Options = NoOpt) extends Reference {
  type Self = FootnoteReference
  def withOptions (options: Options): FootnoteReference = copy(options = options)
  lazy val unresolvedMessage: String = s"Unresolved reference to footnote with label '$label'"
}

/** A reference to a citation with a matching label.  Only part of the
 *  raw document tree and then removed by the rewrite rule that resolves link and image references.
 */
case class CitationReference (label: String, source: String, options: Options = NoOpt) extends Reference {
  type Self = CitationReference
  def withOptions (options: Options): CitationReference = copy(options = options)
  lazy val unresolvedMessage: String = s"Unresolved reference to citation with label '$label'"
}

/** A reference to any kind of referencable object, e.g. a link definition or an internal target.
  * 
  * The reference can be local, in the same document, or anywhere else in the input tree, as long
  * as the id is not ambiguous. 
  * Search for a matching target happens recursively, from the current document, 
  * to the current tree (directory) upwards to the root tree.
  */
case class LinkIdReference (content: Seq[Span], ref: String, source: String, options: Options = NoOpt) extends Reference
  with SpanContainer {
  type Self = LinkIdReference
  def withContent (newContent: Seq[Span]): LinkIdReference = copy(content = newContent)
  def withOptions (options: Options): LinkIdReference = copy(options = options)
  lazy val unresolvedMessage: String = s"Unresolved link id reference '$ref'"
}


/** An explicit hard line break.
 */
case class LineBreak (options: Options = NoOpt) extends Span {
  type Self = LineBreak
  def withOptions (options: Options): LineBreak = copy(options = options)
}

/** An explicit hard page break.
 */
case class PageBreak (options: Options = NoOpt) extends Block {
  type Self = PageBreak
  def withOptions (options: Options): PageBreak = copy(options = options)
}

/** A comment that may be omitted by renderers.
 */
case class Comment (content: String, options: Options = NoOpt) extends Block with TextContainer {
  type Self = Comment
  def withOptions (options: Options): Comment = copy(options = options)
}

/** Message generated by the parser, a directive or a rewrite rule.
  * 
  * They usually get inserted immediately after the block or span that caused the problem.
  * It mixes in both the Span and Block trait so that it can appear in sequences of both types.
  * By default messages are ignored by most renderers (apart from AST), but
  * they can be explicitly activated for a particular level.
  *  
  * A message of level `MessageLevel.Error` will cause a transformation to fail, unless
  * the user has configured with the `renderErrors` method to debug in a visual mode 
  * in which case the errors will get rendered in-place in the output.
 */
case class RuntimeMessage (level: MessageLevel, content: String, options: Options = NoOpt) extends Span 
                                                                                          with Block 
                                                                                          with TextContainer {
  type Self = RuntimeMessage
  def withOptions (options: Options): RuntimeMessage = copy(options = options)
}

/** Signals the severity of a runtime message.
 */
sealed abstract class MessageLevel (private val level: Int) extends Ordered[MessageLevel] {
  def compare(that: MessageLevel): Int = level compare that.level
}

/** Enumeration of available message levels.
  * 
  * The library's internal parsers and AST transformation only use the `Error` level for recoverable issues
  * encountered during transformations. All other levels are available for user code.
  */
object MessageLevel {

  /** Debug level that is not used by the library itself, but may be used by application code for debugging purposes.
   */
  case object Debug extends MessageLevel(0)

  /** Info level that is not used by the library itself, but may be used by application code for debugging purposes.
   */
  case object Info extends MessageLevel(1)

  /** An issue that hints at a potential problem, but in the library's default settings it won't
    * cause the transformation to fail.
    */
  case object Warning extends MessageLevel(2)

  /** An error that is confined to a single AST node or range of nodes, but most likely with surrounding
    * areas unaffected. An example is an internal link that remained unresolved.
   */
  case object Error extends MessageLevel(3)

  /** A critical issue that might affect the integrity of the entire output beyond just the node where it occurred.
    * An example is a configuration header with parsing errors in a markup document that might affect other markup
    * content that would then unexpectedly fall back to defaults.
    */
  case object Fatal extends MessageLevel(4)
}

/** A filter for runtime messages that meet a specified minimum message level.
  */
sealed trait MessageFilter {
  def apply (message: RuntimeMessage): Boolean
}

object MessageFilter {
  case object None extends MessageFilter {
    def apply (message: RuntimeMessage) = false
  }
  // TODO - 0.16 - can be fully private after deprecations get removed
  private[laika] def forLevel (level: MessageLevel) = new MessageFilter {
    def apply (message: RuntimeMessage) = message.level >= level
  }
  val Debug: MessageFilter = forLevel(MessageLevel.Debug)
  val Info: MessageFilter = forLevel(MessageLevel.Info)
  val Warning: MessageFilter = forLevel(MessageLevel.Warning)
  val Error: MessageFilter = forLevel(MessageLevel.Error)
  val Fatal: MessageFilter = forLevel(MessageLevel.Fatal)
}

/** Groups a span that could not be successfully parsed with a runtime message.
 *  Renderers may then choose to just render the fallback, the message or both.
 */
case class InvalidSpan (message: RuntimeMessage, fallback: Span, options: Options = NoOpt) extends Span with Invalid[Span] {
  type Self = InvalidSpan
  def withOptions (options: Options): InvalidSpan = copy(options = options)
}

/** Groups a block that could not be successfully parsed with a runtime message.
 *  Renderers may then choose to just render the fallback, the message or both.
 */
case class InvalidBlock (message: RuntimeMessage, fallback: Block, options: Options = NoOpt) extends Block with Invalid[Block] {
  type Self = InvalidBlock
  def withOptions (options: Options): InvalidBlock = copy(options = options)
}

/** Represents an invalid element in any position, block, span or template.
  * Provides convenience converters to produce instances for any of these three positions.
  */
case class InvalidElement (message: RuntimeMessage, source: String) extends Element {

  def asBlock: InvalidBlock = InvalidBlock(message, LiteralBlock(source))

  def asSpan: InvalidSpan = InvalidSpan(message, Text(source))

  def asTemplateSpan: TemplateSpan = TemplateElement(asSpan)

}

/** Companion for InvalidElement. */
object InvalidElement {
  def apply (message: String, source: String): InvalidElement =
    apply(RuntimeMessage(MessageLevel.Error, message), source)
}

/** A special type of paragraph that serves as a render hint.
 *  Some renderers simplify the rendering of block elements containing only a single
 *  paragraph and render the span content inline (e.g. a `&lt;li&gt;` tag without a nested `&lt;p&gt;` tag
 *  for the paragraph).
 *  Using this element as mandated by some edge cases in both the Markdown and reStructuredText
 *  markup definitions prevents this.
 */
case class ForcedParagraph (content: Seq[Span], options: Options = NoOpt) extends Block
                                                with SpanContainer with Fallback {
  type Self = ForcedParagraph

  def withContent (newContent: Seq[Span]): ForcedParagraph = copy(content = newContent)
  def withOptions (options: Options): ForcedParagraph = copy(options = options)
  
  def fallback: Element = Paragraph(content, options)
}

case class Reverse (length: Int, target: Span, fallback: Span, options: Options = NoOpt) extends Span {
  type Self = Reverse
  def withOptions (options: Options): Reverse = copy(options = options)
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
  def apply (values: Set[String]): Options = SomeOpt(styles = values)
  def unapplySeq (value: Options): Option[Seq[String]] = Some(value.styles.toSeq)
}

private[ast] object ASTRenderer {
  lazy val get: Renderer = Renderer.of(AST).build
}

/** Companion for the Options trait.
 */
object Options {
  def apply (id: Option[String] = None, styles: Set[String] = Set()): Options =
    if (id.isEmpty && styles.isEmpty) NoOpt
    else SomeOpt(id,styles)
}

/** Constants for style names wrapped in Options instances which are commonly used by Laika's core parsers and rewrite rules. */
object Style {
  
  val title: Options = Styles("title")
  val section: Options = Styles("section")
  val sectionNumber: Options = Styles("section-number") // TODO - is camel case right now
  val nav: Options = Styles("nav")
  val navHeader: Options = Styles("nav-header")
  val navList: Options = Styles("nav-list")
  val active: Options = Styles("active") // TODO - rename? self-link?
  val breadcrumb: Options = Styles("breadcrumb")
  val bookmark: Options = Styles("bookmark")
  def level(lev: Int): Options = Styles("level" + lev)
  
  val alignCenter: Options = Styles("align-center")
  val alignLeft: Options = Styles("align-left")
  val alignRight: Options = Styles("align-right")
  val keepTogether: Options = Styles("keep-together")
  
  val citation: Options = Styles("citation")
  val footnote: Options = Styles("footnote")
  val footnoteLabel: Options = Styles("footnote-label")
  
  val label: Options = Styles("label")
  val attribution: Options = Styles("attribution")
  val caption: Options = Styles("caption")
  val legend: Options = Styles("legend")
  val figure: Options = Styles("figure")
  
  val runtimeMessage: Options = Styles("runtime-message")
  val noHighlight: Options = Styles("nohighlight")
  
}
