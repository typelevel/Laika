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
import laika.config.{ConfigEncoder, ConfigValue}

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
