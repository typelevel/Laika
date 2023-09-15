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

package laika.format

import laika.api.format.MarkupFormat
import laika.ast.Block
import laika.bundle.{ BlockParserBuilder, ExtensionBundle, SpanParserBuilder }
import laika.markdown.bundle.VerbatimHTML
import laika.markdown.{ BlockParsers, InlineParsers, ListParsers }
import laika.parse.Parser
import laika.parse.text.{ CharGroup, TextParsers }

/** A parser for Markdown text. Instances of this class may be passed directly
  * to the `Parser` or `Transformer` APIs:
  *
  *  {{{
  *  val document = MarkupParser.of(Markdown).build.parse(inputString)
  *
  *  Transformer.from(Markdown).to(HTML).build.transform(inputString)
  *  }}}
  *
  *  Since this library is not solely focused on producing HTML output,
  *  parsing verbatim HTML elements like defined by the official Markdown
  *  syntax description is an optional feature, as some types of renderers
  *  would not know what to do with HTML nodes in the document tree.
  *  It must be enabled explicitly:
  *
  *  {{{
  *  val parser = MarkupParser.of(Markdown).withRawContent.build
  *  }}}
  *
  *  To switch off all custom extensions like directives,
  *  configuration sections at the start of the document or automatic
  *  id generation for headers, you can run the parser in strict mode:
  *
  *  {{{
  *  val transformer = Transformer.from(Markdown).to(HTML).strict
  *  }}}
  *
  *  @author Jens Halm
  */
case object Markdown extends MarkupFormat {

  val fileSuffixes: Set[String] = Set("md", "markdown")

  object blockParsers extends MarkupFormat.MarkupParsers[BlockParserBuilder] {

    /** Parses an ATX header, a line that starts with 1 to 6 `'#'` characters,
      * with the number of hash characters corresponding to the level of the header.
      * Markdown also allows to decorate the line with trailing `'#'` characters which
      * this parser will remove.
      */
    val atxHeader: BlockParserBuilder = BlockParsers.atxHeader

    /** Parses either a setext header, or a plain paragraph if the second line of the block
      * is not a setext header decoration.
      * Only used for root level blocks where lists starting in the middle of a paragraph are not allowed.
      */
    lazy val rootHeaderOrParagraph: BlockParserBuilder = BlockParsers.rootHeaderOrParagraph

    /** Parses either a setext header, or a plain paragraph if the second line of the block
      * is not a setext header decoration.
      * Only used for nested blocks where lists starting in the middle of a paragraph are allowed.
      */
    lazy val nestedHeaderOrParagraph: BlockParserBuilder = BlockParsers.nestedHeaderOrParagraph

    /** Parses a bullet list, called "unordered list" in the Markdown syntax description.
      */
    val bulletList: BlockParserBuilder = ListParsers.bulletLists

    /** Parses an enumerated list, called "ordered list" in the Markdown syntax description.
      */
    val enumList: BlockParserBuilder = ListParsers.enumLists

    /** Parses a quoted block, a paragraph starting with a `'>'` character,
      * with subsequent lines optionally starting with a `'>'`, too.
      */
    val quotedBlock: BlockParserBuilder = BlockParsers.quotedBlock

    /** Parses a literal block, text indented by a tab or 4 spaces.
      */
    val literalBlock: BlockParserBuilder = BlockParsers.literalBlocks

    /** Parses a link definition in the form `[id]: <url> "title"`.
      * The title is optional as well as the quotes around it and the angle brackets around the url.
      */
    val linkTarget: BlockParserBuilder = BlockParsers.linkTarget

    /** Parses a horizontal rule, a line only decorated with three or more `'*'`, `'-'` or `'_'`
      * characters with optional spaces between them
      */
    val rule: BlockParserBuilder = BlockParsers.rules

    val all: Seq[BlockParserBuilder] = Seq(
      atxHeader.interruptsParagraphWith(TextParsers.oneOf('#')),
      linkTarget,
      quotedBlock,
      rootHeaderOrParagraph,
      nestedHeaderOrParagraph,
      BlockParsers.fallbackParagraph,
      literalBlock,
      rule,
      enumList.rootOnly,
      enumList.nestedOnly.interruptsParagraphWith(TextParsers.oneOf(CharGroup.digit)),
      bulletList.interruptsParagraphWith(TextParsers.oneOf('+', '*', '-'))
    )

  }

  object spanParsers extends MarkupFormat.MarkupParsers[SpanParserBuilder] {

    /** Parses either strong spans enclosed in double asterisks or emphasized spans enclosed in single asterisks.
      */
    val enclosedByAsterisk: SpanParserBuilder = InlineParsers.enclosedByAsterisk

    /** Parses either strong spans enclosed in double underscores or emphasized spans enclosed in single underscores.
      */
    val enclosedByUnderscore: SpanParserBuilder = InlineParsers.enclosedByUnderscore

    /** Parses a link, including nested spans in the link text.
      * Recognizes both, an inline link `[text](url)` and a link reference `[text][id]`.
      */
    lazy val link: SpanParserBuilder = InlineParsers.link

    /** Parses a simple inline link in the form of &lt;http://someURL/&gt;
      */
    val simpleLink: SpanParserBuilder = InlineParsers.simpleLink

    /** Parses a literal span enclosed by one or more backticks.
      * Does neither parse nested spans nor Markdown escapes.
      */
    val literalSpan: SpanParserBuilder = InlineParsers.literalSpan

    /** Parses an inline image.
      * Recognizes both, an inline image `![text](url)` and an image reference `![text][id]`.
      */
    val image: SpanParserBuilder = InlineParsers.image

    /** Parses an explicit hard line break.
      */
    val lineBreak: SpanParserBuilder = InlineParsers.lineBreak

    val all: Seq[SpanParserBuilder] = Seq(
      enclosedByAsterisk,
      enclosedByUnderscore,
      literalSpan,
      image,
      link,
      simpleLink,
      lineBreak
    )

  }

  override lazy val escapedChar: Parser[String] = InlineParsers.escapedChar

  val extensions: Seq[ExtensionBundle] = Seq(VerbatimHTML)

  override def createBlockListParser(parser: Parser[Block]): Parser[Seq[Block]] =
    super.createBlockListParser(BlockParsers.insignificantSpaces ~> parser)

}
