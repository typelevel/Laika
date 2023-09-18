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

import laika.api.bundle.{
  BlockParserBuilder,
  BundleOrigin,
  ExtensionBundle,
  ParserBundle,
  ParserHooks,
  SpanParserBuilder
}
import laika.api.format.MarkupFormat
import laika.ast.Block
import laika.internal.rst.{
  BlockParsers,
  ExplicitBlockParsers,
  InlineParsers,
  ListParsers,
  TableParsers
}
import laika.internal.rst.bundle.{
  DocInfoExtractor,
  ExtendedHTMLRenderer,
  LinkTargetProcessor,
  RawContentExtensions,
  RstExtensionSupport,
  StandardExtensions
}
import laika.parse.Parser
import laika.parse.text.WhitespacePreprocessor

/** A parser for text written in reStructuredText markup. Instances of this class may be passed directly
  * to the `Parseer` or `Transformer` APIs:
  *
  * {{{
  * val document = MarkupParser.of(ReStructuredText).build.parse(inputString)
  *
  * Transformer.from(ReStructuredText).to(HTML).build.transform(inputString)
  * }}}
  *
  * In addition to the implementing the standard reStructuredText directives,
  * the Laika APIs also supports a custom directive type unique to Laika.
  * They represent a library-wide extension mechanism and allow you to implement
  * tags which can be used in any of the supported markup formats or in templates.
  *
  * Laika directives can be registered with the [[laika.api.bundle.DirectiveRegistry]] extension bundle.
  *
  * @author Jens Halm
  */
case object ReStructuredText extends MarkupFormat { self =>

  override val description: String = "reStructuredText"

  val fileSuffixes: Set[String] = Set("rest", "rst")

  object blockParsers extends MarkupFormat.MarkupParsers[BlockParserBuilder] {

    /** Parses a bullet list with any of the supported bullet characters.
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#bullet-lists]].
      */
    lazy val bulletList: BlockParserBuilder = ListParsers.bulletList

    /** Parses an enumerated list in any of the supported combinations of enumeration style and formatting.
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#enumerated-lists]].
      */
    lazy val enumList: BlockParserBuilder = ListParsers.enumList

    /** Parses a field list.
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#field-lists]].
      */
    lazy val fieldList: BlockParserBuilder = ListParsers.fieldList

    /** Parses a definition list.
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#definition-lists]].
      */
    lazy val definitionList: BlockParserBuilder = ListParsers.definitionList

    /** Parses an option list.
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#option-lists]].
      */
    lazy val optionList: BlockParserBuilder = ListParsers.optionList

    /** Parses a block of lines with line breaks preserved.
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#line-blocks]].
      */
    lazy val lineBlock: BlockParserBuilder = ListParsers.lineBlock

    /** Parses a grid table.
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#grid-tables]].
      */
    lazy val gridTable: BlockParserBuilder = TableParsers.gridTable

    /** Parses a simple table.
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#simple-tables]].
      */
    lazy val simpleTable: BlockParserBuilder = TableParsers.simpleTable

    /** Parses a block quote with an optional attribution.
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#block-quotes]]
      */
    lazy val blockQuote: BlockParserBuilder = BlockParsers.blockQuote

    /** Parses a section header with both overline and underline.
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#sections]].
      */
    lazy val headerWithOverline: BlockParserBuilder = BlockParsers.headerWithOverline

    /** Parses a section header with an underline, but no overline.
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#sections]].
      */
    lazy val headerWithUnderline: BlockParserBuilder = BlockParsers.headerWithUnderline

    /** Parses a transition (rule).
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#transitions]].
      */
    val transition: BlockParserBuilder = BlockParsers.transition

    /** Parses a doctest block.
      * This is a feature which is very specific to the world of Python where reStructuredText originates.
      * Therefore the resulting `DoctestBlock` tree element is not part of the standard Laika AST model.
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#doctest-blocks]]
      */
    val doctest: BlockParserBuilder = BlockParsers.doctest

    /** The parser builder for all explicit block items that start with `..` except
      * for directives which are provided by an extension.
      */
    val explicitBlocks: BlockParserBuilder = ExplicitBlockParsers.allBlocks

    /** Parses the short variant of an anonymous link definition
      * (that starts with `__` instead of `.. __:`)
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#anonymous-hyperlinks]].
      */
    lazy val shortAnonymousLinkTarget: BlockParserBuilder =
      ExplicitBlockParsers.shortAnonymousLinkTarget

    /** Parses a single paragraph.
      * Everything between two blank lines that is not recognized
      * as a special reStructuredText block type will be parsed as a regular paragraph.
      */
    lazy val paragraph: BlockParserBuilder = BlockParsers.paragraph

    val all: Seq[BlockParserBuilder] = Seq(
      bulletList,
      enumList,
      fieldList,
      lineBlock,
      optionList,
      explicitBlocks,
      shortAnonymousLinkTarget,
      gridTable,
      simpleTable,
      doctest,
      blockQuote,
      headerWithOverline,
      transition,
      headerWithUnderline,
      definitionList,
      paragraph
    )

  }

  object spanParsers extends MarkupFormat.MarkupParsers[SpanParserBuilder] {

    /** Parses a span of text with strong emphasis.
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#strong-emphasis]]
      */
    lazy val strong: SpanParserBuilder = InlineParsers.strong

    /** Parses a span of emphasized text.
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#emphasis]]
      */
    lazy val em: SpanParserBuilder = InlineParsers.em

    /** Parses an inline literal element.
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#inline-literals]].
      */
    lazy val inlineLiteral: SpanParserBuilder = InlineParsers.inlineLiteral

    /** Parses a phrase link reference (enclosed in back ticks).
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#hyperlink-references]]
      */
    lazy val phraseLinkRef: SpanParserBuilder = InlineParsers.phraseLinkRef

    /** Parses a simple link reference.
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#hyperlink-references]]
      */
    lazy val simpleLinkRef: SpanParserBuilder = InlineParsers.simpleLinkRef

    /** Parses a footnote reference.
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#footnote-references]].
      */
    lazy val footnoteRef: SpanParserBuilder = InlineParsers.footnoteRef

    /** Parses a citation reference.
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#citation-references]].
      */
    lazy val citationRef: SpanParserBuilder = InlineParsers.citationRef

    /** Parses a substitution reference.
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#substitution-references]].
      */
    lazy val substitutionRef: SpanParserBuilder = InlineParsers.substitutionRef

    /** Parses an inline internal link target.
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#inline-internal-targets]]
      */
    lazy val internalTarget: SpanParserBuilder = InlineParsers.internalTarget

    /** Parses an interpreted text element with the role name as a prefix.
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#interpreted-text]]
      */
    lazy val interpretedTextWithRolePrefix: SpanParserBuilder =
      InlineParsers.interpretedTextWithRolePrefix

    /** Parses a standalone HTTP or HTTPS hyperlink (with no surrounding markup).
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#standalone-hyperlinks]]
      */
    lazy val uri: SpanParserBuilder = InlineParsers.uri

    /** Parses a standalone email address (with no surrounding markup).
      *
      * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#standalone-hyperlinks]]
      */
    lazy val email: SpanParserBuilder = InlineParsers.email

    val all: Seq[SpanParserBuilder] = Seq(
      strong,
      em,
      inlineLiteral,
      phraseLinkRef,
      simpleLinkRef,
      footnoteRef,
      citationRef,
      substitutionRef,
      internalTarget,
      interpretedTextWithRolePrefix,
      uri,
      email
    )

  }

  override lazy val escapedChar: Parser[String] = InlineParsers.escapedChar

  private object BundledDefaults extends ExtensionBundle {

    val description: String = "Default extensions for reStructuredText"

    override val origin: BundleOrigin = BundleOrigin.Parser

    override val parsers: ParserBundle = new ParserBundle(
      markupParserHooks = Some(
        new ParserHooks(
          preProcessInput = WhitespacePreprocessor.forString,
          postProcessDocument = DocInfoExtractor,
          postProcessBlocks = LinkTargetProcessor
        )
      )
    )

    override val renderOverrides = Seq(HTML.Overrides(value = ExtendedHTMLRenderer.custom))
  }

  val extensions: Seq[ExtensionBundle] = Seq(
    BundledDefaults,
    RstExtensionSupport,
    StandardExtensions,
    RawContentExtensions
  )

  override def createBlockListParser(parser: Parser[Block]): Parser[Seq[Block]] =
    BlockParsers.createBlockListParser(parser)

}
