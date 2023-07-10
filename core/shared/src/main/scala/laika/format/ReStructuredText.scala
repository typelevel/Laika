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

import laika.ast.Block
import laika.bundle.{ BundleOrigin, ExtensionBundle, ParserBundle, ParserHooks }
import laika.factory.MarkupFormat
import laika.parse.Parser
import laika.parse.text.WhitespacePreprocessor
import laika.rst._
import laika.rst.bundle._

/** A parser for text written in reStructuredText markup. Instances of this class may be passed directly
  *  to the `Parseer` or `Transformer` APIs:
  *
  *  {{{
  *  val document = MarkupParser.of(ReStructuredText).build.parse(inputString)
  *
  *  Transformer.from(ReStructuredText).to(HTML).build.transform(inputString)
  *  }}}
  *
  *  reStructuredText has several types of extension points that are fully supported by Laika.
  *  For more information on how to implement and register those see [[laika.rst.bundle.RstExtensionRegistry]].
  *
  *  In addition to the standard reStructuredText directives, the API also supports a custom directive
  *  type unique to Laika. They represent a library-wide extension mechanism and allow you to implement
  *  tags which can be used in any of the supported markup formats or in templates. If you need this
  *  level of flexibility, it is recommended to use the Laika directives, if you want to stay compatible
  *  with the reStructuredText reference parser, you should pick the standard directives.
  *
  *  Laika directives can be registered with the [[laika.directive.DirectiveRegistry]] extension bundle.
  *  The DSLs for creating directives are similar, but still different,
  *  due to differences in the feature set of the two variants. The Laika directives try to avoid some
  *  of the unnecessary complexities of reStructuredText directives.
  *
  *  @author Jens Halm
  */
case object ReStructuredText extends MarkupFormat { self =>

  override val description: String = "reStructuredText"

  val fileSuffixes: Set[String] = Set("rest", "rst")

  val blockParsers = Seq(
    ListParsers.bulletList,
    ListParsers.enumList,
    ListParsers.fieldList,
    ListParsers.lineBlock,
    ListParsers.optionList,
    ExplicitBlockParsers.allBlocks,
    ExplicitBlockParsers.shortAnonymousLinkTarget,
    TableParsers.gridTable,
    TableParsers.simpleTable,
    BlockParsers.doctest,
    BlockParsers.blockQuote,
    BlockParsers.headerWithOverline,
    BlockParsers.transition,
    BlockParsers.headerWithUnderline,
    ListParsers.definitionList,
    BlockParsers.paragraph
  )

  val spanParsers = Seq(
    InlineParsers.strong,
    InlineParsers.em,
    InlineParsers.inlineLiteral,
    InlineParsers.phraseLinkRef,
    InlineParsers.simpleLinkRef,
    InlineParsers.footnoteRef,
    InlineParsers.citationRef,
    InlineParsers.substitutionRef,
    InlineParsers.internalTarget,
    InlineParsers.interpretedTextWithRolePrefix,
    InlineParsers.uri,
    InlineParsers.email
  )

  override lazy val escapedChar = InlineParsers.escapedChar

  object BundledDefaults extends ExtensionBundle {

    val description: String = "Default extensions for reStructuredText"

    override val origin: BundleOrigin = BundleOrigin.Parser

    override val parsers: ParserBundle = new ParserBundle(
      markupParserHooks = Some(
        new ParserHooks(
          preProcessInput = WhitespacePreprocessor.forInput,
          postProcessDocument = DocInfoExtractor,
          postProcessBlocks = LinkTargetProcessor
        )
      )
    )

    override val renderOverrides = Seq(HTML.Overrides(value = ExtendedHTMLRenderer.custom))
  }

  val extensions = Seq(
    BundledDefaults,
    RstExtensionSupport,
    StandardExtensions,
    RawContentExtensions
  )

  override def createBlockListParser(parser: Parser[Block]): Parser[Seq[Block]] =
    BlockParsers.createBlockListParser(parser)

}
