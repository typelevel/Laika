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

package laika.bundle

import laika.ast.*
import laika.parse.Parser

/** Bundles a collection of all types of parsers used in a transformation.
  *
  * The parsers for text markup and configuration headers are meant to complement
  * base parsers defined by the host language. If they fail for a given input the built-in parsers
  * will still be tried for the same block, span or configuration header respectively.
  *
  * The parsers for stylesheets and templates on the other hand are meant to overwrite
  * any previously installed parsers.
  *
  * @param blockParsers parsers for block elements in text markup, complementing the parsers of the host language
  * @param spanParsers parsers for span elements in text markup, complementing the parsers of the host language
  * @param syntaxHighlighters parsers for syntax highlighting of code blocks
  * @param markupParserHooks hooks for markup parsers to control aspects beyond the individual span and block parsers
  * @param configProvider parser for configuration headers in text markup and template documents and configuration documents
  * @param templateParser parser for template documents
  * @param styleSheetParser parser for CSS documents
  */
class ParserBundle(
    val blockParsers: Seq[BlockParserBuilder] = Nil,
    val spanParsers: Seq[SpanParserBuilder] = Nil,
    val syntaxHighlighters: Seq[SyntaxHighlighter] = Nil,
    val markupParserHooks: Option[ParserHooks] = None,
    val configProvider: Option[ConfigProvider] = None,
    val templateParser: Option[Parser[TemplateRoot]] = None,
    val styleSheetParser: Option[Parser[Set[StyleDeclaration]]] = None
) {

  /** Merges this instance with the specified base.
    * Collections of parsers will be merged.
    * Optional parsers in this instance will overwrite optional parsers
    * in the base (if defined), with the base only serving as a fallback.
    */
  def withBase(base: ParserBundle): ParserBundle =
    new ParserBundle(
      blockParsers ++ base.blockParsers,
      spanParsers ++ base.spanParsers,
      syntaxHighlighters ++ base.syntaxHighlighters,
      (markupParserHooks.toSeq ++ base.markupParserHooks.toSeq).reduceLeftOption(_ withBase _),
      configProvider.orElse(base.configProvider),
      templateParser.orElse(base.templateParser),
      styleSheetParser.orElse(base.styleSheetParser)
    )

  /** Just the extensions for the text markup parser defined in this bundle.
    * Fallback instances will be added where appropriate for parsers or hooks not defined
    * in this bundle.
    */
  private[laika] def markupExtensions: MarkupExtensions =
    new MarkupExtensions(
      blockParsers,
      spanParsers,
      syntaxHighlighters,
      markupParserHooks.getOrElse(new ParserHooks())
    )

}

/** Hooks for markup parsers to control aspects beyond the individual span and block
  * parsers defined for the host language.
  *
  * @param postProcessBlocks function invoked for every block container, allowing post-processing of the result
  * @param postProcessDocument function invoked after parsing but before rewriting, allowing to modify the document
  * @param preProcessInput function invoked before parsing, allowing to pre-process the input
  */
class ParserHooks(
    val postProcessBlocks: Seq[Block] => Seq[Block] = identity,
    val postProcessDocument: UnresolvedDocument => UnresolvedDocument = identity,
    val preProcessInput: String => String = identity
) {

  /** Merges this instance with the specified base.
    * The functions specified in the base are always invoked before
    * the functions in this instance.
    */
  def withBase(base: ParserHooks): ParserHooks = new ParserHooks(
    base.postProcessBlocks andThen postProcessBlocks,
    base.postProcessDocument andThen postProcessDocument,
    base.preProcessInput andThen preProcessInput
  )

}

/** Bundles extensions for the text markup parsers defined for the host language to support additional
  * syntax not recognized by the base parsers.
  *
  * When extension parsers fail for a given input the built-in parsers
  * will still be tried for the same block or span respectively.
  *
  * @param blockParsers parsers for block elements in text markup, complementing the parsers of the host language
  * @param spanParsers parsers for span elements in text markup, complementing the parsers of the host language
  * @param syntaxHighlighters parsers for syntax highlighting of code blocks
  * @param parserHooks hooks for markup parsers to control aspects beyond the individual span and block parsers
  */
private[laika] class MarkupExtensions(
    val blockParsers: Seq[BlockParserBuilder],
    val spanParsers: Seq[SpanParserBuilder],
    val syntaxHighlighters: Seq[SyntaxHighlighter],
    val parserHooks: ParserHooks
)
