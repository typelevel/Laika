/*
 * Copyright 2013-2017 the original author or authors.
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

package laika.parse.rst

import laika.api.ext._
import laika.parse.core.Parser
import laika.parse.core.markup.RootParserBase
import laika.parse.core.text.TextParsers._
import laika.tree.Elements._

/** The main parser for reStructuredText, combining the individual parsers for block and inline elements,
  * and adding functionality like directives depending on configuration.
  *
  * @author Jens Halm
  */
class RootParser(val blockParserExtensions: Seq[BlockParserBuilder] = Nil,
                 val spanParserExtensions: Seq[SpanParserBuilder] = Nil,
                 postProcessBlocks: Seq[Block] => Seq[Block] = identity) extends RootParserBase {


  /** Parses an escaped character. For most characters it produces the character
    *  itself as the result with the only exception being an escaped space character
    *  which is removed from the output in reStructuredText.
    *
    *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#escaping-mechanism]].
    */
  override lazy val escapedChar: Parser[String] = (" " ^^^ "") | (any take 1)


  val mainBlockParsers = Seq(
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

  val mainSpanParsers = Seq(
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
    InlineParsers.email,
    SpanParser.forStartChar('\\').standalone(escapedChar ^^ { Text(_) }).withLowPrecedence // TODO - extract
  )

  override def blockList (parser: => Parser[Block]): Parser[Seq[Block]] =
    BlockParsers.createBlockListParser(parser) ^^ postProcessBlocks


}
