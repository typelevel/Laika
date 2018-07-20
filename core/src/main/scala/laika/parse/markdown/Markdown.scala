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

package laika.parse.markdown

import laika.api.ext.{ExtensionBundle, ParserDefinitionBuilders, RootParserHooks, SpanParser}
import laika.factory.MarkupParser
import laika.parse.core.Parser
import laika.parse.core.markup.RootParser
import laika.parse.core.text.TextParsers.anyOf
import laika.parse.markdown.html.VerbatimHTML
import laika.tree.Elements.{Block, Text}
  
/** A parser for Markdown text. Instances of this class may be passed directly
 *  to the `Parse` or `Transform` APIs:
 *  
 *  {{{
 *  val document = Parse as Markdown fromFile "hello.md"
 *  
 *  Transform from Markdown to HTML fromFile "hello.md" toFile "hello.html"
 *  }}}
 *  
 *  Since this library is not solely focused on producing HTML output,
 *  parsing verbatim HTML elements like defined by the official Markdown 
 *  syntax description is an optional feature, as some types of renderers 
 *  would not know what to do with HTML nodes in the document tree. 
 *  It must be enabled explicitly:
 *  
 *  {{{
 *  val document = Parse.as(Markdown).withRawContent.fromFile("hello.md")
 *  }}}
 *  
 *  To switch off all custom extensions like directives,
 *  configuration sections at the start of the document or automatic
 *  id generation for headers, you can run the parser in strict mode:
 *  
 *  {{{
 *  Transform.from(Markdown).to(HTML).strict
 *    .fromFile("hello.md").toFile("hello.html")
 *  }}}
 * 
 *  @author Jens Halm
 */
object Markdown extends MarkupParser {

  val fileSuffixes: Set[String] = Set("md", "markdown")

  val blockParsers = Seq(
    BlockParsers.atxHeader,
    BlockParsers.linkTarget,
    BlockParsers.quotedBlock,
    BlockParsers.rootHeaderOrParagraph,
    BlockParsers.nestedHeaderOrParagraph
  ) ++
    BlockParsers.literalBlocks ++
    BlockParsers.rules ++
    ListParsers.enumLists ++
    ListParsers.bulletLists

  val spanParsers = Seq(
    InlineParsers.enclosedByAsterisk,
    InlineParsers.enclosedByUnderscore,
    InlineParsers.literalSpan,
    InlineParsers.image,
    InlineParsers.link,
    InlineParsers.simpleLink,
    InlineParsers.lineBreak,
    SpanParser.forStartChar('\\').standalone(escapedChar ^^ { Text(_) }).withLowPrecedence // TODO - extract
  )

  override lazy val escapedChar = InlineParsers.escapedChar

  val extensions = Seq(
    new ExtensionBundle {
      override val parserDefinitions: ParserDefinitionBuilders = ParserDefinitionBuilders(
        rootParserHooks = Some(RootParserHooks(
          postProcessBlocks = HeaderIdInsertion
        ))
      )
    },
    VerbatimHTML
  )

  override def createBlockListParser (parser: Parser[Block]): Parser[Seq[Block]] =
    super.createBlockListParser(BlockParsers.insignificantSpaces ~> parser)
  

}
