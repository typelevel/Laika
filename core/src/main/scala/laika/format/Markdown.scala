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
import laika.bundle.{BlockParserBuilder, BundleOrigin, ExtensionBundle, ParserBundle, ParserHooks, SpanParserBuilder}
import laika.factory.MarkupFormat
import laika.markdown.{BlockParsers, InlineParsers, ListParsers}
import laika.markdown.bundle.{HeaderIdInsertion, VerbatimHTML}
import laika.parse.Parser
  
/** A parser for Markdown text. Instances of this class may be passed directly
 *  to the `Parser` or `Transformer` APIs:
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

  val blockParsers: Seq[BlockParserBuilder] = Seq(
    BlockParsers.atxHeader,
    BlockParsers.linkTarget,
    BlockParsers.quotedBlock,
    BlockParsers.rootHeaderOrParagraph,
    BlockParsers.nestedHeaderOrParagraph,
    BlockParsers.fallbackParagraph
  ) ++
    BlockParsers.literalBlocks ++
    BlockParsers.rules ++
    ListParsers.enumLists ++
    ListParsers.bulletLists

  val spanParsers: Seq[SpanParserBuilder] = Seq(
    InlineParsers.enclosedByAsterisk,
    InlineParsers.enclosedByUnderscore,
    InlineParsers.literalSpan,
    InlineParsers.image,
    InlineParsers.link,
    InlineParsers.simpleLink,
    InlineParsers.lineBreak
  )

  override lazy val escapedChar: Parser[String] = InlineParsers.escapedChar

  object BundledDefaults extends ExtensionBundle {
    val description: String = "Header ids for Markdown"
    override val origin: BundleOrigin = BundleOrigin.Parser
    override val parsers: ParserBundle = ParserBundle(
      markupParserHooks = Some(ParserHooks(
        postProcessBlocks = HeaderIdInsertion
      ))
    )
  }

  val extensions: Seq[ExtensionBundle] = Seq(BundledDefaults, VerbatimHTML)

  override def createBlockListParser (parser: Parser[Block]): Parser[Seq[Block]] =
    super.createBlockListParser(BlockParsers.insignificantSpaces ~> parser)
  

}
