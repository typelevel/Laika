/*
 * Copyright 2012-2021 the original author or authors.
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
import laika.bundle.{BlockParserBuilder, ExtensionBundle, SpanParserBuilder}
import laika.factory.MarkupFormat
//import laika.markdown.bundle.VerbatimHTML
//import laika.markdown.{BlockParsers, InlineParsers, ListParsers}
import laika.parse.Parser
import laika.parse.text.{CharGroup, TextParsers}
  import laika.asciidoc.InlineParsers
  import laika.asciidoc.BlockParsers
  
/** A parser for AsciiDoc text. Instances of this class may be passed directly
 *  to the `Parser` or `Transformer` APIs:
 *  
 *  {{{
 *  val document = MarkupParser.of(AsciiDoc).build.parse(inputString)
 *  
 *  Transformer.from(AsciiDoc).to(HTML).build.transform(inputString)
 *  }}}
 *  
 * 
 *  @author i10416
 */
case object AsciiDoc extends MarkupFormat {

    override val description: String = "AsciiDoc"

  val fileSuffixes: Set[String] = Set("asc", "adoc","asciidoc")

  val blockParsers: Seq[BlockParserBuilder] = Seq(
    BlockParsers.rootParagraph,
    BlockParsers.quotedBlock,
    BlockParsers.fallbackParagraph,
    BlockParsers.rules,
    BlockParsers.literalBlocks,
    /*BlockParsers.linkTarget,
    BlockParsers.section,
    BlockParsers.descreteHeader,(similar to header element in html)
    BolckParsers.commentBlocks,
    ListParsers.enumLists.rootOnly,
    ListParsers.enumLists.nestedOnly.interruptsParagraphWith(TextParsers.oneOf(CharGroup.digit)),
    ListParsers.bulletLists.rootOnly,
    ListParsers.bulletLists.nestedOnly.interruptsParagraphWith(TextParsers.oneOf('+', '*', '-')),*/
  )

  val spanParsers: Seq[SpanParserBuilder] = Seq(
    InlineParsers.em,
    InlineParsers.strong,
    InlineParsers.inlineLiteral,
    InlineParsers.uri,
    InlineParsers.email,
    InlineParsers.lineBreak,
    // note: enclosed by # without role 
    InlineParsers.mark,
    /*
    roles: enclosed by # with leading [<role-name>]. e.g. [.underline]#this text has underline#
    - InlineParsers.underline,
    - InlineParsers.overline,
    - InlineParsers.lineThrough,
    - InlineParsers.nobreak,
    - InlineParsers.nowrap,
    - InlineParsers.prewrap,
    
    InlineParsers.image,
    InlineParsers.link,
    InlineParsers.simpleLink,
    
    entire line
    - InlineParsers.comment,

    */
  )

  override lazy val escapedChar: Parser[String] = InlineParsers.escapedChar

  val extensions: Seq[ExtensionBundle] = Seq(/*VerbatimHTML*/)

  override def createBlockListParser (parser: Parser[Block]): Parser[Seq[Block]] =
    super.createBlockListParser(parser)
}
