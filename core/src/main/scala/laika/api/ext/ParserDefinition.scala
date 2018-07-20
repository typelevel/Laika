/*
 * Copyright 2013-2018 the original author or authors.
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

package laika.api.ext

import com.typesafe.config.Config
import laika.io.Input
import laika.parse.core.Parser
import laika.parse.core.markup.DocumentParser.InvalidElement
import laika.parse.core.markup.{EscapedTextParsers, RecursiveParsers, RecursiveSpanParsers}
import laika.parse.core.text.TextParsers.char
import laika.parse.css.Styles.StyleDeclaration
import laika.tree.Documents.Document
import laika.tree.Elements.{Block, Span}
import laika.tree.Paths.Path
import laika.tree.Templates.TemplateRoot

/**
  * @author Jens Halm
  */
sealed trait ParserDefinition {

  def isRecursive: Boolean

  def precedence: Precedence

}

case class BlockParserDefinition (startChar: Option[Char],
                                  parser: Parser[Block],
                                  isRecursive: Boolean,
                                  position: BlockPosition,
                                  precedence: Precedence) extends ParserDefinition {
  val fullParser = startChar.fold(parser)(_ ~> parser)
}

case class SpanParserDefinition (startChar: Char,
                                 parser: Parser[Span],
                                 isRecursive: Boolean,
                                 precedence: Precedence) extends ParserDefinition

sealed trait ParserBuilder[T <: ParserDefinition] {

  def createParser (recursiveParsers: RecursiveParsers): T

}

trait BlockParserBuilder extends ParserBuilder[BlockParserDefinition]
trait SpanParserBuilder extends ParserBuilder[SpanParserDefinition]

case class RootParserHooks (postProcessBlocks: Seq[Block] => Seq[Block] = identity,
                            postProcessDocument: Document => Document = identity,
                            preProcessInput: Input => Input = identity) {

  def withBase (base: RootParserHooks): RootParserHooks = new RootParserHooks(
    base.postProcessBlocks andThen postProcessBlocks,
    base.postProcessDocument andThen postProcessDocument,
    base.preProcessInput andThen preProcessInput
  )

}

case class ParserDefinitionBuilders(blockParsers: Seq[BlockParserBuilder] = Nil,
                                    spanParsers: Seq[SpanParserBuilder] = Nil,
                                    rootParserHooks: Option[RootParserHooks] = None,
                                    configHeaderParsers: Seq[Path => Parser[Either[InvalidElement, Config]]] = Nil,
                                    templateParser: Option[Parser[TemplateRoot]] = None,
                                    styleSheetParser: Option[Parser[Set[StyleDeclaration]]] = None) {

  def withBase(builders: ParserDefinitionBuilders): ParserDefinitionBuilders =
    ParserDefinitionBuilders(
      blockParsers ++ builders.blockParsers,
      spanParsers ++ builders.spanParsers,
      (rootParserHooks.toSeq ++ builders.rootParserHooks.toSeq).reduceLeftOption(_ withBase _),
      configHeaderParsers ++ builders.configHeaderParsers,
      templateParser.orElse(builders.templateParser),
      styleSheetParser.orElse(builders.styleSheetParser)
    )

}

sealed trait Precedence
object Precedence {
  object High extends Precedence
  object Low extends Precedence
}

class SpanParser (startChar: Char) {

  class DefinitionBuilder (parserFactory: RecursiveSpanParsers => Parser[Span],
                           recursive: Boolean,
                           precedence: Precedence) extends SpanParserBuilder {

    override def createParser (recursiveParsers: RecursiveParsers): SpanParserDefinition =
      SpanParserDefinition(startChar, parserFactory(recursiveParsers), recursive, precedence)

    def withLowPrecedence: DefinitionBuilder = new DefinitionBuilder(parserFactory, recursive, Precedence.Low)

  }

  def standalone (parser: Parser[Span]): DefinitionBuilder = new DefinitionBuilder(_ => parser, false, Precedence.High)

  def recursive (factory: RecursiveSpanParsers => Parser[Span]): DefinitionBuilder =
    new DefinitionBuilder(factory, true, Precedence.High)

}

object SpanParser {

  def forStartChar (char: Char): SpanParser = new SpanParser(char)

}

sealed trait BlockPosition
object BlockPosition {
  object Any extends BlockPosition
  object RootOnly extends BlockPosition
  object NestedOnly extends BlockPosition
}

class BlockParser (startChar: Option[Char] = None) {

  case class DefinitionBuilder (parserFactory: RecursiveParsers => Parser[Block],
                                recursive: Boolean = false,
                                position: BlockPosition = BlockPosition.Any,
                                precedence: Precedence = Precedence.High) extends BlockParserBuilder {

    override def createParser (recursiveParsers: RecursiveParsers): BlockParserDefinition =
      BlockParserDefinition(startChar, parserFactory(recursiveParsers), recursive, position, precedence)

    def withLowPrecedence: DefinitionBuilder = copy(precedence = Precedence.Low)

    def rootOnly: DefinitionBuilder = copy(position = BlockPosition.RootOnly)
    def nestedOnly: DefinitionBuilder = copy(position = BlockPosition.NestedOnly)

  }

  def standalone (parser: Parser[Block]): DefinitionBuilder = new DefinitionBuilder(_ => parser)

  def recursive (factory: RecursiveParsers => Parser[Block]): DefinitionBuilder =
    new DefinitionBuilder(factory, recursive = true)

  def withSpans (factory: RecursiveSpanParsers => Parser[Block]): DefinitionBuilder =
    new DefinitionBuilder(factory)

  def withEscapedText (factory: EscapedTextParsers => Parser[Block]): DefinitionBuilder =
    new DefinitionBuilder(factory)

}

object BlockParser {

  def forStartChar (char: Char): BlockParser = new BlockParser(Some(char))

  def withoutStartChar: BlockParser = new BlockParser()

}
