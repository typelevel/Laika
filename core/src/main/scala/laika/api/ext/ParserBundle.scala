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
import laika.parse.css.Styles.StyleDeclaration
import laika.tree.Documents.Document
import laika.tree.Elements.{Block, InvalidElement}
import laika.tree.Paths.Path
import laika.tree.Templates.TemplateRoot

/**
  * @author Jens Halm
  */
case class ParserBundle(blockParsers: Seq[BlockParserBuilder] = Nil,
                        spanParsers: Seq[SpanParserBuilder] = Nil,
                        markupParserHooks: Option[ParserHooks] = None,
                        configHeaderParsers: Seq[Path => Parser[Either[InvalidElement, Config]]] = Nil,
                        templateParser: Option[Parser[TemplateRoot]] = None,
                        styleSheetParser: Option[Parser[Set[StyleDeclaration]]] = None) {

  def withBase (base: ParserBundle): ParserBundle =
    ParserBundle(
      blockParsers ++ base.blockParsers,
      spanParsers ++ base.spanParsers,
      (markupParserHooks.toSeq ++ base.markupParserHooks.toSeq).reduceLeftOption(_ withBase _),
      configHeaderParsers ++ base.configHeaderParsers,
      templateParser.orElse(base.templateParser),
      styleSheetParser.orElse(base.styleSheetParser)
    )

  def markupExtensions: MarkupExtensions =
    MarkupExtensions(blockParsers, spanParsers, markupParserHooks.getOrElse(ParserHooks()))

}

case class ParserHooks(postProcessBlocks: Seq[Block] => Seq[Block] = identity,
                       postProcessDocument: Document => Document = identity,
                       preProcessInput: Input => Input = identity) {

  def withBase (base: ParserHooks): ParserHooks = new ParserHooks(
    base.postProcessBlocks andThen postProcessBlocks,
    base.postProcessDocument andThen postProcessDocument,
    base.preProcessInput andThen preProcessInput
  )

}

case class MarkupExtensions (blockParsers: Seq[BlockParserBuilder],
                             spanParsers: Seq[SpanParserBuilder],
                             rootParserHooks: ParserHooks)
