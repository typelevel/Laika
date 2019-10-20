/*
 * Copyright 2012-2019 the original author or authors.
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

package laika.io.text

import cats.Parallel
import cats.data.NonEmptyList
import cats.effect.{Async, Blocker, ContextShift}
import laika.api.MarkupParser
import laika.api.builder.{OperationConfig, ParserBuilder}
import laika.ast.{DocumentType, StyleDeclarationSet, TemplateDocument, TextDocumentType}
import laika.io.model.{ParsedTree, TreeInput}
import laika.io.ops.ParallelInputOps
import laika.parse.markup.DocumentParser
import laika.parse.markup.DocumentParser.{ParserError, ParserInput}
import laika.runtime.{ParserRuntime, Runtime}

/** Parser for a tree of input documents.
  *
  * @author Jens Halm
  */
class ParallelParser[F[_]: Async: Runtime] (parsers: NonEmptyList[MarkupParser]) extends ParallelInputOps[F] {

  type Result = ParallelParser.Op[F]

  val F: Async[F] = Async[F]

  val docType: TextDocumentType = DocumentType.Markup

  lazy val config: OperationConfig = parsers.map(_.config).reduceLeft[OperationConfig](_ merge _)

  def fromInput (input: F[TreeInput]): ParallelParser.Op[F] = ParallelParser.Op(parsers, input)

}

/** Builder API for constructing a parsing operation for a tree of input documents.
  */
object ParallelParser {

  /** Builder step that allows to specify the execution context
    * for blocking IO and CPU-bound tasks.
    */
  case class Builder[F[_]: Async: Runtime] (parsers: NonEmptyList[MarkupParser]) {

    /** Specifies an additional parser for text markup.
      * 
      * When multiple parsers exist for an operation, the target parser
      * will be determined by the suffix of the input document, e.g.
      * `.md` for Markdown and `.rst` for reStructuredText.
      */
    def withAlternativeParser (parser: MarkupParser): Builder[F] = copy(parsers = parsers.append(parser))

    /** Specifies an additional parser for text markup.
      *
      * When multiple parsers exist for an operation, the target parser
      * will be determined by the suffix of the input document, e.g.
      * `.md` for Markdown and `.rst` for reStructuredText.
      */
    def withAlternativeParser (parser: ParserBuilder): Builder[F] = copy(parsers = parsers.append(parser.build))

    /** Final builder step that creates a parallel parser.
      */
    def build: ParallelParser[F] = new ParallelParser[F](parsers)

  }

  /** Represents a parsing operation for a tree of input documents.
    *
    * It can be run by invoking the `parse` method which delegates to the library's
    * default runtime implementation or by developing a custom runner that performs
    * the parsing based on this operation's properties.
    */
  case class Op[F[_]: Async: Runtime] (parsers: NonEmptyList[MarkupParser], input: F[TreeInput]) {

    /** Maps the suffixes of the supported markup formats to the corresponding parser.
      */
    val parserMap: Map[String, MarkupParser] = parsers.toList.flatMap(p => p.fileSuffixes.map((_, p))).toMap

    /** The merged configuration of all markup parsers of this operation.
      */
    lazy val config: OperationConfig = parsers.map(_.config).reduceLeft[OperationConfig](_ merge _)

    /** The template parser for this operation. If this property is empty
      * templating is not supported for this operation.
      */
    lazy val templateParser: Option[ParserInput => Either[ParserError, TemplateDocument]] = config.templateParser map { rootParser =>
      DocumentParser.forTemplate(rootParser, config.configProvider)
    }

    /** The parser for CSS documents for this operation. Currently CSS input
      * will only be parsed for PDF output, in case of HTML or EPUB formats
      * CSS documents will merely copied to the target format.
      */
    lazy val styleSheetParser: ParserInput => Either[ParserError, StyleDeclarationSet] =
      DocumentParser.forStyleSheets(config.styleSheetParser)

    /** Performs the parsing operation based on the library's
      * default runtime implementation, suspended in the effect F.
      */
    def parse: F[ParsedTree] = ParserRuntime.run(this)

  }

}
