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

package laika.io.api

import cats.data.NonEmptyList
import cats.effect.{ Async, Resource }
import laika.api.MarkupParser
import laika.api.builder.{ OperationConfig, ParserBuilder }
import laika.ast.{ DocumentType, StyleDeclarationSet, TemplateDocument, TextDocumentType }
import laika.io.descriptor.ParserDescriptor
import laika.io.model.{ InputTreeBuilder, ParsedTree }
import laika.io.ops.InputOps
import laika.io.runtime.{ Batch, ParserRuntime }
import laika.parse.markup.DocumentParser
import laika.parse.markup.DocumentParser.{ DocumentInput, ParserError }
import laika.theme.{ Theme, ThemeProvider }

/** Parser for a tree of input documents.
  *
  * @author Jens Halm
  */
class TreeParser[F[_]: Async: Batch](parsers: NonEmptyList[MarkupParser], val theme: Theme[F])
    extends InputOps[F] {

  type Result = TreeParser.Op[F]

  val F: Async[F] = Async[F]

  val docType: TextDocumentType = DocumentType.Markup

  lazy val config: OperationConfig = parsers
    .map(_.config)
    .reduceLeft[OperationConfig](_ merge _)
    .withBundles(theme.extensions)

  private[laika] def modifyConfig(f: OperationConfig => OperationConfig): TreeParser[F] = {
    val modifiedParsers = parsers.map(p => new MarkupParser(p.format, f(p.config)))
    new TreeParser(modifiedParsers, theme)
  }

  def fromInput(input: InputTreeBuilder[F]): TreeParser.Op[F] = TreeParser.Op(parsers, theme, input)

}

/** Builder API for constructing a parsing operation for a tree of input documents.
  */
object TreeParser {

  /** Builder step that allows to specify the execution context
    * for blocking IO and CPU-bound tasks.
    */
  case class Builder[F[_]: Async: Batch](
      parsers: NonEmptyList[MarkupParser],
      theme: ThemeProvider
  ) {

    /** Specifies an additional parser for text markup.
      *
      * When multiple parsers exist for an operation, the target parser
      * will be determined by the suffix of the input document, e.g.
      * `.md` for Markdown and `.rst` for reStructuredText.
      */
    def withAlternativeParser(parser: MarkupParser): Builder[F] =
      copy(parsers = parsers.append(parser))

    /** Specifies an additional parser for text markup.
      *
      * When multiple parsers exist for an operation, the target parser
      * will be determined by the suffix of the input document, e.g.
      * `.md` for Markdown and `.rst` for reStructuredText.
      */
    def withAlternativeParser(parser: ParserBuilder): Builder[F] =
      copy(parsers = parsers.append(parser.build))

    /** Applies the specified theme to this parser, overriding any previously specified themes.
      */
    def withTheme(theme: ThemeProvider): Builder[F] = copy(theme = theme)

    /** Final builder step that creates a parallel parser.
      */
    def build: Resource[F, TreeParser[F]] = theme.build.map(new TreeParser[F](parsers, _))

  }

  /** Represents a parsing operation for a tree of input documents.
    *
    * It can be run by invoking the `parse` method which delegates to the library's
    * default runtime implementation or by developing a custom runner that performs
    * the parsing based on this operation's properties.
    */
  case class Op[F[_]: Async: Batch](
      parsers: NonEmptyList[MarkupParser],
      theme: Theme[F],
      input: InputTreeBuilder[F]
  ) {

    /** The merged configuration of all markup parsers of this operation, including the theme extensions.
      */
    lazy val config: OperationConfig = parsers
      .map(_.config)
      .reduceLeft[OperationConfig](_ merge _)
      .withBundles(theme.extensions)

    /** The template parser for this operation. If this property is empty
      * templating is not supported for this operation.
      */
    lazy val templateParser: Option[DocumentInput => Either[ParserError, TemplateDocument]] =
      config.templateParser map { rootParser =>
        DocumentParser.forTemplate(rootParser, config.configProvider)
      }

    /** The parser for CSS documents for this operation. Currently CSS input
      * will only be parsed for PDF output, in case of HTML or EPUB formats
      * CSS documents will merely copied to the target format.
      */
    lazy val styleSheetParser: DocumentInput => Either[ParserError, StyleDeclarationSet] =
      DocumentParser.forStyleSheets(config.styleSheetParser)

    /** Performs the parsing operation based on the library's
      * default runtime implementation, suspended in the effect F.
      */
    def parse: F[ParsedTree[F]] = ParserRuntime.run(this)

    /** Provides a description of this operation, the parsers
      * and extension bundles used, as well as the input sources.
      * This functionality is mostly intended for tooling support.
      */
    def describe: F[ParserDescriptor] = ParserDescriptor.create(this)

  }

}
