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
import laika.api.errors.ParserError
import laika.ast.{ StyleDeclarationSet, TemplateDocument }
import laika.io.descriptor.ParserDescriptor
import laika.io.model.{ InputTreeBuilder, ParsedTree }
import laika.io.ops.InputOps
import laika.io.runtime.{ Batch, ParserRuntime }
import laika.parse.markup.DocumentParser
import laika.parse.markup.DocumentParser.DocumentInput
import laika.theme.{ Theme, ThemeProvider }

/** Parser for a tree of input documents.
  *
  * @author Jens Halm
  */
class TreeParser[F[_]: Async: Batch] private (
    parsers: NonEmptyList[MarkupParser],
    val theme: Theme[F]
) extends InputOps[F] {

  type Result = TreeParser.Op[F]

  protected val F: Async[F] = Async[F]

  lazy val config: OperationConfig = parsers
    .map(_.config)
    .reduceLeft[OperationConfig](_ merge _)
    .withBundles(theme.extensions)

  private[laika] def modifyConfig(f: OperationConfig => OperationConfig): TreeParser[F] = {
    val modifiedParsers = parsers.map(p => new MarkupParser(p.format, f(p.config)))
    new TreeParser(modifiedParsers, theme)
  }

  def fromInput(input: InputTreeBuilder[F]): TreeParser.Op[F] =
    new TreeParser.Op(parsers, theme, input)

}

/** Builder API for constructing a parsing operation for a tree of input documents.
  */
object TreeParser {

  /** Builder step that allows to specify the execution context
    * for blocking IO and CPU-bound tasks.
    */
  class Builder[F[_]: Async: Batch] private[io] (
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
      new Builder(parsers.append(parser), theme)

    /** Specifies an additional parser for text markup.
      *
      * When multiple parsers exist for an operation, the target parser
      * will be determined by the suffix of the input document, e.g.
      * `.md` for Markdown and `.rst` for reStructuredText.
      */
    def withAlternativeParser(parser: ParserBuilder): Builder[F] =
      new Builder(parsers.append(parser.build), theme)

    /** Applies the specified theme to this parser, overriding any previously specified themes.
      */
    def withTheme(theme: ThemeProvider): Builder[F] = new Builder(parsers, theme)

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
  class Op[F[_]: Async: Batch] private[io] (
      private[io] val parsers: NonEmptyList[MarkupParser],
      private[io] val theme: Theme[F],
      private[io] val input: InputTreeBuilder[F]
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
    private[io] lazy val templateParser
        : Option[DocumentInput => Either[ParserError, TemplateDocument]] =
      config.templateParser map { rootParser =>
        DocumentParser.forTemplate(rootParser, config.configProvider)
      }

    /** The parser for CSS documents for this operation. Currently CSS input
      * will only be parsed for PDF output, in case of HTML or EPUB formats
      * CSS documents will merely copied to the target format.
      */
    private[io] lazy val styleSheetParser
        : DocumentInput => Either[ParserError, StyleDeclarationSet] =
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
