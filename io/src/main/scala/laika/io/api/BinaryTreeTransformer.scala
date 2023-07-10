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

import cats.data.{ Kleisli, NonEmptyList }
import cats.effect.{ Async, Resource }
import laika.api.MarkupParser
import laika.api.builder.{ OperationConfig, ParserBuilder }
import laika.io.api.BinaryTreeRenderer.{ BinaryRenderFormat, BinaryRenderer }
import laika.io.api.BinaryTreeTransformer.TreeMapper
import laika.io.descriptor.TransformerDescriptor
import laika.io.model.*
import laika.io.ops.{ BinaryOutputOps, InputOps, TreeMapperOps }
import laika.io.runtime.{ Batch, TransformerRuntime }
import laika.theme.{ Theme, ThemeProvider }

/** Transformer that merges a tree of input documents to a single binary output document.
  *
  * @author Jens Halm
  */
class BinaryTreeTransformer[F[_]: Async: Batch] private (
    parsers: NonEmptyList[MarkupParser],
    renderer: BinaryRenderer[F],
    theme: Theme[F],
    mapper: TreeMapper[F]
) extends InputOps[F] {

  type Result = BinaryTreeTransformer.OutputOps[F]

  protected val F: Async[F] = Async[F]

  lazy val config: OperationConfig = parsers
    .map(_.config)
    .reduceLeft[OperationConfig](_ merge _)
    .withBundles(theme.extensions)

  def fromInput(input: InputTreeBuilder[F]): BinaryTreeTransformer.OutputOps[F] =
    new BinaryTreeTransformer.OutputOps(parsers, renderer, theme, input, mapper)

}

/** Builder API for constructing a transformation for a tree of input and binary output documents.
  */
object BinaryTreeTransformer {

  type TreeMapper[F[_]] = Kleisli[F, ParsedTree[F], ParsedTree[F]]

  /** Builder step that allows to specify the execution context
    * for blocking IO and CPU-bound tasks.
    */
  case class Builder[F[_]: Async: Batch] private[io] (
      parsers: NonEmptyList[MarkupParser],
      renderFormat: BinaryRenderFormat,
      config: OperationConfig,
      theme: ThemeProvider,
      mapper: TreeMapper[F]
  ) extends TreeMapperOps[F] {

    type MapRes = Builder[F]

    def evalMapTree(f: ParsedTree[F] => F[ParsedTree[F]]): MapRes =
      new Builder[F](parsers, renderFormat, config, theme, mapper.andThen(f))

    /** Specifies an additional parser for text markup.
      *
      * When multiple parsers exist for an operation,
      * the target parser will be determined by the suffix of the input document,
      * e.g. `.md` for Markdown and `.rst` for reStructuredText.
      */
    def withAlternativeParser(parser: MarkupParser): Builder[F] =
      copy(parsers = parsers.append(parser))

    /** Specifies an additional parser for text markup.
      *
      * When multiple parsers exist for an operation,
      * the target parser will be determined by the suffix of the input document,
      * e.g. `.md` for Markdown and `.rst` for reStructuredText.
      */
    def withAlternativeParser(parser: ParserBuilder): Builder[F] =
      copy(parsers = parsers.append(parser.build))

    /** Applies the specified theme to this transformer, overriding any previously specified themes.
      */
    def withTheme(theme: ThemeProvider): Builder[F] = copy(theme = theme)

    /** Final builder step that creates a parallel transformer for binary output.
      */
    def build: Resource[F, BinaryTreeTransformer[F]] = for {
      initializedTheme    <- theme.build
      initializedRenderer <- BinaryTreeRenderer.buildRenderer(
        renderFormat,
        config,
        initializedTheme
      )
    } yield new BinaryTreeTransformer[F](parsers, initializedRenderer, initializedTheme, mapper)

  }

  /** Builder step that allows to specify the output to render to.
    */
  class OutputOps[F[_]: Async: Batch] private[api] (
      parsers: NonEmptyList[MarkupParser],
      renderer: BinaryRenderer[F],
      theme: Theme[F],
      input: InputTreeBuilder[F],
      mapper: TreeMapper[F]
  ) extends BinaryOutputOps[F] {

    protected val F: Async[F] = Async[F]

    type Result = Op[F]

    def toOutput(output: BinaryOutput[F]): Op[F] =
      new Op[F](parsers, renderer, theme, input, mapper, output)

  }

  /** Represents a transformation for a tree of input documents merged into a single binary output document.
    *
    * It can be run by invoking the `transform` method which delegates to the library's
    * default runtime implementation or by developing a custom runner that performs
    * the transformation based on this operation's properties.
    */
  class Op[F[_]: Async: Batch] private[io] (
      private[io] val parsers: NonEmptyList[MarkupParser],
      private[io] val renderer: BinaryRenderer[F],
      private[io] val theme: Theme[F],
      private[io] val input: InputTreeBuilder[F],
      private[io] val mapper: TreeMapper[F],
      private[io] val output: BinaryOutput[F]
  ) {

    /** Performs the transformation based on the library's
      * default runtime implementation, suspended in the effect F.
      */
    def transform: F[Unit] = TransformerRuntime.run(this)

    /** Provides a description of this operation, the parsers, renderers
      * and extension bundles used, as well as the sources and output target.
      * This functionality is mostly intended for tooling support.
      */
    def describe: F[TransformerDescriptor] = TransformerDescriptor.create(this)

  }

}
