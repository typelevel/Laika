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
import cats.effect.Sync
import laika.api.builder.{OperationConfig, ParserBuilder}
import laika.api.{MarkupParser, Renderer}
import laika.ast.{DocumentType, TextDocumentType}
import laika.io.api.BinaryTreeTransformer.TreeMapper
import laika.io.descriptor.TransformerDescriptor
import laika.io.model.{InputTreeBuilder, ParsedTree, RenderedTreeRoot, TreeOutput}
import laika.io.ops.{InputOps, TextOutputOps, TreeMapperOps}
import laika.io.runtime.{Runtime, TransformerRuntime}
import laika.io.theme.Theme

/** Transformer for a tree of input and output documents.
  *
  * @author Jens Halm
  */
class TreeTransformer[F[_]: Sync: Runtime](parsers: NonEmptyList[MarkupParser],
                                           renderer: Renderer,
                                           theme: Theme[F],
                                           mapper: TreeMapper[F]) extends InputOps[F] {

  type Result = TreeTransformer.OutputOps[F]

  val F: Sync[F] = Sync[F]

  val docType: TextDocumentType = DocumentType.Markup

  val config: OperationConfig = parsers
    .map(_.config)
    .reduceLeft[OperationConfig](_ merge _)
    .withBundles(theme.extensions)

  def fromInput (input: InputTreeBuilder[F]): TreeTransformer.OutputOps[F] =
    TreeTransformer.OutputOps(parsers, renderer, theme, input, mapper)

}

/** Builder API for constructing a transformation for a tree of input and output documents.
  */
object TreeTransformer {

  /** Builder step that allows to specify the execution context
    * for blocking IO and CPU-bound tasks.
    */
  case class Builder[F[_]: Sync: Runtime] (parsers: NonEmptyList[MarkupParser], 
                                           renderer: Renderer, 
                                           theme: Theme[F], 
                                           mapper: TreeMapper[F]) extends TreeMapperOps[F] {

    type MapRes = Builder[F]
    
    def evalMapTree (f: ParsedTree[F] => F[ParsedTree[F]]): MapRes = new Builder[F](parsers, renderer, theme, mapper.andThen(f))

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

    /** Applies the specified theme to this transformer, overriding any previously specified themes.
      */
    def withTheme (theme: Theme[F]): Builder[F] = copy(theme = theme)
    
    /** Final builder step that creates a parallel transformer.
      */
    def build: TreeTransformer[F] = new TreeTransformer[F](parsers, renderer, theme, mapper)

  }

  /** Builder step that allows to specify the output to render to.
    */
  case class OutputOps[F[_]: Sync: Runtime] (parsers: NonEmptyList[MarkupParser],
                                             renderer: Renderer,
                                             theme: Theme[F],
                                             input: InputTreeBuilder[F],
                                             mapper: TreeMapper[F]) extends TextOutputOps[F] {

    val F: Sync[F] = Sync[F]

    type Result = Op[F]

    def toOutput (output: TreeOutput): Op[F] = Op[F](parsers, renderer, theme, input, mapper, output)

  }

  /** Represents a transformation for a tree of input documents.
    *
    * It can be run by invoking the `transform` method which delegates to the library's
    * default runtime implementation or by developing a custom runner that performs
    * the transformation based on this operation's properties.
    */
  case class Op[F[_]: Sync: Runtime] (parsers: NonEmptyList[MarkupParser],
                                      renderer: Renderer,
                                      theme: Theme[F],
                                      input: InputTreeBuilder[F],
                                      mapper: TreeMapper[F],
                                      output: TreeOutput) {

    /** Performs the transformation based on the library's default runtime implementation, suspended in the effect F.
      */
    def transform: F[RenderedTreeRoot[F]] = TransformerRuntime.run(this)

    /** Provides a description of this operation, the parsers, renderers and extension bundles used, 
      * as well as the sources and output target.
      * This functionality is mostly intended for tooling support.
      */
    def describe: F[TransformerDescriptor] = TransformerDescriptor.create(this)
    
  }

}