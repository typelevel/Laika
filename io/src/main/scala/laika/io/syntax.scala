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

package laika.io

import cats.data.{ Kleisli, NonEmptyList }
import cats.effect.{ Async, Sync }
import laika.api.builder.*
import laika.api.format.BinaryPostProcessor
import laika.helium.Helium
import laika.io.api.*
import laika.io.internal.runtime.Batch
import laika.io.ops.IOBuilderOps

/** Implicits that add `sequential[F[_]]` and `parallel[F[_]]` methods to all builder instances for parsers,
  * renderers and transformers from the laika-core module, adding support for file/stream IO,
  * suspended in the effect of your choice.
  *
  * The requirements for the effect are `Sync` for sequential execution and `Async` for parallel execution.
  *
  * Example for transforming an entire directory from Markdown to HTML using the `parallel` builder:
  *
  * {{{
  * val transformer = Transformer
  *   .from(Markdown)
  *   .to(HTML)
  *   .using(GitHubFlavor)
  *   .parallel[IO](4)
  *   .build
  *
  * val res: IO[Unit] = transformer.use {
  *   _.fromDirectory("src")
  *    .toDirectory("target")
  *    .transform
  * }
  * }}}
  *
  * These variants of parser, renderer and transformer are the most powerful of the library, and the only
  * options that support templating, style sheets and the copying of static files from input to output directory.
  *
  * This API can be used to produce an entire HTML site or e-books in the EPUB or PDF format.
  * It is also the basis of all tasks in the sbt plugin, that only adds a thin layer of sbt tasks
  * and settings on top of the library API.
  *
  * The tree for all internal processing steps is based on a virtual path, where in case
  * of directory input and output the root of the virtual tree is mapped to the directory.
  * This abstraction allows the merging of multiple input directories or the addition
  * of inputs constructed in memory to those obtained from the file system.
  *
  * In the case of binary output like EPUB or PDF, the document tree will be rendered
  * to a single, merged output. In case of HTML the output is a tree of separate HTML documents.
  * The API adjusts to the format in use, e.g. EPUB and PDF transformers will offer a
  * `fromDirectory` ... `toFile` flow whereas HTML will offer `fromDirectory` ... `toDirectory`.
  *
  * @author Jens Halm
  */
object syntax {

  implicit class ParserSyntax(builder: ParserBuilder)
      extends IOBuilderOps[TreeParser.Builder] {

    protected def build[F[_]: Async: Batch]: TreeParser.Builder[F] =
      new TreeParser.Builder[F](NonEmptyList.of(builder.build), Helium.defaults.build)

  }

  implicit class TextRendererSyntax(builder: RendererBuilder[_])
      extends IOBuilderOps[TreeRenderer.Builder] {

    protected def build[F[_]: Async: Batch]: TreeRenderer.Builder[F] =
      new TreeRenderer.Builder[F](builder.build.skipRewritePhase, Helium.defaults.build.build)

  }

  implicit class TextTransformerSyntax(builder: TransformerBuilder[_])
      extends IOBuilderOps[TreeTransformer.Builder] {

    protected def build[F[_]: Async: Batch]: TreeTransformer.Builder[F] = {
      val transformer = builder.build
      new TreeTransformer.Builder[F](
        NonEmptyList.of(transformer.parser),
        transformer.renderer,
        Helium.defaults.build,
        Kleisli(Sync[F].pure)
      )
    }

  }

  implicit class BinaryRendererSyntax(
      builder: TwoPhaseRendererBuilder[_, BinaryPostProcessor.Builder]
  ) extends IOBuilderOps[BinaryTreeRenderer.Builder] {

    protected def build[F[_]: Async: Batch]: BinaryTreeRenderer.Builder[F] = {
      new BinaryTreeRenderer.Builder[F](
        builder.twoPhaseFormat,
        builder.config,
        Helium.defaults.build.build
      )
    }

  }

  implicit class BinaryTransformerSyntax(
      builder: TwoPhaseTransformerBuilder[_, BinaryPostProcessor.Builder]
  ) extends IOBuilderOps[BinaryTreeTransformer.Builder] {

    protected def build[F[_]: Async: Batch]: BinaryTreeTransformer.Builder[F] = {
      val parser = new ParserBuilder(builder.markupFormat, builder.config).build
      new BinaryTreeTransformer.Builder[F](
        NonEmptyList.of(parser),
        builder.twoPhaseRenderFormat,
        builder.config,
        Helium.defaults.build,
        Kleisli(Sync[F].pure)
      )
    }

  }

}
