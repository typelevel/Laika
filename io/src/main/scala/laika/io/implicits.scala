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

package laika.io

import cats.Parallel
import cats.data.NonEmptyList
import cats.effect.{Async, Blocker, ContextShift}
import laika.api.builder.{ParserBuilder, RendererBuilder, TransformerBuilder, TwoPhaseRendererBuilder, TwoPhaseTransformerBuilder}
import laika.factory.BinaryPostProcessor
import laika.io.ops.IOBuilderOps
import laika.io.text.{ParallelParser, SequentialParser}
import laika.io.runtime.Runtime

/** Implicits that add an `io` method to all builder instances for parsers, renderers and transformers
  * from the laika-core module, adding support for file/stream IO, suspended in the effect of your choice.
  * 
  * The requirements for the effect are `Async` and `ContextShift` for sequential execution,
  * and additionally `cats.Parallel` for parallel execution.
  *
  * Example for transforming a file from Markdown to HTML using the `sequential` builder:
  *
  * {{{
  * implicit val cs: ContextShift[IO] = 
  *   IO.contextShift(ExecutionContext.global)
  *
  * val blocker = Blocker.liftExecutionContext(
  *   ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
  * )
  *
  * val transformer = Transformer
  *   .from(Markdown)
  *   .to(HTML)
  *   .using(GitHubFlavor)
  *   .io(blocker)
  *   .sequential[IO]
  *   .build
  *
  * val res: IO[Unit] = transformer
  *   .fromFile("hello.md")
  *   .toFile("hello.html")
  *   .transform
  * }}}
  * 
  * Example for transforming an entire directory from Markdown to HTML using the `parallel` builder:
  *
  * {{{
  * implicit val cs: ContextShift[IO] = 
  *   IO.contextShift(ExecutionContext.global)
  *
  * val blocker = Blocker.liftExecutionContext(
  *   ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
  * )
  *
  * val transformer = Transformer
  *   .from(Markdown)
  *   .to(HTML)
  *   .using(GitHubFlavor)
  *   .io(blocker)
  *   .parallel[IO](4)
  *   .build
  *
  * val res: IO[Unit] = transformer
  *   .fromDirectory("src")
  *   .toDirectory("target")
  *   .transform
  * }}}
  * 
  * The parallel variants of parser, renderer and transformer are the most powerful of the library, and the only
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
object implicits {

  implicit class ImplicitParserOps (val builder: ParserBuilder) extends AnyVal {

    /** Builder step for specifying the blocker to use for all blocking IO operations.
      */
    def io (blocker: Blocker): IOBuilderOps[SequentialParser.Builder, ParallelParser.Builder] =
      new IOBuilderOps[SequentialParser.Builder, ParallelParser.Builder] {

        def sequential[F[_] : Async : ContextShift]: SequentialParser.Builder[F] = {
          implicit val runtime: Runtime[F] = Runtime.sequential(blocker)
          new SequentialParser.Builder[F](builder.build)
        }

        def parallel[F[_] : Async : ContextShift : Parallel] (parallelism: Int): ParallelParser.Builder[F] = {
          implicit val runtime: Runtime[F] = Runtime.parallel(blocker, parallelism)
          new ParallelParser.Builder[F](NonEmptyList.of(builder.build))
        }
      }
  }

  implicit class ImplicitTextRendererOps (val builder: RendererBuilder[_]) extends AnyVal {

    /** Builder step for specifying the blocker to use for all blocking IO operations.
      */
    def io (blocker: Blocker): IOBuilderOps[text.SequentialRenderer.Builder, text.ParallelRenderer.Builder] =
      new IOBuilderOps[text.SequentialRenderer.Builder, text.ParallelRenderer.Builder] {

        def sequential[F[_] : Async : ContextShift]: text.SequentialRenderer.Builder[F] = {
          implicit val runtime: Runtime[F] = Runtime.sequential(blocker)
          new text.SequentialRenderer.Builder[F](builder.build)
        }

        def parallel[F[_] : Async : ContextShift : Parallel] (parallelism: Int): text.ParallelRenderer.Builder[F] = {
          implicit val runtime: Runtime[F] = Runtime.parallel(blocker, parallelism)
          new text.ParallelRenderer.Builder[F](builder.build)
        }
      }
  }

  implicit class ImplicitBinaryRendererOps (val builder: TwoPhaseRendererBuilder[_, BinaryPostProcessor]) extends AnyVal {

    /** Builder step for specifying the blocker to use for all blocking IO operations.
      */
    def io (blocker: Blocker): IOBuilderOps[binary.SequentialRenderer.Builder, binary.ParallelRenderer.Builder] =
      new IOBuilderOps[binary.SequentialRenderer.Builder, binary.ParallelRenderer.Builder] {

        def sequential[F[_] : Async : ContextShift]: binary.SequentialRenderer.Builder[F] = {
          implicit val runtime: Runtime[F] = Runtime.sequential(blocker)
          new binary.SequentialRenderer.Builder[F](builder.build)
        }

        def parallel[F[_] : Async : ContextShift : Parallel] (parallelism: Int): binary.ParallelRenderer.Builder[F] = {
          implicit val runtime: Runtime[F] = Runtime.parallel(blocker, parallelism)
          new binary.ParallelRenderer.Builder[F](builder.build)
        }
      }
  }

  implicit class ImplicitTextTransformerOps (val builder: TransformerBuilder[_]) extends AnyVal {

    /** Builder step for specifying the blocker to use for all blocking IO operations.
      */
    def io (blocker: Blocker): IOBuilderOps[text.SequentialTransformer.Builder, text.ParallelTransformer.Builder] =
      new IOBuilderOps[text.SequentialTransformer.Builder, text.ParallelTransformer.Builder] {

        def sequential[F[_] : Async : ContextShift]: text.SequentialTransformer.Builder[F] = {
          implicit val runtime: Runtime[F] = Runtime.sequential(blocker)
          new text.SequentialTransformer.Builder[F](builder.build)
        }

        def parallel[F[_] : Async : ContextShift : Parallel] (parallelism: Int): text.ParallelTransformer.Builder[F] = {
          implicit val runtime: Runtime[F] = Runtime.parallel(blocker, parallelism)
          new text.ParallelTransformer.Builder[F](builder.build)
        }
      }
  }

  implicit class ImplicitBinaryTransformerOps (val builder: TwoPhaseTransformerBuilder[_, BinaryPostProcessor]) extends AnyVal {

    /** Builder step for specifying the blocker to use for all blocking IO operations.
      */
    def io (blocker: Blocker): IOBuilderOps[binary.SequentialTransformer.Builder, binary.ParallelTransformer.Builder] =
      new IOBuilderOps[binary.SequentialTransformer.Builder, binary.ParallelTransformer.Builder] {

        def sequential[F[_] : Async : ContextShift]: binary.SequentialTransformer.Builder[F] = {
          implicit val runtime: Runtime[F] = Runtime.sequential(blocker)
          new binary.SequentialTransformer.Builder[F](builder.build)
        }

        def parallel[F[_] : Async : ContextShift : Parallel] (parallelism: Int): binary.ParallelTransformer.Builder[F] = {
          implicit val runtime: Runtime[F] = Runtime.parallel(blocker, parallelism)
          new binary.ParallelTransformer.Builder[F](builder.build)
        }
      }
  }
  
}
