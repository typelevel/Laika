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

import cats.data.NonEmptyList
import laika.api.builder._
import laika.api.{MarkupParser, Renderer, Transformer}
import laika.factory.BinaryPostProcessor

/** Entry point of the builder API for all parser, renderer and transform operations
  * that process an entire tree of inputs and/or outputs.
  *
  * It builds on top of the parsers, renderers and transformers from the laika-core module
  * and wraps support for file/stream IO and binary output around them.
  * 
  * This type of parser, renderer and transformer is the most powerful of the library, and the only
  * option that supports templating, style sheets, copying of static files from input to output directory
  * and parallel processing.
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
  * Example for transforming a directory of Markdown files to a directory of HTML files:
  *
  * {{{
  * import laika.io.implicits._
  * 
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
  * The first transformer built in the example in the pure transformer from the laika-core module
  * that specifies the markup and render format and other configuration options and extensions.
  *
  * The `laika.io.Parallel` entry point then adds IO capabilities to the existing parser.
  * It is based on an abstract effect from the cats-effect library and thus can be used with
  * cats IO, Monix or Zio. The example above shows the usage with cats IO.
  */
object ParallelX {

  /** Entry point of the builder API for a parsing operation for tree of inputs.
    *
    * Example for parsing a directory of Markdown files:
    *
    * {{{
    * implicit val processingContext: ContextShift[IO] = 
    *   IO.contextShift(ExecutionContext.global)
    *
    * val blockingContext: ContextShift[IO] = 
    *   IO.contextShift(ExecutionContext.fromExecutor(Executors.newCachedThreadPool()))  
    *
    * val parser = Parser
    *   .of(Markdown)
    *   .using(GitHubFlavor)
    *
    * val res: IO[DocumentTreeRoot] = laika.io.Parallel(parser)
    *   .build(processingContext, blockingContext)
    *   .fromDirectory("src")
    *   .parse  
    * }}}
    */
//  def apply (parser: ParserBuilder): text.ParallelParser.Builder = 
//    text.ParallelParser.Builder(NonEmptyList.of(parser.build))

  /** Entry point of the builder API for a rendering operation to a tree of character outputs.
    *
    * Example for rendering HTML files to a directory:
    *
    * {{{
    * implicit val processingContext: ContextShift[IO] = 
    *   IO.contextShift(ExecutionContext.global)
    *
    * val blockingContext: ContextShift[IO] = 
    *   IO.contextShift(ExecutionContext.fromExecutor(Executors.newCachedThreadPool()))  
    *
    * val tree: DocumentTreeRoot = ??? // obtained from a previous parse operation or constructed in-memory.
    *
    * val renderer = Renderer.of(HTML)
    *
    * val res: IO[Unit] = laika.io.Parallel(renderer)
    *   .build(processingContext, blockingContext)
    *   .from(tree)
    *   .toDirectory("target")
    *   .render  
    * }}}
    */
//  def apply (renderer: RendererBuilder[_]): text.ParallelRenderer.Builder = 
//    text.ParallelRenderer.Builder(renderer.build)

  /** Entry point of the builder API for a transformation from a tree of inputs to a tree of character outputs.
    *
    * Example for transforming a directory of Markdown files to HTML:
    *
    * {{{
    * implicit val processingContext: ContextShift[IO] = 
    *   IO.contextShift(ExecutionContext.global)
    *
    * val blockingContext: ContextShift[IO] = 
    *   IO.contextShift(ExecutionContext.fromExecutor(Executors.newCachedThreadPool()))  
    *
    * val transformer = Transformer
    *   .from(Markdown)
    *   .to(HTML)
    *   .using(GitHubFlavor)
    *
    * val res: IO[Unit] = laika.io.Parallel(transformer)
    *   .build(processingContext, blockingContext)
    *   .fromDirectory("src")
    *   .toDirectory("target")
    *   .transform  
    * }}}
    */
//  def apply (transformer: TransformerBuilder[_]): text.ParallelTransformer.Builder = 
//    text.ParallelTransformer.Builder(transformer.build)

  /** Entry point of the builder API for a rendering operation that renders a document tree merged into a single binary 
    * output.
    *
    * Example for rendering an EPUB file:
    *
    * {{{
    * implicit val processingContext: ContextShift[IO] = 
    *   IO.contextShift(ExecutionContext.global)
    *
    * val blockingContext: ContextShift[IO] = 
    *   IO.contextShift(ExecutionContext.fromExecutor(Executors.newCachedThreadPool()))  
    *
    * val tree: DocumentTreeRoot = ??? // obtained from a previous parse operation or constructed in-memory.
    *
    * val renderer = Renderer.of(EPUB)
    *
    * val res: IO[Unit] = laika.io.Parallel(renderer)
    *   .build(processingContext, blockingContext)
    *   .from(tree)
    *   .toFile("out.epub")
    *   .render  
    * }}}
    */
//  def apply (renderer: TwoPhaseRendererBuilder[_, BinaryPostProcessor]): binary.ParallelRenderer.Builder = 
//    binary.ParallelRenderer.Builder(renderer.build)

  /** Entry point of the builder API for a transformation from a tree of inputs merged into to a single binary output.
    *
    * Example for transforming a directory of Markdown files to an EPUB document:
    *
    * {{{
    * implicit val processingContext: ContextShift[IO] = 
    *   IO.contextShift(ExecutionContext.global)
    *
    * val blockingContext: ContextShift[IO] = 
    *   IO.contextShift(ExecutionContext.fromExecutor(Executors.newCachedThreadPool()))  
    *
    * val transformer = Transformer
    *   .from(Markdown)
    *   .to(HTML)
    *   .using(GitHubFlavor)
    *
    * val res: IO[Unit] = laika.io.Parallel(transformer)
    *   .build(processingContext, blockingContext)
    *   .fromDirectory("src")
    *   .toFile("out.epub")
    *   .transform  
    * }}}
    */
//  def apply (transformer: TwoPhaseTransformerBuilder[_, BinaryPostProcessor]): binary.ParallelTransformer.Builder = 
//    binary.ParallelTransformer.Builder(transformer.build)

  /** Entry point of the builder API for a parsing operation for tree of inputs.
    *
    * Example for parsing a directory of Markdown files:
    *
    * {{{
    * implicit val processingContext: ContextShift[IO] = 
    *   IO.contextShift(ExecutionContext.global)
    *
    * val blockingContext: ContextShift[IO] = 
    *   IO.contextShift(ExecutionContext.fromExecutor(Executors.newCachedThreadPool()))  
    *
    * val parser = Parser
    *   .of(Markdown)
    *   .using(GitHubFlavor)
    *
    * val res: IO[DocumentTreeRoot] = laika.io.Parallel(parser)
    *   .build(processingContext, blockingContext)
    *   .fromDirectory("src")
    *   .parse  
    * }}}
    */
  //def apply (parser: MarkupParser): text.ParallelParser.Builder = text.ParallelParser.Builder(NonEmptyList.of(parser))

  /** Entry point of the builder API for a rendering operation to a tree of character outputs.
    *
    * Example for rendering HTML files to a directory:
    *
    * {{{
    * implicit val processingContext: ContextShift[IO] = 
    *   IO.contextShift(ExecutionContext.global)
    *
    * val blockingContext: ContextShift[IO] = 
    *   IO.contextShift(ExecutionContext.fromExecutor(Executors.newCachedThreadPool()))  
    *
    * val tree: DocumentTreeRoot = ??? // obtained from a previous parse operation or constructed in-memory.
    *
    * val renderer = Renderer.of(HTML)
    *
    * val res: IO[Unit] = laika.io.Parallel(renderer)
    *   .build(processingContext, blockingContext)
    *   .from(tree)
    *   .toDirectory("target")
    *   .render  
    * }}}
    */
  //def apply (renderer: Renderer): text.ParallelRenderer.Builder = text.ParallelRenderer.Builder(renderer)

  /** Entry point of the builder API for a transformation from a tree of inputs to a tree of character outputs.
    *
    * Example for transforming a directory of Markdown files to HTML:
    *
    * {{{
    * implicit val processingContext: ContextShift[IO] = 
    *   IO.contextShift(ExecutionContext.global)
    *
    * val blockingContext: ContextShift[IO] = 
    *   IO.contextShift(ExecutionContext.fromExecutor(Executors.newCachedThreadPool()))  
    *
    * val transformer = Transformer
    *   .from(Markdown)
    *   .to(HTML)
    *   .using(GitHubFlavor)
    *
    * val res: IO[Unit] = laika.io.Parallel(transformer)
    *   .build(processingContext, blockingContext)
    *   .fromDirectory("src")
    *   .toDirectory("target")
    *   .transform  
    * }}}
    */
  //def apply (transformer: Transformer): text.ParallelTransformer.Builder = text.ParallelTransformer.Builder(transformer)

  /** Entry point of the builder API for a rendering operation that renders a document tree merged into a single binary 
    * output.
    *
    * Example for rendering an EPUB file:
    *
    * {{{
    * implicit val processingContext: ContextShift[IO] = 
    *   IO.contextShift(ExecutionContext.global)
    *
    * val blockingContext: ContextShift[IO] = 
    *   IO.contextShift(ExecutionContext.fromExecutor(Executors.newCachedThreadPool()))  
    *
    * val tree: DocumentTreeRoot = ??? // obtained from a previous parse operation or constructed in-memory.
    *
    * val renderer = Renderer.of(EPUB)
    *
    * val res: IO[Unit] = laika.io.Parallel(renderer)
    *   .build(processingContext, blockingContext)
    *   .from(tree)
    *   .toFile("out.epub")
    *   .render  
    * }}}
    */
//  def apply (renderer: TwoPhaseRenderer[BinaryPostProcessor]): binary.ParallelRenderer.Builder = 
//    binary.ParallelRenderer.Builder(renderer)

  /** Entry point of the builder API for a transformation from a tree of inputs merged into to a single binary output.
    *
    * Example for transforming a directory of Markdown files to an EPUB document:
    *
    * {{{
    * implicit val processingContext: ContextShift[IO] = 
    *   IO.contextShift(ExecutionContext.global)
    *
    * val blockingContext: ContextShift[IO] = 
    *   IO.contextShift(ExecutionContext.fromExecutor(Executors.newCachedThreadPool()))  
    *
    * val transformer = Transformer
    *   .from(Markdown)
    *   .to(HTML)
    *   .using(GitHubFlavor)
    *
    * val res: IO[Unit] = laika.io.Parallel(transformer)
    *   .build(processingContext, blockingContext)
    *   .fromDirectory("src")
    *   .toFile("out.epub")
    *   .transform  
    * }}}
    */
//  def apply (transformer: TwoPhaseTransformer[BinaryPostProcessor]): binary.ParallelTransformer.Builder = 
//    binary.ParallelTransformer.Builder(transformer)

}
