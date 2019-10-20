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

import laika.api.builder._
import laika.api.{MarkupParser, Renderer, Transformer}
import laika.factory.BinaryPostProcessor

/** Entry point of the builder API for all parser, renderer and transform operations
  * that process a single input and/or output.
  * 
  * It builds on top of the parsers, renderers and transformers from the laika-core module
  * and wraps support for file/stream IO and binary output around them.
  * 
  * Example for transforming a Markdown file to an HTML file:
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
  * val res: IO[Unit] = laika.io.Sequential(transformer)
  *   .build(processingContext, blockingContext)
  *   .fromFile("hello.md")
  *   .toFile("hello.html")
  *   .transform  
  * }}}
  * 
  * The first transformer built in the example in the pure transformer from the laika-core module
  * that specifies the markup and render format and other configuration options and extensions.
  * 
  * The `laika.io.Sequential` entry point then adds IO capabilities to the existing parser.
  * It is based on an abstract effect from the cats-effect library and thus can be used with
  * cats IO, Monix or Zio. The example above shows the usage with cats IO.
  */
object Sequential {

  /** Entry point of the builder API for a parsing operation for a single input.
    * 
    * Example for parsing a Markdown file:
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
    * val res: IO[Document] = laika.io.Sequential(parser)
    *   .build(processingContext, blockingContext)
    *   .fromFile("hello.md")
    *   .parse  
    * }}}
    */
  //def apply (parser: ParserBuilder): text.SequentialParser.Builder = text.SequentialParser.Builder(parser.build)

  /** Entry point of the builder API for a rendering operation for a single character output.
    *
    * Example for rendering an HTML file:
    *
    * {{{
    * implicit val processingContext: ContextShift[IO] = 
    *   IO.contextShift(ExecutionContext.global)
    *
    * val blockingContext: ContextShift[IO] = 
    *   IO.contextShift(ExecutionContext.fromExecutor(Executors.newCachedThreadPool()))  
    *
    * val doc: Document = ??? // obtained from a previous parse operation or constructed in-memory.
    * 
    * val renderer = Renderer.of(HTML)
    *
    * val res: IO[Unit] = laika.io.Sequential(renderer)
    *   .build(processingContext, blockingContext)
    *   .from(doc)
    *   .toFile("out.html")
    *   .render  
    * }}}
    */
//  def apply (renderer: RendererBuilder[_]): text.SequentialRenderer.Builder = 
//    text.SequentialRenderer.Builder(renderer.build)

  /** Entry point of the builder API for a transformation from a single input to a single character output.
    *
    * Example for transforming a file from Markdown to HTML:
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
    * val res: IO[Unit] = laika.io.Sequential(transformer)
    *   .build(processingContext, blockingContext)
    *   .fromFile("input.md")
    *   .toFile("output.html")
    *   .transform  
    * }}}
    */
//  def apply (transformer: TransformerBuilder[_]): text.SequentialTransformer.Builder = 
//    text.SequentialTransformer.Builder(transformer.build)

  /** Entry point of the builder API for a rendering operation for a single binary output.
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
    * val doc: Document = ??? // obtained from a previous parse operation or constructed in-memory.
    *
    * val renderer = Renderer.of(EPUB)
    *
    * val res: IO[Unit] = laika.io.Sequential(renderer)
    *   .build(processingContext, blockingContext)
    *   .from(doc)
    *   .toFile("out.epub")
    *   .render  
    * }}}
    */
//  def apply (renderer: TwoPhaseRendererBuilder[_, BinaryPostProcessor]): binary.SequentialRenderer.Builder = 
//    binary.SequentialRenderer.Builder(renderer.build)

  /** Entry point of the builder API for a transformation from a single input to a single binary output.
    *
    * Example for transforming a file from Markdown to EPUB:
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
    *   .to(EPUB)
    *   .using(GitHubFlavor)
    *
    * val res: IO[Unit] = laika.io.Sequential(transformer)
    *   .build(processingContext, blockingContext)
    *   .fromFile("input.md")
    *   .toFile("output.epub")
    *   .transform  
    * }}}
    */
//  def apply (transformer: TwoPhaseTransformerBuilder[_, BinaryPostProcessor]): binary.SequentialTransformer.Builder = 
//    binary.SequentialTransformer.Builder(transformer.build)

  /** Entry point of the builder API for a parsing operation for a single input.
    *
    * Example for parsing a Markdown file:
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
    * val res: IO[Document] = laika.io.Sequential(parser)
    *   .build(processingContext, blockingContext)
    *   .fromFile("hello.md")
    *   .parse  
    * }}}
    */
  //def apply (parser: MarkupParser): text.SequentialParser.Builder = text.SequentialParser.Builder(parser)

  /** Entry point of the builder API for a rendering operation for a single character output.
    *
    * Example for rendering an HTML file:
    *
    * {{{
    * implicit val processingContext: ContextShift[IO] = 
    *   IO.contextShift(ExecutionContext.global)
    *
    * val blockingContext: ContextShift[IO] = 
    *   IO.contextShift(ExecutionContext.fromExecutor(Executors.newCachedThreadPool()))  
    *
    * val doc: Document = ??? // obtained from a previous parse operation or constructed in-memory.
    *
    * val renderer = Renderer.of(HTML)
    *
    * val res: IO[Unit] = laika.io.Sequential(renderer)
    *   .build(processingContext, blockingContext)
    *   .from(doc)
    *   .toFile("out.html")
    *   .render  
    * }}}
    */
  //def apply (renderer: Renderer): text.SequentialRenderer.Builder = text.SequentialRenderer.Builder(renderer)

  /** Entry point of the builder API for a transformation from a single input to a single character output.
    *
    * Example for transforming a file from Markdown to HTML:
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
    * val res: IO[Unit] = laika.io.Sequential(transformer)
    *   .build(processingContext, blockingContext)
    *   .fromFile("input.md")
    *   .toFile("output.html")
    *   .transform  
    * }}}
    */
//  def apply (transformer: Transformer): text.SequentialTransformer.Builder = 
//    text.SequentialTransformer.Builder(transformer)

  /** Entry point of the builder API for a rendering operation for a single binary output.
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
    * val doc: Document = ??? // obtained from a previous parse operation or constructed in-memory.
    *
    * val renderer = Renderer.of(EPUB)
    *
    * val res: IO[Unit] = laika.io.Sequential(renderer)
    *   .build(processingContext, blockingContext)
    *   .from(doc)
    *   .toFile("out.epub")
    *   .render  
    * }}}
    */
//  def apply (renderer: TwoPhaseRenderer[BinaryPostProcessor]): binary.SequentialRenderer.Builder = 
//    binary.SequentialRenderer.Builder(renderer)

  /** Entry point of the builder API for a transformation from a single input to a single binary output.
    *
    * Example for transforming a file from Markdown to EPUB:
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
    *   .to(EPUB)
    *   .using(GitHubFlavor)
    *
    * val res: IO[Unit] = laika.io.Sequential(transformer)
    *   .build(processingContext, blockingContext)
    *   .fromFile("input.md")
    *   .toFile("output.epub")
    *   .transform  
    * }}}
    */
//  def apply (transformer: TwoPhaseTransformer[BinaryPostProcessor]): binary.SequentialTransformer.Builder = 
//    binary.SequentialTransformer.Builder(transformer)

}  
