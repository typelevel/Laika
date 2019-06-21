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

import cats.effect.{Async, ContextShift}
import laika.api.Transformer
import laika.api.builder.OperationConfig
import laika.ast.{DocumentType, TextDocumentType}
import laika.io.model.{RenderedTreeRoot, TreeInput, TreeOutput}
import laika.io.ops.{ParallelInputOps, ParallelTextOutputOps}
import laika.runtime.{Runtime, TransformerRuntime}

/** Transformer for a tree of input and output documents.
  *
  * @author Jens Halm
  */
class ParallelTransformer[F[_]: Async: Runtime] (transformer: Transformer) extends ParallelInputOps[F] {

  type Result = ParallelTransformer.OutputOps[F]

  val F: Async[F] = Async[F]

  val docType: TextDocumentType = DocumentType.Markup

  val config: OperationConfig = transformer.parser.config

  def fromInput (input: F[TreeInput]): ParallelTransformer.OutputOps[F] = ParallelTransformer.OutputOps(transformer, input)

}

/** Builder API for constructing a transformation for a tree of input and output documents.
  */
object ParallelTransformer {

  /** Builder step that allows to specify the execution context
    * for blocking IO and CPU-bound tasks.
    */
  case class Builder (transformer: Transformer) {

    /** Builder step that allows to specify the execution context
      * for blocking IO and CPU-bound tasks.
      *
      * @param processingContext the execution context for CPU-bound tasks
      * @param blockingContext the execution context for blocking IO
      * @param parallelism the desired level of parallelism for all tree operations                       
      */
    def build[F[_]: Async, G[_]](processingContext: ContextShift[F], blockingContext: ContextShift[F], parallelism: Int)
                                (implicit P: cats.Parallel[F, G]): ParallelTransformer[F] =
      new ParallelTransformer[F](transformer)(implicitly[Async[F]], Runtime.parallel(processingContext, blockingContext, parallelism))

    /** Builder step that allows to specify the execution context
      * for blocking IO and CPU-bound tasks.
      *
      * The level of parallelism is determined from the number of available CPUs.
      * Use the other `build` method if you want to specify the parallelism explicitly.
      * 
      * @param processingContext the execution context for CPU-bound tasks
      * @param blockingContext the execution context for blocking IO
      */
    def build[F[_]: Async, G[_]](processingContext: ContextShift[F], blockingContext: ContextShift[F])
                                (implicit P: cats.Parallel[F, G]): ParallelTransformer[F] =
      build(processingContext, blockingContext, java.lang.Runtime.getRuntime.availableProcessors)

  }

  /** Builder step that allows to specify the output to render to.
    */
  case class OutputOps[F[_]: Async: Runtime] (transformer: Transformer, input: F[TreeInput]) extends ParallelTextOutputOps[F] {

    val F: Async[F] = Async[F]

    type Result = Op[F]

    def toOutput (output: F[TreeOutput]): Op[F] = Op[F](transformer, input, output)

  }

  /** Represents a transformation for a tree of input documents.
    *
    * It can be run by invoking the `transform` method which delegates to the library's
    * default runtime implementation or by developing a custom runner that performs
    * the transformation based on this operation's properties.
    */
  case class Op[F[_]: Async: Runtime] (transformer: Transformer, input: F[TreeInput], output: F[TreeOutput]) {

    /** Performs the transformation based on the library's
      * default runtime implementation, suspended in the effect F.
      */
    def transform: F[RenderedTreeRoot] = TransformerRuntime.run(this)

  }

}