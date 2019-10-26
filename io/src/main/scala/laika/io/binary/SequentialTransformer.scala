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

package laika.io.binary

import cats.effect.{Async, Blocker, ContextShift}
import laika.api.builder.TwoPhaseTransformer
import laika.ast.{DocumentType, TextDocumentType}
import laika.factory.BinaryPostProcessor
import laika.io.ops.SequentialInputOps
import laika.io.binary.SequentialTransformer.BinaryTransformer
import laika.io.model.{BinaryOutput, TextInput}
import laika.io.ops.BinaryOutputOps
import laika.io.runtime.{Runtime, TransformerRuntime}

/** Transformer for a single input and binary output document.
  *
  * @author Jens Halm
  */
class SequentialTransformer[F[_]: Async: Runtime] (transformer: BinaryTransformer) extends SequentialInputOps[F] {

  type InputResult = SequentialTransformer.OutputOps[F]

  val F: Async[F] = Async[F]

  val docType: TextDocumentType = DocumentType.Markup

  def fromInput (input: TextInput[F]): SequentialTransformer.OutputOps[F] = SequentialTransformer.OutputOps(transformer, input)

}

/** Builder API for constructing a transformation from a single input document to a single binary output document.
  */
object SequentialTransformer {

  type BinaryTransformer = TwoPhaseTransformer[BinaryPostProcessor]

  /** Builder step that allows to specify the execution context
    * for blocking IO and CPU-bound tasks.
    */
  case class Builder[F[_]: Async: Runtime] (transformer: BinaryTransformer) {

    /** Final builder step that creates a sequential transformer for binary output.
      */
    def build: SequentialTransformer[F] = new SequentialTransformer[F](transformer)

  }

  /** Builder step that allows to specify the output to render to.
    */
  case class OutputOps[F[_]: Async: Runtime] (transformer: BinaryTransformer, input: TextInput[F]) extends BinaryOutputOps[F] {

    val F: Async[F] = Async[F]

    type Result = Op[F]

    def toOutput (output: F[BinaryOutput]): Op[F] = Op[F](transformer, input, output)

  }

  /** Represents a transformation for a single text input document to a binary output document.
    *
    * It can be run by invoking the `transform` method which delegates to the library's
    * default runtime implementation or by developing a custom runner that performs
    * the transformation based on this operation's properties.
    */
  case class Op[F[_]: Async: Runtime] (transformer: BinaryTransformer, input: TextInput[F], output: F[BinaryOutput]) {

    /** Performs the transformation based on the library's
      * default runtime implementation, suspended in the effect F.
      */
    def transform: F[Unit] = TransformerRuntime.run(this)

  }

}
  
