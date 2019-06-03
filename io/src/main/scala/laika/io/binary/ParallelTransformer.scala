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

import cats.effect.Async
import laika.api.builder.{OperationConfig, TwoPhaseTransformer}
import laika.ast.{DocumentType, TextDocumentType}
import laika.factory.BinaryPostProcessor
import laika.io.binary.ParallelTransformer.BinaryTransformer
import laika.io._
import laika.runtime.TransformerRuntime

/**
  * @author Jens Halm
  */
class ParallelTransformer[F[_]: Async] (transformer: BinaryTransformer) extends ParallelInputOps[F] {

  type Result = ParallelTransformer.OutputOps[F]

  val F: Async[F] = Async[F]

  val docType: TextDocumentType = DocumentType.Markup

  val config: OperationConfig = transformer.markupParser.config


  def fromInput (input: F[TreeInput]): ParallelTransformer.OutputOps[F] = ParallelTransformer.OutputOps(transformer, input)

}

object ParallelTransformer {

  type BinaryTransformer = TwoPhaseTransformer[BinaryPostProcessor]

  case class Builder (transformer: BinaryTransformer) {

    def build[F[_]: Async]: ParallelTransformer[F] = new ParallelTransformer[F](transformer)

  }

  case class OutputOps[F[_]: Async] (transformer: BinaryTransformer, input: F[TreeInput]) extends BinaryOutputOps[F] {

    val F: Async[F] = Async[F]

    type Result = Op[F]

    def toOutput (output: F[BinaryOutput]): Op[F] = Op[F](transformer, input, output)

  }

  case class Op[F[_]: Async] (transformer: BinaryTransformer, input: F[TreeInput], output: F[BinaryOutput]) {

    def transform: F[Unit] = TransformerRuntime.run(this)

  }

}
