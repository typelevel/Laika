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
import laika.api.builder.TwoPhaseTransformer
import laika.ast.{DocumentType, TextDocumentType}
import laika.factory.BinaryPostProcessor
import laika.io.binary.SequentialTransformer.BinaryTransformer
import laika.io.{BinaryOutput, SequentialInputOps, TextInput}

/**
  * @author Jens Halm
  */
class SequentialTransformer[F[_]: Async] (transformer: BinaryTransformer) extends SequentialInputOps[F] {

  type InputResult = SequentialTransformer.OutputOps[F]

  val F: Async[F] = Async[F]

  val docType: TextDocumentType = DocumentType.Markup


  def fromInput (input: F[TextInput]): SequentialTransformer.OutputOps[F] = SequentialTransformer.OutputOps(transformer, input)

}

object SequentialTransformer {

  type BinaryTransformer = TwoPhaseTransformer[BinaryPostProcessor]

  case class Builder (transformer: BinaryTransformer) {

    def build[F[_]: Async]: SequentialTransformer[F] = new SequentialTransformer[F](transformer)

  }

  case class OutputOps[F[_]: Async] (transformer: BinaryTransformer, input: F[TextInput]) extends BinaryOutputOps[F] {

    val F: Async[F] = Async[F]

    type Result = Op[F]

    def toOutput (output: F[BinaryOutput]): Op[F] = Op[F](transformer, input, output)

  }

  case class Op[F[_]: Async] (transformer: BinaryTransformer, input: F[TextInput], output: F[BinaryOutput]) {

    def transform: F[Unit] = ???

  }

}
  
