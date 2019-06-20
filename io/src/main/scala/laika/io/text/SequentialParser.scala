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
import laika.api.{MarkupParser, Renderer}
import laika.ast.{Document, DocumentType, Element, Path, TextDocumentType}
import laika.io.ops.SequentialInputOps
import laika.io.model.TextInput
import laika.runtime.{ParserRuntime, Runtime}

/**
  * @author Jens Halm
  */
class SequentialParser[F[_]: Async: Runtime] (parser: MarkupParser) extends SequentialInputOps[F] {

  type InputResult = SequentialParser.Op[F]

  val F: Async[F] = Async[F]

  val docType: TextDocumentType = DocumentType.Markup


  def fromInput (input: F[TextInput]): SequentialParser.Op[F] = SequentialParser.Op(parser, input)

}

object SequentialParser {

  case class Builder (parser: MarkupParser) {

    def build[F[_]: Async] (processingContext: ContextShift[F], blockingContext: ContextShift[F]): SequentialParser[F] =
      new SequentialParser[F](parser)(implicitly[Async[F]], Runtime.sequential(processingContext, blockingContext))

  }

  case class Op[F[_]: Async: Runtime] (parser: MarkupParser, input: F[TextInput]) {

    def parse: F[Document] = ParserRuntime.run(this)

  }

}
