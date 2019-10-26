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

import cats.effect.{Async, Blocker, ContextShift}
import laika.api.MarkupParser
import laika.ast.{Document, DocumentType, TextDocumentType}
import laika.io.model.{TextInput}
import laika.io.ops.SequentialInputOps
import laika.io.runtime.{ParserRuntime, Runtime}

/** Parser for a single input document.
  * 
  * @author Jens Halm
  */
class SequentialParser[F[_]: Async: Runtime] (parser: MarkupParser) extends SequentialInputOps[F] {

  type InputResult = SequentialParser.Op[F]

  val F: Async[F] = Async[F]

  val docType: TextDocumentType = DocumentType.Markup

  def fromInput (input: TextInput[F]): SequentialParser.Op[F] = SequentialParser.Op(parser, input)

}

/** Builder API for constructing a parsing operation for a single input document.
  */
object SequentialParser {

  /** Builder step that allows to specify the execution context
    * for blocking IO and CPU-bound tasks.
    */
  case class Builder[F[_]: Async: Runtime] (parser: MarkupParser) {

    /** Final builder step that creates a sequential parser.
      */
    def build: SequentialParser[F] = new SequentialParser[F](parser)

  }

  /** Represents a parsing operation for a single input document.
    * 
    * It can be run by invoking the `parse` method which delegates to the library's
    * default runtime implementation or by developing a custom runner that performs
    * the parsing based on this operation's properties.
    */
  case class Op[F[_]: Async: Runtime] (parser: MarkupParser, input: TextInput[F]) {

    /** Performs the parsing operation based on the library's
      * default runtime implementation, suspended in the effect F.
      */
    def parse: F[Document] = ParserRuntime.run(this)

  }

}
