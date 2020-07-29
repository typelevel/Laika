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

package laika.io.helper

import java.io.{ByteArrayInputStream, InputStream}

import cats.effect.{IO, Resource}
import laika.ast.DocumentType.Static
import laika.ast.{DocumentType, Path, TextDocumentType}
import laika.io.model._

import scala.io.Codec

trait InputBuilder {

  object ByteInput {
    def apply (input: String, path: Path)(implicit codec: Codec): BinaryInput[IO] =
      BinaryInput(path, Resource.fromAutoCloseable(IO(new ByteArrayInputStream(input.getBytes(codec.charSet)))))
  }
  
  def build (inputs: Seq[(Path, String)], docTypeMatcher: Path => DocumentType): IO[InputTree[IO]] =
    build(inputs).build(docTypeMatcher, _ => false)

  def build (inputs: Seq[(Path, String)]): InputTreeBuilder[IO] =
    inputs.foldLeft(InputTree[IO]) {
      case (builder, (path, input)) => builder.addString(input, path)
    }
  
}
