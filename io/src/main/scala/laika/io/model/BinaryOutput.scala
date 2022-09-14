/*
 * Copyright 2012-2022 the original author or authors.
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

package laika.io.model

import cats.effect.{Resource, Sync}
import laika.ast.Path

import java.io.{File, OutputStream}

/** A resource for binary output.
  *
  * Most renderers write character data, but formats like PDF or EPUB
  * require a binary stream to write to.
  * 
  * This is the only I/O type not expressed through a generic `F[_]` or an `fs2.Stream` or `fs2.Pipe`
  * as Laika's binary output needs to work with Java libraries for its EPUB and PDF output.
  */
case class BinaryOutput[F[_]] (resource: Resource[F, OutputStream], path: Path, targetFile: Option[File] = None)

object BinaryOutput {

  def forFile[F[_]: Sync] (file: FilePath, path: Path): BinaryOutput[F] = {
    val resource = Resource.fromAutoCloseable(Sync[F].blocking(
      new java.io.BufferedOutputStream(new java.io.FileOutputStream(file.toJavaFile)))
    )
    BinaryOutput(resource, path)
  }

  def forStream[F[_]: Sync] (stream: F[OutputStream], path: Path, autoClose: Boolean = true): BinaryOutput[F] = {
    val resource = if (autoClose) Resource.fromAutoCloseable(stream) else Resource.eval(stream)
    BinaryOutput(resource, path)
  }

}
