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

package laika.io.runtime

import java.io._

import cats.implicits._
import cats.effect.{Sync, Resource}
import fs2._
import cats.effect.IO
import fs2.io.file.Path
import fs2.io.file.Files
import cats.effect.kernel.Async

/** Internal runtime for copying bytes from an InputStream to an OutputStream.
  * 
  * @author Jens Halm
  */
object CopyRuntime {

  /**  Copies all bytes from the specified InputStream to the
    *  OutputStream, executing in the blocking ExecutionContext of the implicit Runtime.
    */
  def copy[F[_]: Sync] (input: InputStream, output: OutputStream): F[Unit] = 
    io.readInputStream(Sync[F].blocking(input),8192,false).through(io.writeOutputStream(Sync[F].blocking(output),false)).compile.drain

  /** Copies all bytes from the specified binary Input to the binary Output,
    * executing in the blocking ExecutionContext of the implicit Runtime.
    */
  def copy[F[_]: Sync] (input: Resource[F, InputStream], output: Resource[F, OutputStream]): F[Unit] =
    (input, output).tupled.use { case (in, out) => copy(in, out) }
  
  def copy[F[_]: Async](input: Resource[F, InputStream],output:Path,chunk:Int = 8192):F[Unit] = {
      type G[T] = Resource[F,T]
      io.readInputStream(input,8192).through(Files[G].writeAll(output)).compile.drain.use_
  }
}
