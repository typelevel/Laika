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

/** Internal runtime for copying bytes from an InputStream to an OutputStream.
  * 
  * @author Jens Halm
  */
object CopyRuntime {

  /**  Copies all bytes from the specified InputStream to the
    *  OutputStream, executing in the blocking ExecutionContext of the implicit Runtime.
    */
  def copy[F[_]: Sync: Runtime] (input: InputStream, output: OutputStream): F[Unit] = (input, output) match {
      
    case (in: FileInputStream, out: FileOutputStream) =>
      Runtime[F].runBlocking {
        Sync[F].delay(in.getChannel.transferTo(0, Integer.MAX_VALUE, out.getChannel))
      }
      
    case _ =>
      Runtime[F].runBlocking {
        Sync[F].delay {
          val buffer = new Array[Byte](8192)
          Iterator.continually(input.read(buffer))
            .takeWhile(_ != -1)
            .foreach { output.write(buffer, 0 , _) }
        }
      }
  }

  /** Copies all bytes from the specified binary Input to the binary Output,
    * executing in the blocking ExecutionContext of the implicit Runtime.
    */
  def copy[F[_]: Sync: Runtime] (input: Resource[F, InputStream], output: Resource[F, OutputStream]): F[Unit] =
    (input, output).tupled.use { case (in, out) => copy(in, out) }

}
