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

package laika.helium.generate

import cats.effect.Sync
import cats.implicits._
import laika.io.model.BinaryInput
import laika.io.runtime.InputRuntime

import scala.io.Codec

private[helium] object MergedStringInputs {

  def merge[F[_]: Sync](inputs: Seq[BinaryInput[F]]): F[String] = {
    inputs.map(_.input).toList.sequence.use { streams =>
      streams.map { stream =>
        InputRuntime.textStreamResource(Sync[F].pure(stream), Codec.UTF8, autoClose = false).use { reader =>
          InputRuntime.readAll(reader, 4000)
        }
      }.sequence.map(_.mkString("\n\n"))
    }
  }
  
}
