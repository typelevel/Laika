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
import laika.ast.Path.Root
import laika.io.model.InputTree
import laika.io.runtime.{InputRuntime, Runtime}

import scala.io.Codec

private[helium] object MergedCSSGenerator {

  def merge[F[_]: Sync: Runtime](varBlock: String): F[String] = {

    val inputTree = InputTree[F]
      .addClasspathResource("laika/helium/css/container.css", Root / "css" / "container.css")
      .addClasspathResource("laika/helium/css/content.css", Root / "css" / "content.css")
      .addClasspathResource("laika/helium/css/nav.css", Root / "css" / "nav.css")
      .addClasspathResource("laika/helium/css/code.css", Root / "css" / "code.css")
      .addClasspathResource("laika/helium/css/toc.css", Root / "css" / "toc.css")
      .build
    
    def merge (inputs: InputTree[F]): F[String] = {
      inputs.binaryInputs.map(_.asResource).toList.sequence.use { streams =>
        streams.map { stream =>
          InputRuntime.textStreamResource(Sync[F].pure(stream), Codec.UTF8, autoClose = false).use { reader => 
            InputRuntime.readAll(reader, 4000)
          }
        }.sequence.map(_.mkString("\n\n"))
      }
    }
    
    for {
      inputs <- inputTree
      merged <- merge(inputs)
    } yield varBlock + merged
  }
  
}
