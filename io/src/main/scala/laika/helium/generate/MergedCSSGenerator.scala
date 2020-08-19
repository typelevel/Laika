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

import java.util.concurrent.Executors

import cats.implicits._
import cats.effect.{Blocker, ContextShift, IO, Sync}
import laika.ast.Path.Root
import laika.io.model.InputTree
import laika.io.runtime.InputRuntime
import laika.io.runtime.Runtime

import scala.concurrent.ExecutionContext
import scala.io.Codec

/**
  * @author Jens Halm
  */
object MergedCSSGenerator {

  // TODO - temporary hack - create ThemeBuilder type and pass it in
  private val blocker = Blocker.liftExecutionContext(
    ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
  )

  def merge[F[_]: Sync](varBlock: String): F[String] = {

    // TODO - temporary hack - create ThemeBuilder type and pass it in
    implicit val contextShift: ContextShift[F] = IO.contextShift(ExecutionContext.global).asInstanceOf[ContextShift[F]]
    
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
            implicit val runtime: Runtime[F] = Runtime.sequential(blocker)
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
