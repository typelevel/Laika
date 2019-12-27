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

package laika.io.config

import java.io.File

import cats.effect.Async
import cats.implicits._
import laika.ast.DocumentType
import laika.ast.Path.Root
import laika.config.ConfigResourceError
import laika.io.model.TextInput
import laika.io.runtime.{InputRuntime, Runtime}

import scala.io.Codec

/**
  * @author Jens Halm
  */
object ResourceLoader {

  def loadFile[F[_]: Async : Runtime] (file: String): F[Option[Either[ConfigResourceError, String]]] = 
    loadFile(new File(file))

  def loadFile[F[_]: Async : Runtime] (file: File): F[Option[Either[ConfigResourceError, String]]] = {
    
    def load: F[Either[ConfigResourceError, String]] = {
      val input = TextInput.fromFile[F](Root, DocumentType.Config, file, Codec.UTF8)
      InputRuntime.readParserInput(input).attempt.map(_.bimap(
        t => ConfigResourceError(s"Unable to load file '${file.getPath}': ${t.getMessage}"), 
        _.context.input
      ))
    }
    
    for {
      exists <- Async[F].delay(file.exists())
      res    <- (if (exists) load.map(Option(_)) else Async[F].pure(None)): F[Option[Either[ConfigResourceError, String]]]
    } yield res
  }
  
  def loadClasspathResource[F[_]: Async : Runtime] (resource: String): F[Option[Either[ConfigResourceError, String]]] = 
    loadFile(getClass.getResource(resource).getFile)
  
  def loadUrl[F[_]: Async : Runtime] (url: String): F[Option[Either[ConfigResourceError, String]]] = ???
  
}
