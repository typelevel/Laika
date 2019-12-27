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

import java.io.{File, FileNotFoundException, InputStream}
import java.net.URL

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
  
  def loadUrl[F[_]: Async : Runtime] (url: URL): F[Option[Either[ConfigResourceError, String]]] = {
    
    val stream: F[InputStream] = for {
      con <- Async[F].delay(url.openConnection())
      _   <- Async[F].delay(con.setRequestProperty("Accept", "application/hocon"))
      _   <- Runtime[F].runBlocking(Async[F].delay(con.connect())) 
      str <- Async[F].delay(con.getInputStream)
    } yield str
    
    val input = TextInput.fromStream[F](Root, DocumentType.Config, stream, Codec.UTF8, autoClose = true)
    InputRuntime.readParserInput(input).attempt.map {
      case Left(_: FileNotFoundException) => None
      case Left(t) => Some(Left(ConfigResourceError(s"Unable to load config from URL '${url.toString}': ${t.getMessage}")))
      case Right(res) => Some(Right(res.context.input))
    }
  }
  
}
