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

package laika.io.config

import java.io.{File, FileNotFoundException, InputStream}
import java.net.URL
import cats.effect.{Async, Sync}
import cats.syntax.all._
import laika.ast.DocumentType
import laika.ast.Path.Root
import laika.config.ConfigResourceError
import laika.io.model.TextInput

import scala.io.Codec

/** Internal utility for loading text resources from the file system, the classpath or via its URL.
  * 
  * @author Jens Halm
  */
object ResourceLoader {

  /** Load the file with the specified name.
    * 
    * If the file does not exist the result will be `None`.
    * If it does exist, but fails to load or parse correctly the result will be `Some(Left(...))`,
    * successfully parsed resources will be returned as `Some(Right(...))`.
    */
  def loadFile[F[_]: Async] (file: String): F[Option[Either[ConfigResourceError, String]]] = 
    loadFile(new File(file))

  /** Load the specified file (which may be a file on the file system or a classpath resource).
    *
    * If the file does not exist the result will be `None`.
    * If it does exist, but fails to load or parse correctly the result will be `Some(Left(...))`,
    * successfully parsed resources will be returned as `Some(Right(...))`.
    */
  def loadFile[F[_]: Async] (file: File): F[Option[Either[ConfigResourceError, String]]] = {
    
    def load: F[Either[ConfigResourceError, String]] = {
      val input = TextInput.fromFile[F](Root, DocumentType.Config, file, Codec.UTF8)
      input.asDocumentInput.attempt.map(_.bimap(
        t => ConfigResourceError(s"Unable to load file '${file.getPath}': ${t.getMessage}"), 
        _.source.input
      ))
    }
    
    for {
      exists <- Sync[F].delay(file.exists())
      res    <- (if (exists) load.map(Option(_)) else Sync[F].pure(None)): F[Option[Either[ConfigResourceError, String]]]
    } yield res
  }

  /** Load the specified classpath resource. 
    * 
    * The resource name is interpreted as absolute and should not start with a leading `/`.
    *
    * If the resource does not exist the result will be `None`.
    * If it does exist, but fails to load or parse correctly the result will be `Some(Left(...))`,
    * successfully parsed resources will be returned as `Some(Right(...))`.
    */
  def loadClasspathResource[F[_]: Async] (resource: String): F[Option[Either[ConfigResourceError, String]]] = 
    Option(getClass.getClassLoader.getResource(resource)) match {
      case Some(url) => loadFile(url.getFile)
      case None => Sync[F].pure(None)
    }

  /** Load the configuration from the specified URL.
    *
    * If the resource does not exist (404 response) the result will be `None`.
    * If it does exist, but fails to load or parse correctly the result will be `Some(Left(...))`,
    * successfully parsed resources will be returned as `Some(Right(...))`.
    */
  def loadUrl[F[_]: Sync] (url: URL): F[Option[Either[ConfigResourceError, String]]] = {
    
    val stream: F[InputStream] = for {
      con <- Sync[F].delay(url.openConnection())
      _   <- Sync[F].delay(con.setRequestProperty("Accept", "application/hocon"))
      _   <- Sync[F].blocking(con.connect())
      str <- Sync[F].delay(con.getInputStream)
    } yield str
    
    val input = TextInput.fromStream[F](Root, DocumentType.Config, stream, Codec.UTF8, autoClose = true)
    input.asDocumentInput.attempt.map {
      case Left(_: FileNotFoundException) => None
      case Left(t) => Some(Left(ConfigResourceError(s"Unable to load config from URL '${url.toString}': ${t.getMessage}")))
      case Right(res) => Some(Right(res.source.input))
    }
  }
  
}
