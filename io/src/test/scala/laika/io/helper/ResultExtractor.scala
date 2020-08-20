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

import java.io.InputStream

import cats.implicits._
import cats.effect.{Blocker, ContextShift, Resource, Sync}
import laika.ast.Path
import laika.io.model.RenderedTreeRoot
import laika.io.runtime.{InputRuntime, Runtime}

import scala.io.Codec

/**
  * @author Jens Halm
  */
trait ResultExtractor {

  implicit class RenderedTreeRootOps[F[_]: Sync: ContextShift](val root: RenderedTreeRoot[F]) extends StringOps  {

    // TODO - move to InputRuntime
    private def readText (stream: Resource[F, InputStream], blocker: Blocker): F[String] = {
      stream.flatMap { str =>
        InputRuntime.textStreamResource(Sync[F].pure(str), Codec.UTF8, autoClose = false)
      }.use { reader =>
        implicit val runtime: Runtime[F] = Runtime.sequential(blocker)
        InputRuntime.readAll(reader, 4000)
      }
    }
    
    def extractStaticContent (path: Path, start: String, end: String, blocker: Blocker): F[String] =
      for {
        input   <- Sync[F].fromEither(root.staticDocuments.find(_.path == path)
                          .toRight(new RuntimeException(s"Not found: '$path'")))
        content <- readText(input.asResource, blocker)
        res     <- Sync[F].fromEither(content.extract(start, end)
                          .toRight(new RuntimeException(s"No substring between: '$start' and '$end'")))
      } yield res.removeIndentation.removeBlankLines
    
    def extractTidiedSubstring (path: Path, start: String, end: String): Option[String] =
      for {
        doc  <- root.allDocuments.find(_.path == path)
        res  <- doc.content.extract(start, end)
      } yield res.removeIndentation.removeBlankLines

    def extractTidiedTagContent (path: Path, tagName: String): Option[String] =
      for {
        doc  <- root.allDocuments.find(_.path == path)
        res  <- doc.content.extractTag(tagName)
      } yield res.removeIndentation.removeBlankLines

  }
  
}
