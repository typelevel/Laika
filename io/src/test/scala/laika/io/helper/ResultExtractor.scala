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

import laika.ast.Path
import laika.io.model.RenderedTreeRoot

/**
  * @author Jens Halm
  */
trait ResultExtractor {

  implicit class RenderedTreeRootOps[F[_]] (val root: RenderedTreeRoot[F]) extends StringOps  {

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
