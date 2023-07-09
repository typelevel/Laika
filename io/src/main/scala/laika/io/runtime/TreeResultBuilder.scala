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

import laika.config.Config
import laika.ast.{ Document, Navigatable, Path, StyleDeclarationSet, TemplateDocument }
import laika.io.model.FilePath

// TODO - remove in 1.0.0-M2
object TreeResultBuilder {

  sealed trait ParserResult extends Navigatable {
    def sourceFile: Option[FilePath]
  }

  case class DocumentResult(doc: Document) extends ParserResult {
    val path: Path                   = doc.path
    val sourceFile: Option[FilePath] = None
  }

  case class TemplateResult(doc: TemplateDocument, sourceFile: Option[FilePath] = None)
      extends ParserResult {
    val path: Path = doc.path
  }

  case class StyleResult(
      doc: StyleDeclarationSet,
      format: String,
      sourceFile: Option[FilePath] = None
  ) extends ParserResult {
    val path: Path = doc.paths.head
  }

  case class ConfigResult(path: Path, config: Config) extends ParserResult {
    val sourceFile: Option[FilePath] = None
  }

}
