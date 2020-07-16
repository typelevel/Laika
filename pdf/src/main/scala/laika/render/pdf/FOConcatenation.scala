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

package laika.render.pdf

import cats.implicits._
import cats.data.NonEmptySet
import laika.config.{Config, ConfigError}
import laika.api.Renderer
import laika.ast.Path.Root
import laika.ast._
import laika.format.{PDF, XSLFO}
import laika.io.model.{RenderedDocument, RenderedTree, RenderedTreeRoot}

/** Concatenates the XSL-FO that serves as a basis for producing the final PDF output
  * and applies the default XSL-FO template to the entire result.
  * 
  * @author Jens Halm
  */
object FOConcatenation {

  /** Concatenates the XSL-FO that serves as a basis for producing the final PDF output
    * and applies the default XSL-FO template to the entire result.
    *
    *  @param result the tree of rendered XSL-FO documents
    *  @param config the configuration to apply
    *  @return the rendered XSL-FO merged to a single String 
    */
  def apply[F[_]] (result: RenderedTreeRoot[F], config: PDF.BookConfig): Either[ConfigError, String] = {

    def concatDocuments: String = {
      val sb = new StringBuilder
      result.allDocuments.foreach(doc => sb.append(doc.content))
      sb.toString
    }

    def ensureAbsoluteCoverImagePath: Config = config.coverImage.fold(result.config) { path =>
      result.config.withValue("laika.pdf.coverImage", path.toString).build
    }

    def applyTemplate(foString: String, template: TemplateDocument): Either[ConfigError, String] = {
      val foElement = RawContent(NonEmptySet.one("fo"), foString)
      val finalConfig = ensureAbsoluteCoverImagePath
      val finalDoc = Document(
        Path.Root / "merged.fo",
        RootElement(foElement),
        fragments = PDFNavigation.generateBookmarks(result, config.navigationDepth),
        config = finalConfig
      )
      val renderer = Renderer.of(XSLFO).build
      template.applyTo(finalDoc).map(renderer.render)
    }

    val defaultTemplate = TemplateDocument(Path.Root / "default.template.fo", result.defaultTemplate)
    applyTemplate(concatDocuments, defaultTemplate)
  }
  
}
