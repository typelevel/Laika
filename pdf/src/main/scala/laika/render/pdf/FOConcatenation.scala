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

import cats.syntax.all.*
import laika.api.Renderer
import laika.api.builder.OperationConfig
import laika.api.config.Config
import laika.api.errors.InvalidDocument
import laika.ast.*
import laika.format.{ PDF, XSLFO }
import laika.io.errors.{ ConfigException, DocumentRendererError }
import laika.io.model.RenderedTreeRoot
import laika.render.FOFormatter.ContentWrapper
import laika.rewrite.{ DefaultTemplatePath, OutputContext }
import laika.theme.config.BookConfig

/** Concatenates the XSL-FO that serves as a basis for producing the final PDF output
  * and applies the default XSL-FO template to the entire result.
  *
  * @author Jens Halm
  */
private[laika] object FOConcatenation {

  /** Concatenates the XSL-FO that serves as a basis for producing the final PDF output
    * and applies the default XSL-FO template to the entire result.
    *
    *  @param result the tree of rendered XSL-FO documents
    *  @param config the configuration to apply
    *  @param opConfig the operation config that had been used to produce the interim XSL-FO result
    *  @return the rendered XSL-FO merged to a single String
    */
  def apply[F[_]](
      result: RenderedTreeRoot[F],
      config: BookConfig,
      opConfig: OperationConfig
  ): Either[Throwable, String] = {

    def concatDocuments: String = {
      val sb = new StringBuilder
      result.allDocuments.foreach(doc => sb.append(doc.content))
      sb.toString
    }

    def ensureAbsoluteCoverImagePath: Config = config.coverImage.fold(result.config) { path =>
      result.config.withValue("laika.pdf.coverImage", path.toString).build
    }

    def applyTemplate(foString: String, template: TemplateDocument): Either[Throwable, String] = {
      val finalConfig     = ensureAbsoluteCoverImagePath
      val virtualPath     = Path.Root / "merged.fo"
      val finalDoc        = Document(virtualPath, RootElement(ContentWrapper(foString)))
        .withFragments(PDFNavigation.generateBookmarks(result, config.navigationDepth))
        .withConfig(finalConfig)
      val renderer        = Renderer.of(XSLFO).withConfig(opConfig).build
      val templateApplied = for {
        rules <- opConfig.rewriteRulesFor(finalDoc, RewritePhase.Render(PDF))
        doc   <- template.applyTo(finalDoc, rules, OutputContext(PDF))
      } yield doc
      templateApplied
        .leftMap(err => ConfigException(err))
        .flatMap(templatedDoc =>
          InvalidDocument.from(templatedDoc, opConfig.failOnMessages).toLeft(templatedDoc)
        )
        .flatMap(
          renderer
            .render(_, result.pathTranslator, result.styles)
            .leftMap(e => DocumentRendererError(e.message, virtualPath))
        )
    }

    val defaultTemplate = TemplateDocument(DefaultTemplatePath.forFO, result.defaultTemplate)
    applyTemplate(concatDocuments, defaultTemplate)
  }

}
