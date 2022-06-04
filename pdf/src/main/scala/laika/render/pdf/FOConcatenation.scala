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

import cats.data.NonEmptySet
import cats.implicits._
import laika.api.Renderer
import laika.api.builder.OperationConfig
import laika.ast.Path.Root
import laika.ast._
import laika.config.{Config, ConfigException, LaikaKeys, ValidationError}
import laika.format.{PDF, XSLFO}
import laika.io.model.RenderedTreeRoot
import laika.parse.markup.DocumentParser.InvalidDocument
import laika.rewrite.{DefaultTemplatePath, OutputContext, TemplateRewriter}
import laika.rewrite.nav.{ConfigurablePathTranslator, PathAttributes, PathTranslator, TranslatorConfig}

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
    *  @param opConfig the operation config that had been used to produce the interim XSL-FO result 
    *  @return the rendered XSL-FO merged to a single String 
    */
  def apply[F[_]] (result: RenderedTreeRoot[F], config: PDF.BookConfig, opConfig: OperationConfig): Either[Throwable, String] = {

    def concatDocuments: String = {
      val sb = new StringBuilder
      result.allDocuments.foreach(doc => sb.append(doc.content))
      sb.toString
    }

    def ensureAbsoluteCoverImagePath: Config = config.coverImage.fold(result.config) { path =>
      result.config.withValue("laika.pdf.coverImage", path.toString).build
    }

    def applyTemplate(foString: String, template: TemplateDocument): Either[Throwable, String] = {
      val foElement = RawContent(NonEmptySet.one("fo"), foString)
      val finalConfig = ensureAbsoluteCoverImagePath
      val virtualPath = Path.Root / "merged.fo"
      val finalDoc = Document(
        virtualPath,
        RootElement(foElement),
        fragments = PDFNavigation.generateBookmarks(result, config.navigationDepth),
        config = finalConfig
      )
      val renderer = Renderer.of(XSLFO).withConfig(opConfig).build
      val templateApplied = for {
        rootCursor <- RootCursor(DocumentTreeRoot(DocumentTree(Root, Seq(finalDoc))), Some(OutputContext("fo","pdf")))
        docCursor  <- rootCursor.allDocuments.find(_.path == virtualPath).toRight(ValidationError("internal error"))
        rules      <- opConfig.rewriteRulesFor(finalDoc, RewritePhase.Render)
        doc        <- TemplateRewriter.applyTemplate(docCursor, _ => Right(rules), template)
      } yield doc
      templateApplied
        .leftMap(err => ConfigException(err))
        .flatMap(templatedDoc => InvalidDocument.from(templatedDoc, opConfig.failOnMessages).toLeft(templatedDoc))
        .map(renderer.render(_, result.pathTranslator, result.styles))
    }

    val defaultTemplate = TemplateDocument(DefaultTemplatePath.forFO, result.defaultTemplate)
    applyTemplate(concatDocuments, defaultTemplate)
  }
  
}
