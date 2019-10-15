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

package laika.render.pdf

import laika.api.config.Config
import laika.api.Renderer
import laika.ast.Path.Root
import laika.ast._
import laika.format.{PDF, XSLFO}
import laika.io.model.{RenderedDocument, RenderedTree, RenderedTreeRoot}
import laika.render.pdf.PDFNavigation.DocNames

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
  def apply (result: RenderedTreeRoot, config: PDF.Config): String = {

    def concatDocuments: String = {

      def append (sb: StringBuilder, tree: RenderedTree): Unit = {

        tree.titleDocument.foreach(doc => sb.append(doc.content))

        tree.content foreach {
          case d: RenderedDocument => sb.append(d.content)
          case t: RenderedTree => append(sb, t)
          case _ => ()
        }
      }

      val sb = new StringBuilder
      result.coverDocument.foreach(doc => sb.append(doc.content))
      append(sb, result.tree) // TODO - improve formatting
      sb.toString
    }

    def resolveCoverImagePath: Config =
      result.config.getOpt[String]("pdf.coverImage").toOption.flatten.fold(result.config) { uri => // TODO - 0.12 - error handling
        val resolvedUri = PathInfo.fromURI(uri, Root).fold(uri)(_.absolute.toString)
        result.config.withValue("pdf.coverImage", resolvedUri).build
      }

    val resultWithoutToc = result.copy(tree = result.tree.copy(content = result.tree.content.filterNot(_.path == Root / s"${DocNames.toc}.fo")))

    def applyTemplate(foString: String, template: TemplateDocument): String = {
      val foElement = RawContent(Seq("fo"), foString)
      val finalConfig = resolveCoverImagePath
      val finalDoc = Document(
        Path.Root / "merged.fo",
        RootElement(Seq(foElement)),
        fragments = PDFNavigation.generateBookmarks(resultWithoutToc, config.bookmarkDepth),
        config = finalConfig
      )
      val templateApplied = template.applyTo(finalDoc)
      Renderer.of(XSLFO).build.render(templateApplied)
    }

    val defaultTemplate = TemplateDocument(Path.Root / "default.template.fo", result.defaultTemplate)
    applyTemplate(concatDocuments, defaultTemplate)
  }
  
}
