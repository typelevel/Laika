/*
 * Copyright 2014-2018 the original author or authors.
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

package laika.format

import java.time.Instant
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.util.{Locale, UUID}

import laika.ast.Path.Root
import laika.ast._
import laika.config.RenderConfig
import laika.execute.InputExecutor
import laika.factory.{RenderFormat, RenderResultProcessor}
import laika.io.Output.BinaryOutput
import laika.io.OutputTree.StringOutputTree
import laika.io.{Output, OutputTree}
import laika.render.epub.StyleSupport.XHTMLTemplateParser
import laika.render.epub.{ConfigFactory, ContainerWriter, HtmlRenderExtensions, StyleSupport}
import laika.render.{HTMLRenderer, HTMLWriter}

/** A post processor for EPUB output, based on an interim HTML renderer.
 *  May be directly passed to the `Render` or `Transform` APIs:
 * 
 *  {{{
 *  val document: Document = ...
 *  Render as EPUB from document toFile "hello.epub"
 *  
 *  Transform from Markdown to PDF fromDirectory "book-src" toFile "book.epub"
 *  }}}
 *  
 *  In the second example above the input from an entire directory gets
 *  merged into a single output file.
 * 
 *  @author Jens Halm
 */
object EPUB extends RenderResultProcessor[HTMLWriter] {

  /** A render format for XHTML output as used by EPUB output.
    *
    * This format is usually not used directly with Laika's `Render` or `Transform` APIs.
    * It is primarily used internally by the parent `EPUB` instance.
    *
    *  @author Jens Halm
    */
  object XHTML extends RenderFormat[HTMLWriter] {

    val fileSuffix: String = "epub.xhtml"

    def newRenderer (output: Output, root: Element, render: Element => Unit,
                     styles: StyleDeclarationSet, config: RenderConfig): (HTMLWriter, Element => Unit) = {

      val writer = new HTMLWriter(output.asFunction, render, root, formatted = config.renderFormatted)
      val renderer = new HTMLRenderer(writer, config.minMessageLevel, "epub.xhtml")

      (writer, renderer.render)
    }

    override lazy val defaultTheme: Theme = Theme(
      customRenderer = HtmlRenderExtensions.all,
      defaultTemplate = Some(templateResource.content)
    )

    private val templateName = "default.template.epub.xhtml"

    private lazy val templateResource: TemplateDocument =
      XHTMLTemplateParser.parse(InputExecutor.classPathParserInput(s"/templates/$templateName", Path.Root / templateName))

  }

  val format = XHTML

  /** Configuration options for the generated EPUB output.
    *
    *  @param metadata the metadata associated with the document
    *  @param tocDepth the number of levels to generate a table of contents for
    *  @param tocTitle the title for the table of contents
    */
  case class Config(metadata: DocumentMetadata = DocumentMetadata(), tocDepth: Int = Int.MaxValue, tocTitle: Option[String] = None, coverImage: Option[String] = None) {
    lazy val identifier: String = metadata.identifier.getOrElse(s"urn:uuid:${UUID.randomUUID.toString}")
    lazy val date: Instant = metadata.date.getOrElse(Instant.now)
    lazy val formattedDate: String = DateTimeFormatter.ISO_INSTANT.format(date.truncatedTo(ChronoUnit.SECONDS))
    lazy val language: Locale = metadata.language.getOrElse(Locale.getDefault)
  }

  /** Companion for the creation of `Config` instances.
    */
  object Config {

    /** The default configuration.
      */
    val default: Config = apply()
  }

  private lazy val writer = new ContainerWriter

  /** Produces an EPUB container from the specified document tree.
   *
   *  It includes the following files in the container:
   *
   *  - All text markup in the provided document tree, transformed to HTML by the specified render function.
   *  - All static content in the provided document tree, copied to the same relative path within the EPUB container.
   *  - Metadata and navigation files as required by the EPUB specification, auto-generated from the document tree
   *    and the configuration of this instance.
   *
   *  @param tree the tree to render to HTML
   *  @param render the render function for producing the HTML
   *  @param output the output to write the EPUB container to
   */  
  def process (tree: DocumentTree, render: (DocumentTree, OutputTree) => Unit, defaultTemplate: TemplateRoot, output: BinaryOutput): Unit = {

    val htmlOutput = new StringOutputTree(tree.path)
    val treeConfig = ConfigFactory.forTree(tree)
    val treeWithStyles = StyleSupport.ensureContainsStyles(tree)
    val treeWithCover = treeConfig.coverImage.fold(tree) { image =>
      treeWithStyles.copy(content = Document(Root / "cover", RootElement(Seq(SpanSequence(Seq(Image("cover", URI(image)))))), config = com.typesafe.config.ConfigFactory.parseString("title: Cover")) +: tree.content)
    }

    render(treeWithCover, htmlOutput)
    writer.write(treeWithCover, treeConfig, htmlOutput.result, output)

  }
  
}
