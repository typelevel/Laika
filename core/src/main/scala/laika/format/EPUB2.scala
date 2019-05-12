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

package laika.format

import java.time.Instant
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.util.{Locale, UUID}

import laika.ast.Path.Root
import laika.ast._
import laika.execute.InputExecutor
import laika.factory.{RenderContext2, RenderFormat2, RenderResultProcessor2}
import laika.io.{BinaryOutput, RenderResult2}
import laika.render.epub.StyleSupport.XHTMLTemplateParser
import laika.render.epub.{ConfigFactory, ContainerWriter, HtmlRenderExtensions, StyleSupport}
import laika.render.{HTMLFormatter, XHTMLRenderer}

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
object EPUB2 extends RenderResultProcessor2[HTMLFormatter] {

  /** A render format for XHTML output as used by EPUB output.
    *
    * This format is usually not used directly with Laika's `Render` or `Transform` APIs.
    * It is primarily used internally by the parent `EPUB` instance.
    *
    *  @author Jens Halm
    */
  object XHTML extends RenderFormat2[HTMLFormatter] {

    val fileSuffix: String = "epub.xhtml"

    val defaultRenderer: (HTMLFormatter, Element) => String = XHTMLRenderer

    val formatterFactory: RenderContext2[HTMLFormatter] => HTMLFormatter = HTMLFormatter // TODO - 0.12 - needs formatter that closes empty tags

    override lazy val defaultTheme: Theme = Theme(
      customRenderer = HtmlRenderExtensions.all,
      defaultTemplate = Some(templateResource.content)
    )

    private val templateName = "default.template.epub.xhtml"

    private lazy val templateResource: TemplateDocument =
      XHTMLTemplateParser.parse(InputExecutor.classPathParserInput(s"/templates/$templateName", Path.Root / templateName))

  }

  val format: RenderFormat2[HTMLFormatter] = XHTML

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

  def prepareTree (tree: DocumentTree): DocumentTree = {
    val treeConfig = ConfigFactory.forTreeConfig(tree.config)
    val treeWithStyles = StyleSupport.ensureContainsStyles(tree)
    treeConfig.coverImage.fold(tree) { image =>
      treeWithStyles.copy(
        content = Document(Root / "cover", 
          RootElement(Seq(SpanSequence(Seq(Image("cover", URI(image)))))), 
        config = com.typesafe.config.ConfigFactory.parseString("title: Cover")) +: tree.content
      )
    }
  }

  /** Produces an EPUB container from the specified result tree.
   *
   *  It includes the following files in the container:
   *
   *  - All text markup in the provided document tree, transformed to HTML by the specified render function.
   *  - All static content in the provided document tree, copied to the same relative path within the EPUB container.
   *  - Metadata and navigation files as required by the EPUB specification, auto-generated from the document tree
   *    and the configuration of this instance.
   *
   * @param result the result of the render operation as a tree
   * @param output the output to write the final result to
   */
  def process (result: RenderResult2, output: BinaryOutput): Unit = writer.write(result, output)
  
}
