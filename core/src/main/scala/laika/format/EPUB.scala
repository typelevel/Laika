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

import laika.ast.{DocumentTree, TemplateRoot}
import laika.factory.{RenderFormat, RenderResultProcessor}
import laika.io.Output.BinaryOutput
import laika.io.OutputTree
import laika.io.OutputTree.{ResultTree, StringOutputTree}
import laika.render.HTMLWriter
import laika.render.epub.EPUBContainerWriter

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
class EPUB private (val format: RenderFormat[HTMLWriter], config: Option[EPUB.Config]) extends RenderResultProcessor[HTMLWriter] {


  /** Allows to specify configuration options like the recursion depth for the table of content.
   */
  def withConfig (config: EPUB.Config): EPUB = new EPUB(format, Some(config))


  private lazy val writer = new EPUBContainerWriter(config.getOrElse(EPUB.Config.default))

  /** Renders the HTML that serves as a basis for producing the final EPUB output.
   *  The result should include the output from rendering the documents in the 
   *  specified tree as well as any additional insertions like a table of content.
   *  For this the specified `DocumentTree` instance may get
   *  modified before passing it to the given render function.
   *  
   *  In rare cases where the flexibility provided by `EPUB.Config` is not sufficient,
   *  this method may get overridden.
   * 
   *  @param tree the document tree serving as input for the renderer
   *  @param render the actual render function for producing the HTML output
   *  @return the rendered HTML as a tree of String results
   */
  protected def renderHTML (tree: DocumentTree, render: (DocumentTree, OutputTree) => Unit, defaultTemplate: TemplateRoot): ResultTree = {
    val htmlOutput = new StringOutputTree(tree.path)
    render(tree, htmlOutput)
    htmlOutput.result
  }

  /** Produces an EPUB container from the specified document tree.
   *
   *  It included the following file in the container:
   *
   *  - All text markup in the provided document tree, transformed to HTML by the specified render function.
   *  - All static content in the provided document tree, copied to the same relative path within the EPUB container.
   *  - Metadata and navigation files as required by the EPUB specification, auto-generated from the document tree
   *    and the configuration of this instance.
   *  EPUB container metadata as well as all included  to the specified final output.
   * 
   *  @param tree the tree to render to HTML
   *  @param render the render function for producing the HTML
   *  @param output the output to write the EPUB container to
   */  
  def process (tree: DocumentTree, render: (DocumentTree, OutputTree) => Unit, defaultTemplate: TemplateRoot, output: BinaryOutput): Unit = {
    
    val resultTree = renderHTML(tree, render, defaultTemplate)

    writer.write(tree, resultTree, output)

    // TODO - do we need a default template here?

  }
  
}

/** The default instance of the EPUB renderer.
  */
object EPUB extends EPUB(HTML, None) {

  /** Configuration options for the generated EPUB output.
    *
    *  @param tocDepth the number of levels to generate a table of contents for
    *  @param tocTitle the title for the table of contents
    */
  case class Config(tocDepth: Int = Int.MaxValue, tocTitle: Option[String] = None)

  /** Companion for the creation of `Config` instances.
    */
  object Config {

    /** The default configuration.
      */
    val default: Config = apply()
  }

}
