/*
 * Copyright 2013-2016 the original author or authors.
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

package laika.api

import laika.ast._
import laika.config.{OperationConfig, RenderConfigBuilder}
import laika.factory.{RenderFormat, RenderResultProcessor}
import laika.io.Output.Binary
import laika.io.OutputTree._
import laika.io._
import laika.rewrite.TemplateRewriter

/** API for performing a render operation to various types of output using an existing
 *  document tree model. 
 *  
 *  In cases where a render operation follows a parse operation 
 *  immediately, it is more convenient to use the [[laika.api.Transform]] API 
 *  instead which combines a parse and a render operation directly.
 *  
 *  Example for rendering HTML to a file:
 *  
 *  {{{
 *  val doc: Document = ...
 *  
 *  Render as HTML from doc toFile "hello.html"
 *  }}}
 *  
 *  Example for rendering HTML from an entire tree of documents to a directory:
 *  
 *  {{{
 *  val tree: DocumentTree = ...
 *  
 *  Render as HTML from tree toDirectory "path/to/output"
 *  }}}
 *  
 *  Example for rendering PDF from an entire tree of documents to a single target file:
 *  
 *  {{{
 *  val tree: DocumentTree = ...
 *  
 *  Render as PDF from tree toFile "hello.pdf"
 *  }}}
 *  
 *  @tparam Writer the writer API to use which varies depending on the renderer
 * 
 *  @author Jens Halm
 */
abstract class Render[Writer] private (protected[api] val format: RenderFormat[Writer],
                                       val config: OperationConfig) extends RenderConfigBuilder[Writer] {

  protected[this] lazy val theme = config.themeFor(format)


  /** The output operations that can be performed for a single input document.
   */
  type DocOps

  /** The output operations that can be performed for an entire tree of input documents.
   */
  type TreeOps

  /** The concrete implementation of the abstract Render type.
   */
  type ThisType <: Render[Writer]


  @deprecated("renamed to rendering for consistency", "0.9.0")
  def using (render: Writer => RenderFunction): ThisType = rendering(render)

  /** Specifies the element to render. This may be a `RootElement` instance
   *  as well as any other type of `Element`, thus allowing to render document
   *  fragments, too.
   *
   *  @param elem the element to render
   *  @return a new Operation instance that allows to specify the output
   */
  def from (elem: Element): DocOps

  /** Specifies the document to render.
   *
   *  @param doc the document to render
   *  @return a new Operation instance that allows to specify the output
   */
  def from (doc: Document): DocOps

  /** Specifies the document tree to render.
   *
   *  @param tree the document tree to render
   *  @return a new BatchOperation instance that allows to specify the outputs
   */
  def from (tree: DocumentTree): TreeOps


  /** Renders the specified element to the given output.
   * 
   *  @param element the element to render
   *  @param output the output to render to
   *  @param styles the styles to apply to the elements during the render process
   */
  protected[this] def render (element: Element, output: Output, styles: StyleDeclarationSet): Unit = { 
    
    class Renderer (out: Output) extends (Element => Unit) {
      lazy val (writer, renderF) = format.newRenderer(out, element, this, styles, config)

      lazy val mainF: Element => Unit = theme.customRenderer(writer).applyOrElse(_, renderF)

      def apply (element: Element) = mainF(element)
    }

    IO(output) { out =>
      new Renderer(out).apply(element)
      out.flush()
    }
  }
  
  /** Renders the specified document tree to the given output.
   * 
   *  @param docTree the tree to render
   *  @param outputTree the output tree to render to
   */
  protected[this] def render (docTree: DocumentTree, outputTree: OutputTree): Unit = {

    type Operation = () => Unit

    def renderTree (outputTree: OutputTree, styles: StyleDeclarationSet, path: Path, content: RootElement): Operation = {
      val output = outputTree.newOutput(path.basename +"."+ format.fileSuffix)
      () => render(content, output, styles)
    }

    def copy (outputTree: OutputTree)(input: Input): Operation = {
      val output = outputTree.newOutput(input.path.name)
      () => IO.copy(input, output)
    }

    def collectOperations (outputTree: OutputTree, parentStyles: StyleDeclarationSet, docTree: DocumentTree): Seq[Operation] = {

      def isOutputRoot (source: DocumentTree) = (source.sourcePaths.headOption, outputTree) match {
        case (Some(inPath), out: DirectoryOutputTree) => inPath == out.directory.getAbsolutePath
        case _ => false
      }

      val styles = parentStyles ++ docTree.styles(format.fileSuffix)

      (docTree.content flatMap {
        case doc: Document => Seq(renderTree(outputTree, styles, doc.path, doc.content))
        case tree: DocumentTree if !isOutputRoot(tree) => collectOperations(outputTree.newChild(tree.name), styles, tree)
        case _ => Seq()
      }) ++
      (docTree.additionalContent flatMap {
        case doc: DynamicDocument => Seq(renderTree(outputTree, styles, doc.path, doc.content))
        case static: StaticDocument if outputTree.acceptsStaticFiles => Seq(copy(outputTree)(static.input))
        case _ => Seq()
      })
    }

    val templateName = "default.template." + format.fileSuffix
    val treeWithTpl = if (docTree.selectTemplate(Path.Current / templateName).isDefined) docTree
                      else docTree.copy(templates = docTree.templates :+ TemplateDocument(Path.Root / templateName,
                           theme.defaultTemplateOrFallback))
    val treeWithTplApplied = TemplateRewriter.applyTemplates(treeWithTpl, format.fileSuffix)
    val finalTree = theme.staticDocuments.merge(treeWithTplApplied)
    val operations = collectOperations(outputTree, theme.defaultStyles, finalTree)

    (if (config.parallel) operations.par else operations) foreach (_())
  }
    

}

/** Serves as an entry point to the Render API.
 * 
 *  @author Jens Halm
 */
object Render {
  
  /** A render operation that maps each input document of a
   *  given input tree to a corresponding output document
   *  in the destination tree.
   *  
   *  @param format the factory for the rendere to use
   *  @param cfg the configuration for the render operation
   */
  class RenderMappedOutput[Writer] (format: RenderFormat[Writer],
                                    cfg: OperationConfig) extends Render[Writer](format, cfg) {

    type DocOps = TextOuputOps
    type TreeOps = OutputTreeOps
    type ThisType = RenderMappedOutput[Writer]

    def withConfig(newConfig: OperationConfig): ThisType =
      new RenderMappedOutput[Writer](format, newConfig)

    def from (element: Element): TextOuputOps = new TextOuputOps {
      def toOutput (out: Output) = render(element, out, theme.defaultStyles)
    }
    
    def from (doc: Document): TextOuputOps = from(doc.content)
    
    def from (tree: DocumentTree): OutputTreeOps = new OutputTreeOps {
      def toOutputTree (out: OutputTree) = render(tree, out)
    }
    
  }
  
  /** A render operation that gathers input from one or more
   *  input documents in an input tree structure to be rendered 
   *  to a single output destination.
   *  
   *  This is necessary for formats like PDF, where the output
   *  will be contained in a single file, but the input can still
   *  be conveniently organized in a full directory structure.
   *  
   *  @param processor the processor that merges the results from the individual render operations into a single output
   *  @param cfg the configuration for the render operation
   */
  class RenderGatheredOutput[Writer] (processor: RenderResultProcessor[Writer],
                                      cfg: OperationConfig) extends Render[Writer](processor.format, cfg) {
    
    type DocOps = BinaryOutputOps
    type TreeOps = BinaryOutputOps
    type ThisType = RenderGatheredOutput[Writer]

    def withConfig(newConfig: OperationConfig): ThisType =
      new RenderGatheredOutput[Writer](processor, newConfig)

    def from (element: Element): BinaryOutputOps =
      from(Document(Path.Root / "target", RootElement(Seq(TemplateRoot(Seq(TemplateElement(element)))))))
    
    def from (doc: Document): BinaryOutputOps =
      from(DocumentTree(Path.Root, Seq(doc)))
    
    def from (tree: DocumentTree): BinaryOutputOps = new BinaryOutputOps {
      def toBinaryOutput (out: Output with Binary) =
        processor.process(tree, render, theme.defaultTemplateOrFallback, out.asBinaryOutput)
    }
    
  }
  

  /** Returns a new Render instance for the specified render format.
   *  This factory is usually an object provided by the library
   *  or a plugin that is capable of rendering a specific output. 
   * 
   *  @param format the renderer factory responsible for creating the final renderer
   */
  def as [Writer] (format: RenderFormat[Writer]): RenderMappedOutput[Writer] =
    new RenderMappedOutput(format, OperationConfig.default)
  
  /** Returns a new Render instance for the specified processor.
   *  This instance is usually an object provided by the library
   *  or a plugin that is capable of rendering a specific output. 
   * 
   *  @param processor the processor responsible for processing the renderer result
   */
  def as [Writer] (processor: RenderResultProcessor[Writer]): RenderGatheredOutput[Writer] =
    new RenderGatheredOutput(processor, OperationConfig.default)
  
}
