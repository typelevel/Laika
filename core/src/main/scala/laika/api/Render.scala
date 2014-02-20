/*
 * Copyright 2013 the original author or authors.
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

import java.io.File
import java.io.OutputStream
import java.io.Writer
import scala.annotation.implicitNotFound
import scala.io.Codec
import laika.io.IO
import laika.io.Input
import laika.io.Output
import laika.io.OutputProvider
import laika.io.OutputProvider._
import laika.tree.Elements.Element
import laika.tree.Elements.RenderFunction
import laika.tree.Documents._
import laika.tree.Templates.TemplateDocument
import laika.factory.RendererFactory
import laika.parse.css.Styles.StyleDeclarationSet
  
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
 *  @tparam W the writer API to use which varies depending on the renderer
 * 
 *  @author Jens Halm
 */
class Render[W] private (factory: RendererFactory[W],
                         customRenderers: List[W => RenderFunction] = Nil) {

  
  /** Represents a single render operation for a specific
   *  document model. Various types of output can be
   *  specified to trigger the actual rendering.
   */
  class Operation private[Render] (elem: Element) {

    /** Renders the model to the file with the specified name.
     * 
     *  @param name the name of the file to parse
     *  @param codec the character encoding of the file, if not specified the platform default will be used.
     */
    def toFile (name: String)(implicit codec: Codec) = toOutput(Output.toFile(name)(codec))
    
    /** Renders the model to the specified file.
     * 
     *  @param file the file to write to
     *  @param codec the character encoding of the file, if not specified the platform default will be used.
     */
    def toFile (file: File)(implicit codec: Codec) = toOutput(Output.toFile(file)(codec))
    
    /** Renders the model to the specified output stream.
     * 
     *  @param stream the stream to render to
     *  @param codec the character encoding of the stream, if not specified the platform default will be used.
     */
    def toStream (stream: OutputStream)(implicit codec: Codec) = toOutput(Output.toStream(stream)(codec))

    /** Renders the model to the console.
     */
    def toConsole = toStream(System.out)

    /** Renders the model to the specified writer.
     */
    def toWriter (writer: Writer) = toOutput(Output.toWriter(writer))

    /** Renders the model to the specified `StringBuilder`.
     */
    def toBuilder (builder: StringBuilder) = toOutput(Output.toBuilder(builder))
    

    /** Renders the model to a String and returns it.
     */
    override def toString = {
      val builder = new StringBuilder
      toBuilder(builder)
      builder.toString
    } 
  
    private object RenderFunction extends (Element => Unit) {
      var delegate: Element => Unit = _ //throw new IllegalStateException("render function not initialized")
      def apply (element: Element) = delegate(element)
    }
    
    /** Renders the model to the specified output.
     *  
     *  This is a generic method based on Laika's IO abstraction layer that concrete
     *  methods delegate to. Usually not used directly in application code, but
     *  might come in handy for very special requirements.
     */
    def toOutput (out: Output): Unit = toOutput(out, factory.defaultStyles)
    
    private[Render] def toOutput (out: Output, styles: StyleDeclarationSet): Unit = { 
      IO(out) { out =>
        val (writer, render) = factory.newRenderer(out, elem, RenderFunction, styles)
        
        RenderFunction.delegate = customRenderers match {
          case Nil => render
          case xs  => {
            val default:RenderFunction = { case e => render(e) }
            (xs map { _(writer) }).reverse reduceRight { _ orElse _ } orElse default
          }
        }
        
        RenderFunction(elem)
        
        out.flush()
      }
    }
    
  }
  
  /** Represents a set of recursive render operations for a specific
   *  document tree. Various types of output can be
   *  specified to trigger the actual rendering.
   */
  class BatchOperation private[Render] (tree: DocumentTree) {
    
    /** Renders the document tree to the
     *  specified directory and its subdirectories.
     *  Required subdirectories which do not exist yet will be created.
     * 
     *  @param name the name of the directory to write to
     *  @param codec the character encoding of the files, if not specified the platform default will be used.
     */
    def toDirectory (name: String)(implicit codec: Codec) = toTree(Directory(name)(codec))

    /** Renders the document tree to the
     *  specified directory and its subdirectories.
     *  Required subdirectories which do not exist yet will be created.
     * 
     *  @param dir the directory to write to
     *  @param codec the character encoding of the files, if not specified the platform default will be used.
     */
    def toDirectory (dir: File)(implicit codec: Codec) = toTree(Directory(dir)(codec))
  
    /** Renders the document tree to the
     *  current working directory and its subdirectories.
     *  Required subdirectories which do not exist yet will be created.
     * 
     *  @param codec the character encoding of the files, if not specified the platform default will be used.
     */
    def toDefaultDirectory (implicit codec: Codec) = toTree(DefaultDirectory(codec))

    /** Renders the document tree to the output
     *  obtained from the specified configuation builder.
     *  
     *  @param builder a builder for the configuration from which the output to write to can be obtained
     */
    def toTree (builder: OutputConfigBuilder): Unit = toTree(builder.build)
      
    /** Renders the document tree to the output
     *  obtained from the specified configuation.
     *  
     *  @param config the configuration from which the output to write to can be obtained
     */
    def toTree (config: OutputConfig): Unit = {
      
      type Operation = () => Unit
      
      val styles = factory.defaultStyles ++ tree.styles(factory.fileSuffix)
      
      def render (provider: OutputProvider)(doc: Document): Operation 
        = () => from(doc.content).toOutput(provider.newOutput(doc.path.basename +"."+ factory.fileSuffix), styles)
        
      def copy (provider: OutputProvider)(input: Input): Operation 
        = () => IO.copy(input, provider.newOutput(input.path.name))
      
      def collectOperations (tree: DocumentTree, provider: OutputProvider): Seq[Operation] =
          (tree.documents map render(provider)) ++ 
          (tree.dynamicDocuments map render(provider)) ++ 
          (tree.staticDocuments map copy(provider)) ++
          (tree.subtrees map { tree => collectOperations(tree, provider.newChild(tree.name)) }).flatten
    
      val templateName = "default.template." + factory.fileSuffix
      val treeWithTpl = if (tree.selectTemplate(Current / templateName).isDefined) tree 
                        else tree.withTemplate(new TemplateDocument(Root / templateName, factory.defaultTemplate)) 
      val finalTree = treeWithTpl.applyTemplates(factory.fileSuffix)
      val operations = collectOperations(finalTree, config.provider)
      
      (if (config.parallel) operations.par else operations) foreach (_())
    }
    
  }

  /** Specifies a custom render function that overrides one or more of the default
   *  renderers for the output format this instance uses.
   *  
   *  To be precise this method expects a function that returns a partial function.
   *  The outer function allows to capture the writer instance to write to and will
   *  only be invoked once. The partial function will then be invoked for each
   *  elememnt it is defined at. 
   * 
   *  Simple example for customizing the HTML output for emphasized text, adding a specific
   *  style class:
   *  
   *  {{{
   *  val doc: Document = ...
   *  
   *  Render as HTML using { out => 
   *    { case Emphasized(content) => out << """&lt;em class="big">""" << content << "&lt;/em>" } 
   *  } from doc toString
   *  }}}
   */
  def using (render: W => RenderFunction): Render[W] = {
    new Render(factory, render :: customRenderers)
  }
  
  
  /** Specifies the element to render. This may be a `RootElement` instance
   *  as well as any other type of `Element`, thus allowing to render document
   *  fragments, too.
   * 
   *  @param elem the element to render
   *  @return a new Operation instance that allows to specify the output
   */
  def from (elem: Element) = new Operation(elem)
  
  /** Specifies the document to render. 
   * 
   *  @param doc the document to render
   *  @return a new Operation instance that allows to specify the output
   */
  def from (doc: Document) = new Operation(doc.content)
  
  /** Specifies the document tree to render. 
   * 
   *  @param tree the document tree to render
   *  @return a new BatchOperation instance that allows to specify the outputs
   */
  def from (tree: DocumentTree) = new BatchOperation(tree)
  

}

/** Serves as an entry point to the Render API.
 * 
 *  @author Jens Halm
 */
object Render {
  
  /** Returns a new Render instance for the specified renderer factory.
   *  This factory is usually an object provided by the library
   *  or a plugin that is capable of rendering a specific output
   *  format like HTML or PrettyPrint for debugging. 
   * 
   *  @param factory the renderer factory responsible for creating the final renderer
   */
  def as [W] (factory: RendererFactory[W]): Render[W] = new Render(factory) 
  
}