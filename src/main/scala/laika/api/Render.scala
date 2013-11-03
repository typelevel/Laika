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
import laika.io.Output
import laika.tree.Elements.Element
import laika.tree.Documents._
import laika.io.OutputProvider
import laika.factory.RendererFactory
import laika.io.Input
  
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
 *  @tparam W the writer API to use which varies depending on the renderer
 * 
 *  @author Jens Halm
 */
class Render[W] private (factory: RendererFactory[W],
                         customRenderers: List[W => PartialFunction[Element, Unit]] = Nil) {

  
  /** Represents a single render operation for a specific
   *  document tree model. Various types of output can be
   *  specified to trigger the actual rendering.
   */
  class Operation private[Render] (elem: Element) {

    /** Renders the tree model to the file with the specified name.
     * 
     *  @param name the name of the file to parse
     *  @param codec the character encoding of the file, if not specified the platform default will be used.
     */
    def toFile (name: String)(implicit codec: Codec) = toOutput(Output.toFile(name)(codec))
    
    /** Renders the tree model to the specified file.
     * 
     *  @param file the file to write to
     *  @param codec the character encoding of the file, if not specified the platform default will be used.
     */
    def toFile (file: File)(implicit codec: Codec) = toOutput(Output.toFile(file)(codec))
    
    /** Renders the tree model to the specified output stream.
     * 
     *  @param stream the stream to render to
     *  @param codec the character encoding of the stream, if not specified the platform default will be used.
     */
    def toStream (stream: OutputStream)(implicit codec: Codec) = toOutput(Output.toStream(stream)(codec))

    /** Renders the tree model to the console.
     */
    def toConsole = toStream(System.out)

    /** Renders the tree model to the specified writer.
     */
    def toWriter (writer: Writer) = toOutput(Output.toWriter(writer))

    /** Renders the tree model to the specified `StringBuilder`.
     */
    def toBuilder (builder: StringBuilder) = toOutput(Output.toBuilder(builder))
    

    /** Renders the tree model to a String and returns it.
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
    
    def toOutput (out: Output) = { 
      IO(out) { out =>
        val (writer, render) = factory.newRenderer(out, RenderFunction)
        
        RenderFunction.delegate = customRenderers match {
          case Nil => render
          case xs  => {
            val default:PartialFunction[Element, Unit] = { case e => render(e) }
            (xs map { _(writer) }).reverse reduceRight { _ orElse _ } orElse default
          }
        }
        
        RenderFunction(elem)
        
        out.flush()
      }
    }
    
  }
  
  class BatchOperation private[Render] (tree: DocumentTree) {
    
    def toDirectory (name: String)(implicit codec: Codec) = toTree(Directory(name)(codec))

    def toDirectory (dir: File)(implicit codec: Codec) = toTree(Directory(dir)(codec))
  
    def toDefaultDirectory (implicit codec: Codec) = toTree(DefaultDirectory(codec))
  
    case class OutputTreeConfig (provider: OutputProvider)
  
    class OutputConfigBuilder private[Render] (
        dir: File,
        codec: Codec) {
      
      def build = OutputTreeConfig(OutputProvider.forRootDirectory(dir)(codec))
    }
    
    implicit def builderToConfig (builder: OutputConfigBuilder): OutputTreeConfig = builder.build
    
    object Directory {
      def apply (name: String)(implicit codec: Codec) = new OutputConfigBuilder(new File(name), codec)
      def apply (file: File)(implicit codec: Codec) = new OutputConfigBuilder(file, codec)
    }
    
    object DefaultDirectory {
      def apply (implicit codec: Codec) = Directory(System.getProperty("user.dir"))(codec)
    }

    def toTree (provider: OutputProvider): Unit = toTree(OutputTreeConfig(provider)) // TODO - remove
    
    def toTree (config: OutputTreeConfig): Unit = {
      
      type Operation = () => Unit
      
      def render (provider: OutputProvider)(doc: Document): Operation 
        = () => from(doc.content).toOutput(provider.newOutput(doc.path.basename +"."+ factory.fileSuffix))
        
      def copy (provider: OutputProvider)(input: Input): Operation 
        = () => IO.copy(input, provider.newOutput(input.path.name)) // TODO - allow to skip unmodified files or the entire copy step (configurable)
      
      def collectOperations (tree: DocumentTree, provider: OutputProvider): Seq[Operation] =
          (tree.documents map render(provider)) ++ 
          (tree.dynamicDocuments map render(provider)) ++ 
          (tree.staticDocuments map copy(provider)) ++
          (tree.subtrees map { tree => collectOperations(tree, provider.newChild(tree.name)) }).flatten
    
      val operations = collectOperations(tree, config.provider)
      
      operations foreach (_()) // TODO - this step can optionally run in parallel
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
  def using (render: W => PartialFunction[Element, Unit]): Render[W] = {
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
  
  def from (doc: Document) = new Operation(doc.content)
  
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