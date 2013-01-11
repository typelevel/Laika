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
  
/** API for performing a render operation to various types of output using an existing
 *  document tree model. 
 *  
 *  In cases where a render operation follows a parse operation 
 *  immediately, it is more convenient to use the [[transformer.Transform]] API 
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
class Render[W] private (setup: (Output, Element => Unit) => (W, Element => Unit),
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
    def toFile (name: String)(implicit codec: Codec) = IO(Output.toFile(name)(codec))(render)
    
    /** Renders the tree model to the specified file.
     * 
     *  @param file the file to write to
     *  @param codec the character encoding of the file, if not specified the platform default will be used.
     */
    def toFile (file: File)(implicit codec: Codec) = IO(Output.toFile(file)(codec))(render)
    
    /** Renders the tree model to the specified output stream.
     * 
     *  @param stream the stream to render to
     *  @param codec the character encoding of the stream, if not specified the platform default will be used.
     */
     def toStream (stream: OutputStream)(implicit codec: Codec) = IO(Output.toStream(stream)(codec))(render)

     /** Renders the tree model to the console.
      */
     def toConsole = toStream(System.out)

     /** Renders the tree model to the specified writer.
      */
    def toWriter (writer: Writer) = IO(Output.toWriter(writer))(render)

    /** Renders the tree model to the specified `StringBuilder`.
     */
    def toBuilder (builder: StringBuilder) = IO(Output.toBuilder(builder))(render)

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
    
    private def render (out: Output) = { 
      
      val (writer, render) = setup(out, RenderFunction)
      
      RenderFunction.delegate = customRenderers match {
        case Nil => render
        case xs  => {
          val default:PartialFunction[Element, Unit] = { case e => render(e) }
          (xs map { _(writer) }).reverse reduceRight { _ orElse _ } orElse default
        }
      }
      
      RenderFunction(elem)
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
    new Render(setup, render :: customRenderers)
  }
  
  
  /** Specifies the element to render. This may be a root `Document` instance
   *  as well as any other type of `Element`, thus allowing to render document
   *  fragments, too.
   * 
   *  @param elem the element to render
   *  @return a new Operation instance that allows to specify the output
   */
  def from (elem: Element) = new Operation(elem)
  

}

/** Serves as an entry point to the Render API.
 * 
 *  @author Jens Halm
 */
object Render {
  
  /** Returns a new Render instance for the specified render setup function.
   *  This function is usually an object provided by the library
   *  or a plugin that is capable of rendering a specific output
   *  format like HTML or PrettyPrint for debugging. 
   * 
   *  @param render the render setup function responsible for creating the final renderer
   */
  def as [W] (render: (Output, Element => Unit) => (W, Element => Unit)): Render[W] = new Render(render) 
  
}