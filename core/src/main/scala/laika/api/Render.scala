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

import java.io.{File, OutputStream}

import laika.api.config.{OperationConfig, OperationConfigBuilder}
import laika.api.ext.{ExtensionBundle, Theme}
import laika.api.ext.ExtensionBundle.LaikaDefaults
import laika.directive.{DirectiveSupport, StandardDirectives}
import laika.factory.{RenderResultProcessor, RendererFactory}
import laika.io.Output.Binary
import laika.io.{IO, Input, Output, OutputProvider}
import laika.io.OutputProvider._
import laika.parse.css.Styles.StyleDeclarationSet
import laika.rewrite.TemplateRewriter
import laika.tree.Documents._
import laika.tree.Elements.{Element, RenderFunction, RootElement}
import laika.tree.Paths.{Current, Path, Root}
import laika.tree.Templates._

import scala.io.Codec
  
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
abstract class Render[Writer] private (private[Render] val factory: RendererFactory[Writer],
                                       protected val config: OperationConfig) extends OperationConfigBuilder {

  
  /** The type of the rendering target for a single input document.
   */
  type DocTarget

  /** The type of the rendering target for an entire tree of input documents. 
   */
  type TreeTarget

  /** The concrete implementation of the abstract Render type.
   */
  type ThisType <: Render[Writer]


  private lazy val mergedBundle: ExtensionBundle = ExtensionBundle.mergeBundles(config.bundles)

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
  def using (render: Writer => RenderFunction): ThisType = using(new ExtensionBundle {
    override def themeFor[W](rendererFactory: RendererFactory[W]): Theme[W] =
      Theme(customRenderers = Seq(render)).asInstanceOf[Theme[W]]
  })

  /** Specifies the element to render. This may be a `RootElement` instance
   *  as well as any other type of `Element`, thus allowing to render document
   *  fragments, too.
   * 
   *  @param elem the element to render
   *  @return a new Operation instance that allows to specify the output
   */
  def from (elem: Element): DocTarget
  
  /** Specifies the document to render. 
   * 
   *  @param doc the document to render
   *  @return a new Operation instance that allows to specify the output
   */
  def from (doc: Document): DocTarget
  
  /** Specifies the document tree to render. 
   * 
   *  @param tree the document tree to render
   *  @return a new BatchOperation instance that allows to specify the outputs
   */
  def from (tree: DocumentTree): TreeTarget


  protected[this] lazy val defaultStyles: StyleDeclarationSet =
    factory.defaultTheme.defaultStyles ++ mergedBundle.themeFor(factory).defaultStyles

  protected[this] lazy val defaultTemplate: TemplateRoot = {
    mergedBundle.themeFor(factory).defaultTemplate
      .orElse(factory.defaultTheme.defaultTemplate)
      .getOrElse(TemplateRoot(List(TemplateContextReference("document.content"))))
  }

  /** Renders the specified element to the given output.
   * 
   *  @param element the element to render
   *  @param output the output to render to
   *  @param styles the styles to apply to the elements during the render process
   */
  protected[this] def render (element: Element, output: Output, styles: StyleDeclarationSet): Unit = { 
    
    object RenderFunction extends (Element => Unit) {
      var delegate: Element => Unit = _
      def apply (element: Element) = delegate(element)
    }

    val customRenderers = mergedBundle.themeFor(factory).customRenderers
    
    IO(output) { out =>
      val (writer, renderF) = factory.newRenderer(out, element, RenderFunction, styles)
      
      RenderFunction.delegate = customRenderers match {
        case Nil => renderF
        case xs  =>
          val default:RenderFunction = { case e => renderF(e) }
          (xs map { _(writer) }).reverse reduceRight { _ orElse _ } orElse default
      }
      
      RenderFunction(element)
      
      out.flush()
    }
  }
  
  /** Renders the specified document tree to the given output.
   * 
   *  @param tree the element to render
   *  @param config the configuration for the output to render to
   */
  protected[this] def render (tree: DocumentTree, config: OutputConfig): Unit = {
    
    type Operation = () => Unit
    
    def renderTree (provider: OutputProvider, styles: StyleDeclarationSet, path: Path, content: RootElement): Operation = {
      val output = provider.newOutput(path.basename +"."+ factory.fileSuffix)
      () => render(content, output, styles)
    } 
      
    def copy (provider: OutputProvider)(input: Input): Operation = {
      val output = provider.newOutput(input.path.name)
      () => IO.copy(input, output)
    }
    
    def collectOperations (provider: OutputProvider, parentStyles: StyleDeclarationSet, tree: DocumentTree): Seq[Operation] = {

      def isOutputRoot (source: DocumentTree) = (source.sourcePaths.headOption, config.provider) match {
        case (Some(inPath), out: DirectoryOutputProvider) => inPath == out.directory.getAbsolutePath
        case _ => false
      }

      val styles = parentStyles ++ tree.styles(factory.fileSuffix)

      (tree.content flatMap {
        case doc: Document => Seq(renderTree(provider, styles, doc.path, doc.content))
        case tree: DocumentTree if !isOutputRoot(tree) => collectOperations(provider.newChild(tree.name), styles, tree)
        case _ => Seq()
      }) ++
      (tree.additionalContent flatMap {
        case doc: DynamicDocument => Seq(renderTree(provider, styles, doc.path, doc.content))
        case static: StaticDocument if config.copyStaticFiles => Seq(copy(provider)(static.input))
        case _ => Seq()
      })
    }
  
    val templateName = "default.template." + factory.fileSuffix
    val treeWithTpl = if (tree.selectTemplate(Current / templateName).isDefined) tree 
                      else tree.copy(templates = tree.templates :+ TemplateDocument(Root / templateName, defaultTemplate))
    val finalTree = TemplateRewriter.applyTemplates(treeWithTpl, factory.fileSuffix)
    val operations = collectOperations(config.provider, defaultStyles, finalTree)
    
    (if (config.parallel) operations.par else operations) foreach (_())
  }
    

}

/** Serves as an entry point to the Render API.
 * 
 *  @author Jens Halm
 */
object Render {
  
  
  /** A target for a render operation that accepts only binary output.
   */
  trait BinaryTarget {
    
    /** Renders the model to the file with the specified name.
     * 
     *  @param name the name of the file to parse
     *  @param codec the character encoding of the file, if not specified the platform default will be used.
     */
    def toFile (name: String)(implicit codec: Codec): Unit = renderBinary(Output.toFile(name)(codec))
    
    /** Renders the model to the specified file.
     * 
     *  @param file the file to write to
     *  @param codec the character encoding of the file, if not specified the platform default will be used.
     */
    def toFile (file: File)(implicit codec: Codec): Unit = renderBinary(Output.toFile(file)(codec))
    
    /** Renders the model to the specified output stream.
     * 
     *  @param stream the stream to render to
     *  @param codec the character encoding of the stream, if not specified the platform default will be used.
     */
    def toStream (stream: OutputStream)(implicit codec: Codec): Unit = renderBinary(Output.toStream(stream)(codec))
    
    /** Renders the model to the specified output.
     *  
     *  This is a generic method based on Laika's IO abstraction layer that concrete
     *  methods delegate to. Usually not used directly in application code, but
     *  might come in handy for very special requirements.
     */
    def toBinaryOutput (out: Output with Binary): Unit = renderBinary(out)
    
    /** Renders the model to the specified binary output.
     */
    protected def renderBinary (out: Output with Binary): Unit
      
  }
  
  /** Represents a single destination for a render operation. 
   *  Various types of output can be
   *  specified to trigger the actual rendering.
   */
  trait SingleTarget extends BinaryTarget {
    
    /** Renders the model to the console.
     */
    def toConsole: Unit = toStream(System.out)

    /** Renders the model to the specified writer.
     */
    def toWriter (writer: java.io.Writer): Unit = renderTo(Output.toWriter(writer))

    /** Renders the model to the specified `StringBuilder`.
     */
    def toBuilder (builder: StringBuilder): Unit = renderTo(Output.toBuilder(builder))
    
    /** Renders the model to a String and returns it.
     */
    override def toString = {
      val builder = new StringBuilder
      toBuilder(builder)
      builder.toString
    } 
  
    /** Renders the model to the specified output.
     *  
     *  This is a generic method based on Laika's IO abstraction layer that concrete
     *  methods delegate to. Usually not used directly in application code, but
     *  might come in handy for very special requirements.
     */
    def toOutput (out: Output): Unit = renderTo(out)
    
    /** Renders the model to the specified binary output.
     */
    protected def renderTo (out: Output): Unit
     
    /** Renders the model to the specified binary output.
     */
    protected def renderBinary (out: Output with Binary): Unit = renderTo(out)
    
  }
  
  /** Represents a tree of output destinations for recursive render operations. 
   *  Various types of output can be specified to trigger the actual rendering.
   */
  trait MappedTreeTarget {
    
    /** Renders the document tree to the
     *  specified directory and its subdirectories.
     *  Required subdirectories which do not exist yet will be created.
     * 
     *  @param name the name of the directory to write to
     *  @param codec the character encoding of the files, if not specified the platform default will be used.
     */
    def toDirectory (name: String)(implicit codec: Codec): Unit = toTree(Directory(name)(codec))

    /** Renders the document tree to the
     *  specified directory and its subdirectories.
     *  Required subdirectories which do not exist yet will be created.
     * 
     *  @param dir the directory to write to
     *  @param codec the character encoding of the files, if not specified the platform default will be used.
     */
    def toDirectory (dir: File)(implicit codec: Codec): Unit = toTree(Directory(dir)(codec))
  
    /** Renders the document tree to the
     *  current working directory and its subdirectories.
     *  Required subdirectories which do not exist yet will be created.
     * 
     *  @param codec the character encoding of the files, if not specified the platform default will be used.
     */
    def toDefaultDirectory (implicit codec: Codec): Unit = toTree(DefaultDirectory(codec))

    /** Renders the document tree to the output
     *  obtained from the specified configuation builder.
     *  
     *  @param builder a builder for the configuration from which the output to write to can be obtained
     */
    def toTree (builder: OutputConfigBuilder): Unit = renderTo(builder)
      
    /** Renders the model to the specified output.
     */
    protected def renderTo (out: OutputConfigBuilder): Unit
    
  }
  

  /** A render operation that maps each input document of a
   *  given input tree to a corresponding output document
   *  in the destination tree.
   *  
   *  @param factory the factory for the rendere to use
   *  @param cfg the configuration for the render operation
   */
  class RenderMappedOutput[Writer] (factory: RendererFactory[Writer],
                                    cfg: OperationConfig) extends Render[Writer](factory, cfg) {
    
    type DocTarget = SingleTarget
    type TreeTarget = MappedTreeTarget
    type ThisType = RenderMappedOutput[Writer]

    protected[api] def withConfig(newConfig: OperationConfig): ThisType =
      new RenderMappedOutput[Writer](factory, newConfig)

    def from (element: Element): SingleTarget = new SingleTarget {
      protected def renderTo (out: Output) = render(element, out, defaultStyles)
    }
    
    def from (doc: Document): SingleTarget = from(doc.content)
    
    def from (tree: DocumentTree): TreeTarget = new MappedTreeTarget {
      protected def renderTo (out: OutputConfigBuilder) = render(tree, out.build)
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
                                      cfg: OperationConfig) extends Render[Writer](processor.factory, cfg) {
    
    type DocTarget = BinaryTarget
    type TreeTarget = BinaryTarget
    type ThisType = RenderGatheredOutput[Writer]

    protected[api] def withConfig(newConfig: OperationConfig): ThisType =
      new RenderGatheredOutput[Writer](processor, newConfig)

    def from (element: Element): BinaryTarget = 
      from(Document(Root / "target", RootElement(Seq(TemplateRoot(Seq(TemplateElement(element)))))))
    
    def from (doc: Document): BinaryTarget = 
      from(DocumentTree(Root, Seq(doc)))
    
    def from (tree: DocumentTree): BinaryTarget = new BinaryTarget {
      protected def renderBinary (out: Output with Binary) = processor.process(tree, render, defaultTemplate, out.asBinaryOutput)
    }
    
  }
  

  /** Returns a new Render instance for the specified renderer factory.
   *  This factory is usually an object provided by the library
   *  or a plugin that is capable of rendering a specific output. 
   * 
   *  @param factory the renderer factory responsible for creating the final renderer
   */
  def as [Writer] (factory: RendererFactory[Writer]): RenderMappedOutput[Writer] =
    new RenderMappedOutput(factory, OperationConfig(Seq(LaikaDefaults, DirectiveSupport, StandardDirectives)))
  
  /** Returns a new Render instance for the specified processor.
   *  This instance is usually an object provided by the library
   *  or a plugin that is capable of rendering a specific output. 
   * 
   *  @param processor the processor responsible for processing the renderer result
   */
  def as [Writer] (processor: RenderResultProcessor[Writer]): RenderGatheredOutput[Writer] =
    new RenderGatheredOutput(processor, OperationConfig(Seq(LaikaDefaults, DirectiveSupport, StandardDirectives)))
  
}
