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
import laika.execute.RenderExecutor
import laika.factory.{RenderFormat, RenderFormat2, RenderResultProcessor, RenderResultProcessor2}
import laika.io._

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
 *  @tparam FMT the formatter API to use which varies depending on the renderer
 * 
 *  @author Jens Halm
 */
abstract class Render[FMT] private (protected[api] val format: RenderFormat2[FMT],
                                       val config: OperationConfig) extends RenderConfigBuilder[FMT] {

  protected[this] lazy val theme = config.themeFor(format)


  /** The output operations that can be performed for a single input document.
   */
  type DocOps

  /** The output operations that can be performed for an entire tree of input documents.
   */
  type TreeOps

  /** The concrete implementation of the abstract Render type.
   */
  type ThisType <: Render[FMT]

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

}

/** Serves as an entry point to the Render API.
 * 
 *  @author Jens Halm
 */
object Render {

  sealed trait Done
  case object Done extends Done
  
  case class Op[Writer] (format: RenderFormat[Writer], config: OperationConfig, element: Element, output: TextOutput) {
    def execute: Done = RenderExecutor.execute(this)
  }

  case class Op2[FMT] (format: RenderFormat2[FMT], config: OperationConfig, element: Element, output: TextOutput) {
    def execute: String = RenderExecutor.execute(this, None)
  }

  case class TreeOp[Writer] (format: RenderFormat[Writer], config: OperationConfig, tree: DocumentTree, output: OutputTree) {
    def execute: Done = RenderExecutor.execute(this)
  }

  case class TreeOp2[FMT] (format: RenderFormat2[FMT], config: OperationConfig, tree: DocumentTree, output: TreeOutput) {
    def execute: RenderResult2 = RenderExecutor.execute(this)
  }

  case class MergeOp[Writer] (processor: RenderResultProcessor[Writer], config: OperationConfig, tree: DocumentTree, output: BinaryOutput) {
    def execute: Done = RenderExecutor.execute(this)
  }

  case class MergeOp2[FMT] (processor: RenderResultProcessor2[FMT], config: OperationConfig, tree: DocumentTree, output: BinaryOutput) {
    def execute: Done = RenderExecutor.execute(this)
  }
  
  /** A render operation that maps each input document of a
   *  given input tree to a corresponding output document
   *  in the destination tree.
   *  
   *  @param format the factory for the rendere to use
   *  @param cfg the configuration for the render operation
   */
  class RenderMappedOutput[FMT] (format: RenderFormat2[FMT],
                                    cfg: OperationConfig) extends Render[FMT](format, cfg) {

    type DocOps = TextRenderOutputOps[FMT]
    type TreeOps = RenderOutputTreeOps[FMT]
    type ThisType = RenderMappedOutput[FMT]

    def withConfig(newConfig: OperationConfig): ThisType =
      new RenderMappedOutput[FMT](format, newConfig)

    def from (element: Element): TextRenderOutputOps[FMT] = new TextRenderOutputOps[FMT] {
      def toTextOutput (out: TextOutput) = Op2[FMT](format, cfg, element, out)
    }
    
    def from (doc: Document): TextRenderOutputOps[FMT] = from(doc.content)
    
    def from (tree: DocumentTree): RenderOutputTreeOps[FMT] = new RenderOutputTreeOps[FMT] {
      def toOutputTree (out: TreeOutput) = TreeOp2[FMT](format, cfg, tree, out)
    }
    
  }
  
  /** A render operation that merges input from one or more
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
  class RenderMergedOutput[FMT] (processor: RenderResultProcessor2[FMT],
                                    cfg: OperationConfig) extends Render[FMT](processor.format, cfg) {
    
    type DocOps = BinaryRenderOutputOps[FMT]
    type TreeOps = BinaryRenderOutputOps[FMT]
    type ThisType = RenderMergedOutput[FMT]

    def withConfig(newConfig: OperationConfig): ThisType =
      new RenderMergedOutput[FMT](processor, newConfig)

    def from (element: Element): BinaryRenderOutputOps[FMT] =
      from(Document(Path.Root / "target", RootElement(Seq(TemplateRoot(Seq(TemplateElement(element)))))))
    
    def from (doc: Document): BinaryRenderOutputOps[FMT] =
      from(DocumentTree(Path.Root, Seq(doc)))
    
    def from (tree: DocumentTree): BinaryRenderOutputOps[FMT] = new BinaryRenderOutputOps[FMT] {
      def toBinaryOutput (out: BinaryOutput): Result = MergeOp2[FMT](processor, cfg, tree, out)
    }
    
  }
  

  /** Returns a new Render instance for the specified render format.
   *  This factory is usually an object provided by the library
   *  or a plugin that is capable of rendering a specific output. 
   * 
   *  @param format the renderer factory responsible for creating the final renderer
   */
  def as [FMT] (format: RenderFormat2[FMT]): RenderMappedOutput[FMT] =
    new RenderMappedOutput(format, OperationConfig.default)
  
  /** Returns a new Render instance for the specified processor.
   *  This instance is usually an object provided by the library
   *  or a plugin that is capable of rendering a specific output. 
   * 
   *  @param processor the processor responsible for processing the renderer result
   */
  def as [FMT] (processor: RenderResultProcessor2[FMT]): RenderMergedOutput[FMT] =
    new RenderMergedOutput(processor, OperationConfig.default)
  
}
