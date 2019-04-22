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

import laika.api.Render.Done
import laika.ast.Path.Root
import laika.ast._
import laika.config.{OperationConfig, TransformConfigBuilder}
import laika.execute.TransformExecutor
import laika.factory.{MarkupParser, RenderFormat, RenderResultProcessor}
import laika.io.Output.Binary
import laika.io._

/** API for performing a transformation operation from and to various types of input and output,
 *  combining a parse and render operation. 
 *  
 *  In cases where a parse or render operation should
 *  be performed separately, for example for manually processing the document tree model
 *  between these operations, the [[laika.api.Parse]] and [[laika.api.Render]] APIs 
 *  should be used instead.
 *  
 *  Example for transforming from Markdown to HTML using files for both input and output:
 *  
 *  {{{
 *  Transform from Markdown to HTML fromFile "hello.md" toFile "hello.html"
 *  }}}
 *  
 *  Example for transforming an entire directory and its subdirectories to HTML in a target directory:
 *  
 *  {{{
 *  Transform from Markdown to HTML fromDirectory "source" toDirectory "target"
 *  }}}
 *  
 *  Example for transforming an entire directory and its subdirectories to a single PDF file:
 *  
 *  {{{
 *  Transform from Markdown to PDF fromDirectory "source" toFile "hello.pdf"
 *  }}}
 *  
 *  Or for transforming a document fragment from a string to the AST format
 *  for debugging purposes:
 *  
 *  {{{
 *  val input = "some *emphasized* text"
 *  
 *  Transform from Markdown to AST fromString input toString
 *  
 *  res0: java.lang.String = 
 *  Document - Blocks: 1
 *  . Paragraph - Spans: 3
 *  . . Text - 'some ' 
 *  . . Emphasized - Spans: 1
 *  . . . Text - 'emphasized'
 *  . . Text - ' text'
 *  }}}
 *  
 *  Apart from specifying input and output, the Transform API also allows to customize the operation
 *  in various ways. The `usingRule` and `creatingRule` methods allow to rewrite the document tree
 *  between the parse and render operations and the `rendering` method allows to customize the
 *  way certain types of elements are rendered.
 *  
 *  @tparam Writer the writer API to use which varies depending on the renderer
 * 
 *  @author Jens Halm
 */
abstract class Transform [Writer] private[Transform] (parsers: Seq[MarkupParser], protected val format: RenderFormat[Writer]) extends
  TransformConfigBuilder[Writer] with InputOps with InputTreeOps  {
  
  type ThisType <: Transform[Writer]
  type InputResult <: OutputOps
  type InputTreeResult <: OutputOps

}

/** Serves as an entry point to the Transform API.
 * 
 *  @author Jens Halm
 */
object Transform {

  case class Op[Writer] (parsers: Seq[MarkupParser], format: RenderFormat[Writer], config: OperationConfig, input: Input, output: Output) {
    def execute: Done = TransformExecutor.execute(this)
  }

  case class TreeOp[Writer] (parsers: Seq[MarkupParser], format: RenderFormat[Writer], config: OperationConfig, input: InputTree, output: OutputTree) {
    def execute: Done = TransformExecutor.execute(this)
  }

  case class MergeOp[Writer] (parsers: Seq[MarkupParser], processor: RenderResultProcessor[Writer], config: OperationConfig, input: InputTree, output: Output with Binary) {
    def execute: Done = TransformExecutor.execute(this)
  }
  
  /** A transform operation that maps each input document of a
   *  given input tree to a corresponding output document
   *  in the destination tree.
   *  
   *  @param parse the parser to use for parsing the input
   *  @param render the renderer to use for producing the output
   */
  class TransformMappedOutput[Writer] (parsers: Seq[MarkupParser], format: RenderFormat[Writer],
                                       val config: OperationConfig) extends Transform[Writer](parsers, format) {
    
    type InputResult = TextTransformOutputOps[Writer]
  
    type InputTreeResult = TransformOutputTreeOps[Writer]
  
    type ThisType = TransformMappedOutput[Writer]
    
    def withConfig (newConfig: OperationConfig): ThisType = new TransformMappedOutput(parsers, format, newConfig)

    def fromInput (input: Input): InputResult = new TextTransformOutputOps[Writer] {
      def toOutput (out: Output) = Op[Writer](parsers, format, config, input, out)
    }

    def fromInputTree (inputTree: InputTree): InputTreeResult = new TransformOutputTreeOps[Writer] {
      def toOutputTree (tree: OutputTree) = TreeOp[Writer](parsers, format, config, inputTree, tree)
    }
    
  }
  
  /** A transform operation that merges input from one or more
   *  input documents in an input tree structure to be rendered 
   *  to a single output destination.
   *  
   *  @param parse the parser to use for parsing the input
   *  @param render the renderer to use for producing the output
   */
  class TransformMergedOutput[Writer] (parsers: Seq[MarkupParser], processor: RenderResultProcessor[Writer],
                                       val config: OperationConfig) extends Transform[Writer](parsers, processor.format) {
    
    type InputResult = BinaryTransformOutputOps[Writer]
  
    type InputTreeResult = BinaryTransformOutputOps[Writer]
  
    type ThisType = TransformMergedOutput[Writer]
    
    def withConfig (newConfig: OperationConfig): ThisType = new TransformMergedOutput(parsers, processor, newConfig)

    def fromInput (input: Input): InputResult = {
      // TODO - 0.12 - temporary workaround until IO gets redesigned
      val inputTree = new InputTree {
        override def path: Path = Root
        override def configDocuments: Seq[Input] = Nil
        override def markupDocuments: Seq[Input] = Seq(input)
        override def dynamicDocuments: Seq[Input] = Nil
        override def styleSheets: Map[String, Seq[Input]] = Map.empty
        override def staticDocuments: Seq[Input] = Nil
        override def templates: Seq[Input] = Nil
        override def subtrees: Seq[InputTree] = Nil
        override def sourcePaths: Seq[String] = Nil
      }
      fromInputTree(inputTree)
    }

    def fromInputTree (inputTree: InputTree): InputTreeResult = new BinaryTransformOutputOps[Writer] {
      def toBinaryOutput (out: Output with Binary): Result = MergeOp[Writer](parsers, processor, config, inputTree, out)
    }

  }

  /** Step in the setup for a transform operation where the
   *  renderer must be specified.
   */
  class Builder private[Transform] (factories: Seq[MarkupParser], config: OperationConfig) {

    /** Returns a new Builder instance adding the specified parser factory.
     *  This factory is usually an object provided by the library
     *  or a plugin that is capable of parsing a specific markup
     *  format like Markdown or reStructuredText.
     *  
     *  This method is useful if you want to combine different markup
     *  formats within a single document tree. 
     * 
     *  @param parser the parser factory to add to the previously specified parsers
     *  @return a new Builder instance
     */
    def or (parser: MarkupParser): Builder = new Builder(factories :+ parser, config.withBundlesFor(parser))
    
    /** Creates and returns a new Transform instance for the specified renderer and the
     *  previously specified parser. The returned instance is stateless and reusable for
     *  multiple transformations.
     * 
     *  @param format the render format to use for the transformation
     *  @return a new Transform instance
     */
    def to [Writer] (format: RenderFormat[Writer]): TransformMappedOutput[Writer] =
      new TransformMappedOutput(factories, format, config)
    
    /** Creates and returns a new Transform instance for the specified renderer and the
     *  previously specified parser. The returned instance is stateless and reusable for
     *  multiple transformations.
     * 
     *  @param processor the processor to use for the transformation
     *  @return a new Transform instance
     */
    def to [Writer] (processor: RenderResultProcessor[Writer]): TransformMergedOutput[Writer] = 
      new TransformMergedOutput(factories, processor, config)
    
  }
  
  /** Returns a new Builder instance for the specified parser factory.
   *  This factory is usually an object provided by the library
   *  or a plugin that is capable of parsing a specific markup
   *  format like Markdown or reStructuredText. The returned builder
   *  can then be used to specifiy the renderer to create the actual
   *  Transform instance.
   * 
   *  @param parser the parser factory to use
   *  @return a new Builder instance for specifying the renderer
   */
  def from (parser: MarkupParser): Builder = new Builder(Seq(parser), OperationConfig.default.withBundlesFor(parser))
  
  
}
