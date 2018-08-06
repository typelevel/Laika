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

import laika.api.Render.{RenderMappedOutput, RenderGatheredOutput}
import laika.ast._
import laika.config.{OperationConfig, TransformConfigBuilder}
import laika.factory.{MarkupParser, RenderFormat, RenderResultProcessor}
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
abstract class Transform [Writer] private[Transform] (parse: Parse, protected val format: RenderFormat[Writer]) extends
  TransformConfigBuilder[Writer] with InputOps with InputTreeOps  {
  
  type ThisType <: Transform[Writer]
  type InputResult <: OutputOps
  type InputTreeResult <: OutputOps

  def fromInput (input: Input): InputResult = fromDocument(parse.fromInput(input))

  def fromInputTree (inputTree: InputTree): InputTreeResult = fromTree(parse.fromInputTree(inputTree))

  /**  Renders the specified document tree and returns a new target instance
    *  which allows to specify the output.
    *
    *  @param tree the document tree to transform
    */
  protected[this] def fromTree (tree: DocumentTree): InputTreeResult
  
  /** Renders the specified document and returns a new target instance 
   *  which allows to specify the output.
   * 
   *  @param doc the document to transform
   */
  protected[this] def fromDocument (doc: Document): InputResult
  
}

/** Serves as an entry point to the Transform API.
 * 
 *  @author Jens Halm
 */
object Transform {
   
  
  /** A transform operation that maps each input document of a
   *  given input tree to a corresponding output document
   *  in the destination tree.
   *  
   *  @param parse the parser to use for parsing the input
   *  @param render the renderer to use for producing the output
   */
  class TransformMappedOutput[Writer] (parse: Parse, render: RenderMappedOutput[Writer],
                                       val config: OperationConfig) extends Transform[Writer](parse, render.format) {
    
    type InputResult = TextOuputOps
  
    type InputTreeResult = OutputTreeOps
  
    type ThisType = TransformMappedOutput[Writer]
    
    def withConfig (newConfig: OperationConfig): ThisType =
      new TransformMappedOutput(parse.withConfig(newConfig), render.withConfig(newConfig), newConfig)
    
    def fromDocument (doc: Document): TextOuputOps = render.from(doc)
    
    def fromTree (tree: DocumentTree): OutputTreeOps = render.from(tree)

  }
  
  /** A transform operation that gathers input from one or more
   *  input documents in an input tree structure to be rendered 
   *  to a single output destination.
   *  
   *  @param parse the parser to use for parsing the input
   *  @param render the renderer to use for producing the output
   */
  class TransformGatheredOutput[Writer] (parse: Parse, render: RenderGatheredOutput[Writer],
                                         val config: OperationConfig) extends Transform[Writer](parse, render.format) {
    
    type InputResult = BinaryOutputOps
  
    type InputTreeResult = BinaryOutputOps
  
    type ThisType = TransformGatheredOutput[Writer]
    
    def withConfig (newConfig: OperationConfig): ThisType =
      new TransformGatheredOutput(parse.withConfig(newConfig), render.withConfig(newConfig), newConfig)

    def fromDocument (doc: Document): BinaryOutputOps = render.from(doc)
    
    def fromTree (tree: DocumentTree): BinaryOutputOps = render.from(tree)

  }

  /** Step in the setup for a transform operation where the
   *  renderer must be specified.
   */
  class Builder private[Transform] (factories: Seq[MarkupParser]) {

    private lazy val parse = factories.tail.foldLeft(Parse.as(factories.head))(_ or _)

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
    def or (parser: MarkupParser): Builder = new Builder(factories :+ parser)
    
    /** Creates and returns a new Transform instance for the specified renderer and the
     *  previously specified parser. The returned instance is stateless and reusable for
     *  multiple transformations.
     * 
     *  @param format the render format to use for the transformation
     *  @return a new Transform instance
     */
    def to [Writer] (format: RenderFormat[Writer]): TransformMappedOutput[Writer] =
      new TransformMappedOutput(parse, Render as format withConfig parse.config, parse.config)
    
    /** Creates and returns a new Transform instance for the specified renderer and the
     *  previously specified parser. The returned instance is stateless and reusable for
     *  multiple transformations.
     * 
     *  @param processor the processor to use for the transformation
     *  @return a new Transform instance
     */
    def to [Writer] (processor: RenderResultProcessor[Writer]): TransformGatheredOutput[Writer] = 
      new TransformGatheredOutput(parse, Render as processor withConfig parse.config, parse.config)
    
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
  def from (parser: MarkupParser): Builder = new Builder(Seq(parser))
  
  
}
