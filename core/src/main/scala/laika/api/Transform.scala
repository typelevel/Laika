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
import java.io.InputStream
import java.io.Reader
import scala.io.Codec
import laika.io._
import laika.tree.Documents._
import laika.tree.Elements._
import laika.tree.RewriteRules
import laika.factory.ParserFactory
import laika.factory.RendererFactory
import laika.io.InputProvider._
import laika.io.OutputProvider._
import laika.io.Output.Binary
import laika.template.ParseTemplate
import laika.directive.Directives.Templates
import laika.template.DefaultTemplate
import laika.api.Parse.Parsers
import laika.api.Parse.Parser
import Transform._
import laika.factory.RenderResultProcessor
import laika.parse.css.ParseStyleSheet
  
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
 *  Example for transforming an entire directory and its subdirectories:
 *  
 *  {{{
 *  Transform from Markdown to HTML fromDirectory "source" toDirectory "target"
 *  }}}
 *  
 *  Or for transforming a document fragment from a string to the PrettyPrint format
 *  for debugging purposes:
 *  
 *  {{{
 *  val input = "some *emphasized* text"
 *  
 *  Transform from Markdown to PrettyPrint fromString input toString
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
 *  @tparam W the writer API to use which varies depending on the renderer
 * 
 *  @author Jens Halm
 */
class Transform [Writer, DocTarget, TreeTarget] private[Transform] (parse: Parse, 
    operation: Operation[Writer, DocTarget, TreeTarget], rules: Rules) {
  
  /** Specifies a rewrite rule to be applied to the document tree model between the
   *  parse and render operations. This is identical to calling `Document.rewrite`
   *  directly, but if there is no need to otherwise access the document instance
   *  and just chain parse and render operations this hook is more convenient.
   *  
   *  The rule is a partial function that takes an `Element` and returns an `Option[Element]`.
   *  
   *  If the function is not defined for a specific element the old element remains
   *  in the tree unchanged. If it returns `None` then the node gets removed from the tree, 
   *  if it returns an element it will replace the old one. Of course the function may
   *  also return the old element.
   *  
   *  The rewriting is performed in a way that only branches of the tree that contain
   *  new or removed elements will be replaced. It is processed bottom-up, therefore
   *  any element container passed to the rule only contains children which have already
   *  been processed. 
   *  
   *  In case multiple rewrite rules need to be applied it may be more efficient to
   *  first combine them with `orElse`.
   */
  def usingRule (newRule: RewriteRule) = creatingRule(_ => newRule)
  
  /** Specifies a rewrite rule to be applied to the document tree model between the
   *  parse and render operations. This is identical to calling `Document.rewrite`
   *  directly, but if there is no need to otherwise access the document instance
   *  and just chain parse and render operations this hook is more convenient.
   *  
   *  The difference of this method to the `usingRule` method is that it expects a function
   *  that expects a Document instance and returns the rewrite rule. This way the full document
   *  can be queried before any rule is applied. This is necessary in cases where the rule
   *  (which gets applied node-by-node) depends on information from other nodes. An example
   *  from the built-in rewrite rules is the rule that resolves link references. To replace
   *  all link reference elements with actual link elements, the rewrite rule needs to know
   *  all LinkDefinitions the document tree contains.
   *  
   *  The rule itself is a partial function that takes an `Element` and returns an `Option[Element]`.
   *  
   *  If the function is not defined for a specific element the old element remains
   *  in the tree unchanged. If it returns `None` then the node gets removed from the tree, 
   *  if it returns an element it will replace the old one. Of course the function may
   *  also return the old element.
   *  
   *  The rewriting is performed in a way that only branches of the tree that contain
   *  new or removed elements will be replaced. It is processed bottom-up, therefore
   *  any element container passed to the rule only contains children which have already
   *  been processed. 
   *  
   *  In case multiple rewrite rules need to be applied it may be more efficient to
   *  first combine them with `orElse`.
   */
  def creatingRule (newRule: DocumentContext => RewriteRule) 
      = new Transform(parse, operation, rules + newRule) 
  
  /** Specifies a custom render function that overrides one or more of the default
   *  renderers for the output format this instance uses.
   *  
   *  This method expects a function that returns a partial function as the parameter.
   *  The outer function allows to capture the writer instance to write to and will
   *  only be invoked once. The partial function will then be invoked for each
   *  elememnt it is defined at. 
   * 
   *  Simple example for customizing the HTML output for emphasized text, adding a specific
   *  style class:
   *  
   *  {{{
   *  Transform from Markdown to HTML rendering { out => 
   *    { case Emphasized(content) => out << """&lt;em class="big">""" << content << "&lt;/em>" } 
   *  } fromFile "hello.md" toFile "hello.html"
   *  }}}
   */
  def rendering (customRenderer: Writer => RenderFunction) 
      = new Transform(parse, operation withRenderer customRenderer, rules)
  
  
  /** Parses the specified string and returns a new Operation instance which allows to specify the output.
   *  Any kind of input is valid, including an empty string. 
   */
  def fromString (str: String): DocTarget = operation.fromDocument(parse.fromString(str), rules)
  
  /** Parses the input from the specified reader
   *  and returns a new Operation instance which allows to specify the output.
   */
  def fromReader (reader: Reader): DocTarget = operation.fromDocument(parse.fromReader(reader), rules)
  
  /** Parses the file with the specified name
   *  and returns a new Operation instance which allows to specify the output.
   *  Any kind of character input is valid, including empty files.
   * 
   *  @param name the name of the file to parse
   *  @param codec the character encoding of the file, if not specified the platform default will be used.
   */
  def fromFile (name: String)(implicit codec: Codec): DocTarget = operation.fromDocument(parse.fromFile(name)(codec), rules)
  
  /** Parses the specified file
   *  and returns a new Operation instance which allows to specify the output.
   *  Any kind of character input is valid, including empty files.
   * 
   *  @param file the file to read from
   *  @param codec the character encoding of the file, if not specified the platform default will be used.
   */
  def fromFile (file: File)(implicit codec: Codec): DocTarget = operation.fromDocument(parse.fromFile(file)(codec), rules)
  
  /** Parses the input from the specified stream
   *  and returns a new Operation instance which allows to specify the output.
   * 
   *  @param stream the stream to use as input for the parser
   *  @param codec the character encoding of the stream, if not specified the platform default will be used.
   */
  def fromStream (stream: InputStream)(implicit codec: Codec): DocTarget = operation.fromDocument(parse.fromStream(stream)(codec), rules)
  
  /** Parses files from the specified directory and its subdirectories
   *  and returns a new builder instance which allows to specify the output and 
   *  other configuration options.
   * 
   *  @param name the name of the directory to traverse
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   *  @param return a builder which allows to specify the output and other configuration options
   */
  def fromDirectory (name: String)(implicit codec: Codec): TreeTarget = fromDirectory(name, hiddenFileFilter)(codec)
  
  /** Parses files from the specified directory and its subdirectories
   *  and returns a new builder instance which allows to specify the output and 
   *  other configuration options.
   * 
   *  @param name the name of the directory to traverse
   *  @param exclude the files to exclude from processing
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   *  @param return a builder which allows to specify the output and other configuration options
   */
  def fromDirectory (name: String, exclude: FileFilter)(implicit codec: Codec): TreeTarget = fromTree(InputProvider.Directory(name, exclude)(codec))

  /** Parses files from the specified directory and its subdirectories
   *  and returns a new builder instance which allows to specify the output and 
   *  other configuration options.
   * 
   *  @param dir the directory to traverse
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   *  @param return a builder which allows to specify the output and other configuration options
   */
  def fromDirectory (dir: File)(implicit codec: Codec): TreeTarget = fromDirectory(dir, hiddenFileFilter)(codec)
  
  /** Parses files from the specified directory and its subdirectories
   *  and returns a new builder instance which allows to specify the output and 
   *  other configuration options.
   * 
   *  @param dir the directory to traverse
   *  @param exclude the files to exclude from processing
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   *  @param return a builder which allows to specify the output and other configuration options
   */
  def fromDirectory (dir: File, exclude: FileFilter)(implicit codec: Codec): TreeTarget = fromTree(InputProvider.Directory(dir, exclude)(codec))
  
  /** Parses files from the specified directories and its subdirectories, 
   *  merging them into a tree with a single root
   *  and returns a new builder instance which allows to specify the output and 
   *  other configuration options.
   * 
   *  @param roots the root directories to traverse
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   */
  def fromDirectories (roots: Seq[File])(implicit codec: Codec): TreeTarget = fromDirectories(roots, hiddenFileFilter)(codec)
  
  /** Parses files from the specified directories and its subdirectories, 
   *  merging them into a tree with a single root
   *  and returns a new builder instance which allows to specify the output and 
   *  other configuration options.
   * 
   *  @param roots the root directories to traverse
   *  @param exclude the files to exclude from processing
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   */
  def fromDirectories (roots: Seq[File], exclude: FileFilter)(implicit codec: Codec): TreeTarget = fromTree(InputProvider.Directories(roots, exclude)(codec))
  
  /** Parses from the specified input
   *  and returns a new builder instance which allows to specify the output and 
   *  other configuration options.
   * 
   *  @param inputBuilder the input to transform
   */
  def fromTree (inputBuilder: InputConfigBuilder): TreeTarget = operation.fromTree(inputBuilder, parse, rules)
  

} 

/** Serves as an entry point to the Transform API.
 * 
 *  @author Jens Halm
 */
object Transform {
   
  trait Operation[Writer, DocTarget, TreeTarget] {
    
    def fromDocument (doc: Document, rules: Rules): DocTarget
    
    def fromTree (input: InputConfigBuilder, parse: Parse, rules: Rules): TreeTarget
    
    def withRenderer (customRenderer: Writer => RenderFunction): Operation[Writer, DocTarget, TreeTarget] 
    
    protected def rewrite (doc: Document, rules: Rules) = doc rewriteWith rules.forContext(DocumentContext(doc))

    protected def rewrite (tree: DocumentTree, rules: Rules) = tree.rewrite(rules.all, AutonumberContext.defaults)
    
  }
  
  class MapOperation[Writer] (render: Render[Writer, Render.SingleTarget, Render.TreeTarget]) 
      extends Operation[Writer, Render.SingleTarget, TreeTarget] {
    
    def fromDocument (doc: Document, rules: Rules): Render.SingleTarget = new Render.SingleTarget {
      protected def renderTo (out: Output) = render from rewrite(doc,rules) toOutput out
    }
    
    def fromTree (input: InputConfigBuilder, parse: Parse, rules: Rules) = new TreeTarget(transform(parse,rules), input)
    
    protected def transform (parse: Parse, rules: Rules)(input: InputConfigBuilder, output: OutputConfigBuilder) = {
      val tree = parse.fromTree(input)
      render from rewrite(tree,rules) toTree output
    }
    
    def withRenderer (customRenderer: Writer => RenderFunction) = new MapOperation(render using customRenderer)
    
  }
  
  class GatherOperation[Writer] (render: Render[Writer, Render.BinaryTarget, Render.BinaryTarget]) 
      extends Operation[Writer, Render.BinaryTarget, GatherTarget] {
    
    def fromDocument (doc: Document, rules: Rules): Render.BinaryTarget = new Render.BinaryTarget {
      protected def renderBinary (out: Output with Binary) = render from rewrite(doc,rules) toBinaryOutput out
    }
    
    def fromTree (input: InputConfigBuilder, parse: Parse, rules: Rules) = new GatherTarget(transform(parse,rules), input)
    
    protected def transform (parse: Parse, rules: Rules)(input: InputConfigBuilder, output: Output with Binary) = {
      val tree = parse.fromTree(input)
      render from rewrite(tree,rules) toBinaryOutput output
    }
    
    def withRenderer (customRenderer: Writer => RenderFunction) = new GatherOperation(render using customRenderer)
    
  }
  
  private[laika] class Rules (rules: List[DocumentContext => RewriteRule]){
    
    def all = rules.reverse
    
    def forContext (context: DocumentContext) = (rules map { _(context) }).reverse      
    
    def + (newRule: DocumentContext => RewriteRule) = new Rules(newRule :: rules)
    
  }
  
  /** API for configuring a batch operation for an input tree.
   *  Gives access to all relevant aspects of traversing, parsing and rendering
   *  a tree of inputs.
   */
  trait TreeConfigBuilder[ThisType] {
    
    protected def withInputBuilder (f: InputConfigBuilder => InputConfigBuilder): ThisType
    
    protected def withParallelExecution: ThisType
    
    /** Specifies the style sheet engine to use for 
     *  parsing all CSS inputs found in the tree.
     */
    def withStyleSheets (parser: ParseStyleSheet) = withInputBuilder(_.withStyleSheets(parser))
    
    /** Specifies the template engine to use for 
     *  parsing all template inputs found in the tree.
     */
    def withTemplates (parse: ParseTemplate) = withInputBuilder(_.withTemplates(parse))
    
    /** Specifies custom template directives to use with
     *  the default template engine.
     */
    def withTemplateDirectives (directives: Templates.Directive*) = withInputBuilder(_.withTemplateDirectives(directives:_*))
    
    /** Specifies the function to use for determining the document type
     *  of the input based on its path.
     */
    def withDocTypeMatcher (matcher: Path => DocumentType) = withInputBuilder(_.withDocTypeMatcher(matcher))

    /** Specifies a root configuration file that gets
     *  inherited by this tree and its subtrees.
     *  The syntax of the input is expected to be of a format
     *  compatible with the Typesafe Config library.
     */
    def withConfigFile (file: File) = withInputBuilder(_.withConfigFile(file))
    
    /** Specifies the name of a root configuration file that gets
     *  inherited by this tree and its subtrees.
     *  The syntax of the input is expected to be of a format
     *  compatible with the Typesafe Config library.
     */
    def withConfigFile (name: String) = withInputBuilder(_.withConfigFile(name))
    
    /** Specifies a root configuration source that gets
     *  inherited by this tree and its subtrees.
     *  The syntax of the input is expected to be of a format
     *  compatible with the Typesafe Config library.
     */
    def withConfigString (source: String) = withInputBuilder(_.withConfigString(source))
    
    /** Instructs both the parser and renderer to process all inputs and outputs in parallel.
     *  The recursive structure of document trees will be flattened before parsing and rendering
     *  and then get reassembled afterwards, therefore the parallel processing
     *  includes all subtrees of the document tree.
     *  
     *  The actual transformation is a three phase process, the first (parsing) and
     *  third (rendering) can run in parallel. The second phase in the middle cannot,
     *  as this is the document tree model rewrite step where things like cross references or
     *  table of contents get processed that need access to more than just the current
     *  document. 
     */
    def inParallel = withParallelExecution
    
  }
  
  class TreeTarget (transform: (InputConfigBuilder, OutputConfigBuilder) => Unit,
                    inputBuilder: InputConfigBuilder, 
                    isParallel:Boolean = false) extends TreeConfigBuilder[TreeTarget] 
                                                   with Render.TreeTarget { 
    
    protected def withInputBuilder (f: InputConfigBuilder => InputConfigBuilder) 
      = new TreeTarget(transform, f(inputBuilder), isParallel)
    
    protected def withParallelExecution = new TreeTarget(transform, inputBuilder.inParallel, true)
    
    protected def renderTo (out: OutputConfigBuilder) =
      transform(inputBuilder, if (isParallel) out.inParallel else out)
    
  }
  
  class GatherTarget (transform: (InputConfigBuilder, Output with Binary) => Unit,
                     inputBuilder: InputConfigBuilder, 
                     isParallel:Boolean = false) extends TreeConfigBuilder[GatherTarget] 
                                                    with Render.BinaryTarget { 
    
    protected def withInputBuilder (f: InputConfigBuilder => InputConfigBuilder) 
      = new GatherTarget(transform, f(inputBuilder), isParallel)
    
    protected def withParallelExecution = new GatherTarget(transform, inputBuilder.inParallel, true)
    
    protected def renderBinary (out: Output with Binary) =
      transform(inputBuilder, out) // TODO - parallel output?
    
  }

  /** Step in the setup for a transform operation where the
   *  renderer must be specified.
   */
  class Builder private[Transform] (parse: Parse) {

    /** Returns a new Builder instance adding the specified parser factory.
     *  This factory is usually an object provided by the library
     *  or a plugin that is capable of parsing a specific markup
     *  format like Markdown or reStructuredText.
     *  
     *  This method is useful if you want to combine different markup
     *  formats within a single document tree. 
     * 
     *  @param factory the parser factory to add to the previously specified parsers
     *  @return a new Builder instance
     */
    def or (factory: ParserFactory) = new Builder(parse or factory)
    
    /** Creates and returns a new Transform instance for the specified renderer and the
     *  previously specified parser. The returned instance is stateless and reusable for
     *  multiple transformations.
     * 
     *  @param factory the renderer factory to use for the transformation
     *  @return a new Transform instance
     */
    def to [W] (factory: RendererFactory[W]): Transform[W, Render.SingleTarget, TreeTarget] = 
      new Transform(parse.withoutRewrite, new MapOperation(Render as factory), new Rules(Nil)) 
    
    /** Creates and returns a new Transform instance for the specified renderer and the
     *  previously specified parser. The returned instance is stateless and reusable for
     *  multiple transformations.
     * 
     *  @param factory the renderer factory to use for the transformation
     *  @return a new Transform instance
     */
    def to [Writer] (processor: RenderResultProcessor[Writer]): Transform[Writer, Render.BinaryTarget, GatherTarget] = 
      new Transform(parse.withoutRewrite, new GatherOperation(Render as processor), new Rules(Nil)) 
    
  }
  
  /** Returns a new Builder instance for the specified parser factory.
   *  This factory is usually an object provided by the library
   *  or a plugin that is capable of parsing a specific markup
   *  format like Markdown or reStructuredText. The returned builder
   *  can then be used to specifiy the renderer to create the actual
   *  Transform instance.
   * 
   *  @param factory the parser factory to use
   *  @return a new Builder instance for specifying the renderer
   */
  def from (factory: ParserFactory): Builder = new Builder(Parse as factory)
  
  
}
