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
import java.io.Writer
import scala.io.Codec
import laika.api.Transform.Rules
import laika.io._
import laika.tree.Documents._
import laika.tree.Elements._
import laika.tree.RewriteRules
import laika.factory.ParserFactory
import laika.factory.RendererFactory
import laika.io.InputProvider._
import laika.io.OutputProvider._
import laika.template.ParseTemplate
import Transform._
import laika.directive.Directives.Templates
import laika.template.DefaultTemplate
import laika.api.Parse.Parsers
import laika.api.Parse.Parser
  
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
class Transform [W] private[Transform] (parse: Parse, render: Render[W], rules: Rules, targetSuffix: String) {
  
  /** Represents a single transformation operation for a specific
   *  input that has already been parsed. Various types of output can be
   *  specified to trigger the actual rendering.
   */
  class Operation private[Transform] (raw: Document) { 

    private val document = raw rewriteWith rules.forContext(DocumentContext(raw))
    private val op = render from document
    
    /** Renders to the file with the specified name.
     * 
     *  @param name the name of the file to parse
     *  @param codec the character encoding of the file, if not specified the platform default will be used.
     */
    def toFile (name: String)(implicit codec: Codec) = op.toFile(name)(codec)
    
    /** Renders to the specified file.
     * 
     *  @param file the file to write to
     *  @param codec the character encoding of the file, if not specified the platform default will be used.
     */
    def toFile (file: File)(implicit codec: Codec) = op.toFile(file)(codec)


    /** Renders to the specified output stream.
     * 
     *  @param stream the stream to render to
     *  @param codec the character encoding of the stream, if not specified the platform default will be used.
     */
    def toStream (stream: OutputStream)(implicit codec: Codec) = op.toStream(stream)(codec)
    
    /** Renders directly to the console.
      */
    def toConsole = op toConsole

    /** Renders to the specified writer.
      */
    def toWriter (writer: Writer) = op toWriter writer
    
    /** Renders to the specified `StringBuilder`.
     */
    def toBuilder (builder: StringBuilder) = op toBuilder builder 
    
    def toOutput (output: Output) = op toOutput output

    /** Renders to a String and returns it.
     */
    override def toString = op toString
    
  }

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
      = new Transform(parse, render, rules + newRule, targetSuffix) 
  
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
  def rendering (customRenderer: W => RenderFunction) 
      = new Transform(parse, render using customRenderer, rules, targetSuffix)
  
  
  /** Parses the specified string and returns a new Operation instance which allows to specify the output.
   *  Any kind of input is valid, including an empty string. 
   */
  def fromString (str: String) = new Operation(parse.fromString(str))
  
  /** Parses the input from the specified reader
   *  and returns a new Operation instance which allows to specify the output.
   */
  def fromReader (reader: Reader) = new Operation(parse.fromReader(reader))
  
  /** Parses the file with the specified name
   *  and returns a new Operation instance which allows to specify the output.
   *  Any kind of character input is valid, including empty files.
   * 
   *  @param name the name of the file to parse
   *  @param codec the character encoding of the file, if not specified the platform default will be used.
   */
  def fromFile (name: String)(implicit codec: Codec) = new Operation(parse.fromFile(name)(codec))
  
  /** Parses the specified file
   *  and returns a new Operation instance which allows to specify the output.
   *  Any kind of character input is valid, including empty files.
   * 
   *  @param file the file to read from
   *  @param codec the character encoding of the file, if not specified the platform default will be used.
   */
  def fromFile (file: File)(implicit codec: Codec) = new Operation(parse.fromFile(file)(codec))
  
  /** Parses the input from the specified stream
   *  and returns a new Operation instance which allows to specify the output.
   * 
   *  @param stream the stream to use as input for the parser
   *  @param codec the character encoding of the stream, if not specified the platform default will be used.
   */
  def fromStream (stream: InputStream)(implicit codec: Codec) = new Operation(parse.fromStream(stream)(codec))
  
  
  /** API for configuring a batch operation for a directory.
   *  Gives access to all relevant aspects of traversing, parsing and rendering
   *  a tree of inputs.
   */
  class DirectoryConfigBuilder private[Transform] (inputBuilder: InputConfigBuilder, isParallel: Boolean = false) {
    
    /** Specifies the template engine to use for 
     *  parsing all template inputs found in the tree.
     */
    def withTemplates (parse: ParseTemplate) = 
      new DirectoryConfigBuilder(inputBuilder.withTemplates(parse), isParallel)
    
    /** Specifies custom template directives to use with
     *  the default template engine.
     */
    def withTemplateDirectives (directives: Templates.Directive*) =
      new DirectoryConfigBuilder(inputBuilder.withTemplateDirectives(directives:_*), isParallel)
    
    /** Specifies the function to use for determining the document type
     *  of the input based on its path.
     */
    def withDocTypeMatcher (matcher: Path => DocumentType) =
      new DirectoryConfigBuilder(inputBuilder.withDocTypeMatcher(matcher), isParallel)

    /** Specifies a root configuration file that gets
     *  inherited by this tree and its subtrees.
     *  The syntax of the input is expected to be of a format
     *  compatible with the Typesafe Config library.
     */
    def withConfigFile (file: File) = 
      new DirectoryConfigBuilder(inputBuilder.withConfigFile(file), isParallel)
    
    /** Specifies the name of a root configuration file that gets
     *  inherited by this tree and its subtrees.
     *  The syntax of the input is expected to be of a format
     *  compatible with the Typesafe Config library.
     */
    def withConfigFile (name: String) =
      new DirectoryConfigBuilder(inputBuilder.withConfigFile(name), isParallel)
    
    /** Specifies a root configuration source that gets
     *  inherited by this tree and its subtrees.
     *  The syntax of the input is expected to be of a format
     *  compatible with the Typesafe Config library.
     */
    def withConfigString (source: String) =
      new DirectoryConfigBuilder(inputBuilder.withConfigString(source), isParallel)
    
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
    def inParallel = new DirectoryConfigBuilder(inputBuilder.inParallel, true)
    
    /** Renders the result to files in the directory with the specified name.
     *  The root directory must exist, but any required subdirectories
     *  will be created on demand while rendering.
     */
    def toDirectory (name: String)(implicit codec: Codec) =
      execute(OutputProvider.Directory(name)(codec))
      
    /** Renders the result to files in the specified directory.
     *  The root directory must exist, but any required subdirectories
     *  will be created on demand while rendering.
     */
    def toDirectory (dir: File)(implicit codec: Codec) =
      execute(OutputProvider.Directory(dir)(codec))
    
    private def execute (outputBuilder: OutputConfigBuilder) = {
      withConfig(BatchConfig(inputBuilder.build(parse.parsers), if (isParallel) outputBuilder.inParallel.build else outputBuilder.build))
    }
  }
  
  /** Parses files from the specified directory and its subdirectories
   *  and returns a new builder instance which allows to specify the output and 
   *  other configuration options.
   * 
   *  @param name the name of the directory to traverse
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   *  @param return a builder which allows to specify the output and other configuration options
   */
  def fromDirectory (name: String)(implicit codec: Codec): DirectoryConfigBuilder = fromDirectory(name, hiddenFileFilter)(codec)
  
  /** Parses files from the specified directory and its subdirectories
   *  and returns a new builder instance which allows to specify the output and 
   *  other configuration options.
   * 
   *  @param name the name of the directory to traverse
   *  @param exclude the files to exclude from processing
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   *  @param return a builder which allows to specify the output and other configuration options
   */
  def fromDirectory (name: String, exclude: FileFilter)(implicit codec: Codec) = new DirectoryConfigBuilder(InputProvider.Directory(name, exclude)(codec))

  /** Parses files from the specified directory and its subdirectories
   *  and returns a new builder instance which allows to specify the output and 
   *  other configuration options.
   * 
   *  @param dir the directory to traverse
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   *  @param return a builder which allows to specify the output and other configuration options
   */
  def fromDirectory (dir: File)(implicit codec: Codec): DirectoryConfigBuilder = fromDirectory(dir, hiddenFileFilter)(codec)
  
  /** Parses files from the specified directory and its subdirectories
   *  and returns a new builder instance which allows to specify the output and 
   *  other configuration options.
   * 
   *  @param dir the directory to traverse
   *  @param exclude the files to exclude from processing
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   *  @param return a builder which allows to specify the output and other configuration options
   */
  def fromDirectory (dir: File, exclude: FileFilter)(implicit codec: Codec) = new DirectoryConfigBuilder(InputProvider.Directory(dir, exclude)(codec))
  
  /** Parses files from the specified directories and its subdirectories, 
   *  merging them into a tree with a single root
   *  and returns a new builder instance which allows to specify the output and 
   *  other configuration options.
   * 
   *  @param roots the root directories to traverse
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   */
  def fromDirectories (roots: Seq[File])(implicit codec: Codec): DirectoryConfigBuilder = fromDirectories(roots, hiddenFileFilter)(codec)
  
  /** Parses files from the specified directories and its subdirectories, 
   *  merging them into a tree with a single root
   *  and returns a new builder instance which allows to specify the output and 
   *  other configuration options.
   * 
   *  @param roots the root directories to traverse
   *  @param exclude the files to exclude from processing
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   */
  def fromDirectories (roots: Seq[File], exclude: FileFilter)(implicit codec: Codec) = new DirectoryConfigBuilder(InputProvider.Directories(roots, exclude)(codec))
  
  /** Parses files from the `source` directory inside the current working directory
   *  and renders the result to the `target` directory inside the current working directory.
   * 
   *  @param exclude the files to exclude from processing
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   */
  def withDefaultDirectories (exclude: FileFilter = hiddenFileFilter)(implicit codec: Codec) = withConfig(DefaultDirectories(exclude)(codec).build(parse.parsers))
  
  /** Parses files from the `source` directory inside the specified root directory
   *  and renders the result to the `target` directory inside the root directory.
   *  Both directories must already exist inside the specified directory.
   * 
   *  @param name the name of the root directory that contains the source and target directory
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   */
  def withRootDirectory (name: String)(implicit codec: Codec): Unit = withRootDirectory(new File(name), hiddenFileFilter)(codec)
  
  /** Parses files from the `source` directory inside the specified root directory
   *  and renders the result to the `target` directory inside the root directory.
   *  Both directories must already exist inside the specified directory.
   * 
   *  @param name the name of the root directory that contains the source and target directory
   *  @param exclude the files to exclude from processing
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   */
  def withRootDirectory (name: String, exclude: FileFilter)(implicit codec: Codec): Unit = withRootDirectory(new File(name), exclude)(codec)
  
  /** Parses files from the `source` directory inside the specified root directory
   *  and renders the result to the `target` directory inside the root directory.
   *  Both directories must already exist inside the specified directory.
   * 
   *  @param dir the root directory that contains the source and target directory
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   */
  def withRootDirectory (dir: File)(implicit codec: Codec): Unit = withRootDirectory(dir, hiddenFileFilter)(codec)
  
  /** Parses files from the `source` directory inside the specified root directory
   *  and renders the result to the `target` directory inside the root directory.
   *  Both directories must already exist inside the specified directory.
   * 
   *  @param dir the root directory that contains the source and target directory
   *  @param exclude the files to exclude from processing
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   */
  def withRootDirectory (dir: File, exclude: FileFilter)(implicit codec: Codec): Unit = withConfig(RootDirectory(dir, exclude)(codec).build(parse.parsers))
  
  /** Transforms documents according to the specified batch configuration.
   *  
   *  @param builder a builder for the configuration for the input and output trees to process
   */
  def withConfig (builder: BatchConfigBuilder): Unit = withConfig(builder.build(parse.parsers)) 

  /** Transforms documents according to the specified batch configuration.
   *  
   *  @param config the configuration for the input and output trees to process
   */
  def withConfig (config: BatchConfig): Unit = {

    val tree = parse.fromTree(config.input)

    val rewritten = tree.rewrite(rules.all, AutonumberContext.defaults)
    
    render from rewritten toTree config.output
  }
  

    
} 

/** Serves as an entry point to the Transform API.
 * 
 *  @author Jens Halm
 */
object Transform {
   
  private[laika] class Rules (rules: List[DocumentContext => RewriteRule]){
    
    def all = rules.reverse
    
    def forContext (context: DocumentContext) = (rules map { _(context) }).reverse      
    
    def + (newRule: DocumentContext => RewriteRule) = new Rules(newRule :: rules)
    
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
    def to [W] (factory: RendererFactory[W]): Transform[W] = 
      new Transform(parse.asRawDocument, Render as factory, new Rules(Nil), factory.fileSuffix) 
    
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
  
  /** The configuration for a batch process, containing the configuration
   *  for the inputs and outputs to use.
   */
  case class BatchConfig (input: InputConfig, output: OutputConfig)
  
  /** API for configuring a batch operation.
   *  Gives access to all relevant aspects of traversing, parsing and rendering
   *  a tree of inputs.
   */
  class BatchConfigBuilder private[Transform] (inputBuilder: InputConfigBuilder, outputBuilder: OutputConfigBuilder) {
    
    /** Specifies the template engine to use for 
     *  parsing all template inputs found in the tree.
     */
    def withTemplates (parser: ParseTemplate) = 
      new BatchConfigBuilder(inputBuilder.withTemplates(parser), outputBuilder)
    
    /** Specifies custom template directives to use with
     *  the default template engine.
     */
    def withTemplateDirectives (directives: Templates.Directive*) =
      new BatchConfigBuilder(inputBuilder.withTemplateDirectives(directives:_*), outputBuilder)
    
    /** Specifies the function to use for determining the document type
     *  of the input based on its path.
     */
    def withDocTypeMatcher (matcher: Path => DocumentType) =
      new BatchConfigBuilder(inputBuilder.withDocTypeMatcher(matcher), outputBuilder)

    /** Specifies a root configuration file that gets
     *  inherited by this tree and its subtrees.
     *  The syntax of the input is expected to be of a format
     *  compatible with the Typesafe Config library.
     */
    def withConfigFile (file: File) = 
      new BatchConfigBuilder(inputBuilder.withConfigFile(file), outputBuilder)
    
    /** Specifies the name of a root configuration file that gets
     *  inherited by this tree and its subtrees.
     *  The syntax of the input is expected to be of a format
     *  compatible with the Typesafe Config library.
     */
    def withConfigFile (name: String) =
      new BatchConfigBuilder(inputBuilder.withConfigFile(name), outputBuilder)
    
    /** Specifies a root configuration source that gets
     *  inherited by this tree and its subtrees.
     *  The syntax of the input is expected to be of a format
     *  compatible with the Typesafe Config library.
     */
    def withConfigString (source: String) =
      new BatchConfigBuilder(inputBuilder.withConfigString(source), outputBuilder)
    
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
    def inParallel = new BatchConfigBuilder(inputBuilder.inParallel, outputBuilder.inParallel)
    
    /** Builds the final configuration for this input tree
     *  for the specified parser factory.
     */
    def build (parsers: Parsers) = BatchConfig(inputBuilder.build(parsers), outputBuilder.build)
  }
  
  /** Factory methods for creating BatchConfigBuilders.
   */
  object BatchConfigBuilder {

    /** Creates a BatchConfigBuilder instance for a specific root directory in the file system.
     *  
     *  This factory method expects the default directory layout where the root directory
     *  contains a `source` subdirectory containing all markup and template files to process
     *  and a `target` directory the renderer will write to.
     */
    def apply (root: File, exclude: FileFilter, codec: Codec) = {
      require(root.exists, s"Directory ${root.getAbsolutePath} does not exist")
      require(root.isDirectory, s"File ${root.getAbsolutePath} is not a directory")
      
      val sourceDir = new File(root, "source")
      val targetDir = new File(root, "target")
      
      new BatchConfigBuilder(InputProvider.Directory(sourceDir, exclude)(codec), OutputProvider.Directory(targetDir)(codec))
    }
    
    /** Creates a BatchConfigBuilder instance containing separate builders for input and output.
     */
    def apply (inputBuilder: InputConfigBuilder, outputBuilder: OutputConfigBuilder) = 
      new BatchConfigBuilder(inputBuilder, outputBuilder)
    
  }
  
  /** Creates BatchConfigBuilder instances for a specific root directory in the file system.
   *  
   *  This entry point expects the default directory layout where the root directory
   *  contains a `source` subdirectory containing all markup and template files to process
   *  and a `target` directory the renderer will write to.
   */
  object RootDirectory {
    def apply (name: String)(implicit codec: Codec): BatchConfigBuilder = apply(name, hiddenFileFilter)(codec)
    def apply (name: String, exclude: FileFilter)(implicit codec: Codec) = BatchConfigBuilder(new File(name), exclude, codec)
    def apply (file: File)(implicit codec: Codec): BatchConfigBuilder = apply(file, hiddenFileFilter)(codec)
    def apply (file: File, exclude: FileFilter)(implicit codec: Codec) = BatchConfigBuilder(file, exclude, codec)
  }
  
  /** Creates BatchConfigBuilder instances using the current working directory as its root.
   *  
   *  This entry point expects the default directory layout where the root directory
   *  contains a `source` subdirectory containing all markup and template files to process
   *  and a `target` directory the renderer will write to.
   */
  object DefaultDirectories {
    def apply (exclude: FileFilter = hiddenFileFilter)(implicit codec: Codec) = BatchConfigBuilder(new File(System.getProperty("user.dir")), exclude, codec)
  }
  
  
  
}
