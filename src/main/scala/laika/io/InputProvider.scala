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

package laika.io

import java.io.File
import laika.tree.Documents._
import scala.io.Codec
import laika.template.ParseTemplate
import laika.directive.Directives.Templates
import laika.factory.ParserFactory
import laika.template.DefaultTemplate

/** Represents a tree structure of Inputs, abstracting over various types of IO resources. 
 *  
 *  While the default implementations wrap the structure of directories in the file system,
 *  other implementations may build an entirely virtual input tree structure. 
 * 
 *  @author Jens Halm
 */
trait InputProvider {

  
  /** The local name of the tree represented by this provider.
   */
  lazy val name: String = path.name

  /** The full path of the tree represented by this provider.
   *  This path is always an absolute path
   *  from the root of the (virtual) input tree,
   *  therefore does not represent the filesystem
   *  path in case of file I/O.
   */
  def path: Path
  
  /** All inputs for configuration files
   *  on this level of the input hierarchy.
   */
  def configDocuments: Seq[Input]

  /** All inputs for markup documents
   *  that need to be parsed
   *  on this level of the input hierarchy.
   */
  def markupDocuments: Seq[Input]

  /** All inputs for dynamic files
   *  that need to be processed
   *  on this level of the input hierarchy.
   */
  def dynamicDocuments: Seq[Input]

  /** All inputs for static files
   *  that need to be copied
   *  from this level of the input hierarchy.
   */
  def staticDocuments: Seq[Input]

  /** All inputs for template files
   *  on this level of the input hierarchy.
   */
  def templates: Seq[Input]
  
  /** All subtrees of this provider.
   */
  def subtrees: Seq[InputProvider]
  
  
}


/** Factory methods for creating `InputProvider` and `InputConfigBuilder`
 *  instances. The latter offers fine grained control like setting
 *  custom document type matchers or custom template engines, before
 *  creating the actual `InputProvider`.
 */
object InputProvider {
  
  private class DirectoryInputProvider (dir: File, val path: Path, docTypeMatcher: Path => DocumentType, codec: Codec) extends InputProvider {
    
    private def docType (f: File) = docTypeMatcher(path / f.getName)

    private def toInput (pairs: Array[(DocumentType,File)]) = pairs.map(p => Input.fromFile(p._2, path)(codec)).toList

    private lazy val files = dir.listFiles filter (_.isFile) map (f => (docType(f), f)) groupBy (_._1)
    
    private def documents (docType: DocumentType) = files.get(docType).map(toInput).getOrElse(Nil)
    
    lazy val configDocuments = documents(Config)
    
    lazy val markupDocuments = documents(Markup)
    
    lazy val dynamicDocuments = documents(Dynamic)
    
    lazy val staticDocuments = documents(Static)
    
    lazy val templates =  documents(Template)
    
    lazy val subtrees = {
      dir.listFiles filter (f => f.isDirectory && docType(f) != Ignored) map (d => new DirectoryInputProvider(d, path / d.getName, docTypeMatcher, codec)) toList
    }
    
  }

  /** Creates an InputProvider based on the specified directory, including
   *  all subdirectories.
   * 
   *  @param root the root directory of the input tree
   *  @param docTypeMatcher a function determining the document type based on the path of the input
   *  @param codec the character encoding of the files, if not specified the platform default will be used
   */
  def forRootDirectory (root: File, docTypeMatcher: Path => DocumentType)(implicit codec: Codec): InputProvider = {
    require(root.exists, "Directory "+root.getAbsolutePath()+" does not exist")
    require(root.isDirectory, "File "+root.getAbsolutePath()+" is not a directory")
    
    new DirectoryInputProvider(root, Root, docTypeMatcher, codec)
  }
  
  /** The configuration for an input tree, consisting of the actual provider for
   *  all inputs, a separate input for (optional) root configuration sources,
   *  the template engine to use and a flag whether parsing should be performed
   *  in parallel.
   */
  case class InputConfig (provider: InputProvider, config: Seq[Input], templateParser: ParseTemplate, parallel: Boolean)
  
  /** Responsible for building new InputProviders based
   *  on the specified document type matcher and codec.
   */
  trait ProviderBuilder {
    def build (docTypeMatcher: Path => DocumentType, codec: Codec): InputProvider
  }
  
  private[InputProvider] class DirectoryProviderBuilder (root: File) extends ProviderBuilder {
    def build (docTypeMatcher: Path => DocumentType, codec: Codec) = 
      InputProvider.forRootDirectory(root, docTypeMatcher)(codec)
  }

  /** API for configuring an input tree.
   *  Gives access to all relevant aspects of traversing, parsing and processing
   *  a tree of inputs.
   */
  class InputConfigBuilder (
      provider: ProviderBuilder,
      codec: Codec,
      docTypeMatcher: Option[Path => DocumentType] = None,
      templateParser: Option[ParseTemplate] = None,
      config: List[Input] = Nil,
      isParallel: Boolean = false) {
    
    /** Specifies the template engine to use for 
     *  parsing all template inputs found in the tree.
     */
    def withTemplates (parser: ParseTemplate) = 
      new InputConfigBuilder(provider, codec, docTypeMatcher, Some(parser), config, isParallel)
    
    /** Specifies custom template directives to use with
     *  the default template engine.
     */
    def withTemplateDirectives (directives: Templates.Directive*) =
      withTemplates(ParseTemplate as DefaultTemplate.withDirectives(directives:_*))

    /** Specifies the function to use for determining the document type
     *  of the input based on its path.
     */
    def withDocTypeMatcher (matcher: Path => DocumentType) =
      new InputConfigBuilder(provider, codec, Some(matcher), templateParser, config, isParallel)

    /** Specifies a root configuration file that gets
     *  inherited by this tree and its subtrees.
     *  The syntax of the input is expected to be of a format
     *  compatible with the Typesafe Config library.
     */
    def withConfigFile (file: File) = withConfigInput(Input.fromFile(file)(codec))
    
    /** Specifies the name of a root configuration file that gets
     *  inherited by this tree and its subtrees.
     *  The syntax of the input is expected to be of a format
     *  compatible with the Typesafe Config library.
     */
    def withConfigFile (name: String) = withConfigInput(Input.fromFile(name)(codec))
    
    /** Specifies a root configuration source that gets
     *  inherited by this tree and its subtrees.
     *  The syntax of the input is expected to be of a format
     *  compatible with the Typesafe Config library.
     */
    def withConfigString (source: String) = withConfigInput(Input.fromString(source))
    
    private def withConfigInput (input: Input) = 
      new InputConfigBuilder(provider, codec, docTypeMatcher, templateParser, input :: config, isParallel)
    
    /** Instructs the parser to process all inputs in parallel.
     *  The recursive structure of inputs will be flattened before parsing
     *  and then get reassembled afterwards, therefore the parallel processing
     *  includes all subtrees of this input tree.
     */
    def inParallel = new InputConfigBuilder(provider, codec, docTypeMatcher, templateParser, config, true) // TODO - custom TaskSupport
    
    /** Builds the final configuration for this input tree
     *  for the specified parser factory.
     */
    def build (parser: ParserFactory) = {
      val matcher = docTypeMatcher getOrElse new DefaultDocumentTypeMatcher(parser.fileSuffixes, Seq("*.svn","*.git"))
      val templates = templateParser getOrElse ParseTemplate
      InputConfig(provider.build(matcher, codec), config, templates, isParallel)
    }
  }

  /** Creates InputConfigBuilder instances for a specific root directory in the file system.
   */
  object Directory {
    def apply (name: String)(implicit codec: Codec) = new InputConfigBuilder(new DirectoryProviderBuilder(new File(name)), codec)
    def apply (file: File)(implicit codec: Codec) = new InputConfigBuilder(new DirectoryProviderBuilder(file), codec)
  }
  
  /** Creates InputConfigBuilder instances using the current working directory as its root.
   */
  object DefaultDirectory {
    def apply (implicit codec: Codec) = Directory(System.getProperty("user.dir"))(codec)
  }
  
}
