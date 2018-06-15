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

import java.io.{File, InputStream, Reader}

import com.typesafe.config.{ConfigFactory, Config => TConfig}
import laika.api.Parse.Parsers
import laika.api.ext.{ConfigProvider, ExtensionBundle}
import laika.factory.ParserFactory
import laika.io.DocumentType._
import laika.io.InputProvider._
import laika.io.{DocumentType, IO, Input, InputProvider}
import laika.parse.core.Parser
import laika.parse.core.combinator.Parsers.{documentParserFunction, success}
import laika.parse.css.Styles.{StyleDeclaration, StyleDeclarationSet}
import laika.rewrite.{DocumentCursor, RewriteRules}
import laika.tree.Documents._
import laika.tree.Elements.RewriteRule
import laika.tree.Paths.Path

import scala.io.Codec
  
/** API for performing a parse operation from various types of input to obtain
 *  a document tree without a subsequent render operation. 
 *  
 *  In cases where a render operation should follow immediately, it is more 
 *  convenient to use the [[laika.api.Transform]] API instead which 
 *  combines a parse and a render operation directly.
 *  
 *  Example for parsing Markdown from a file:
 *  
 *  {{{
 *  val doc = Parse as Markdown fromFile "hello.md"
 *  }}}
 *  
 *  Example for parsing from an entire directory:
 *  
 *  {{{
 *  val tree = Parse as Markdown fromDirectory "path/to/source"
 *  }}}
 *  
 *  Example for parsing a directory that contains markup documents in different formats:
 *  
 *  {{{
 *  val tree = Parse as Markdown or ReStructuredText fromDirectory "path/to/source"
 *  }}}
 * 
 *  @author Jens Halm
 */
class Parse private (parsers: Parsers, rewrite: Boolean, bundles: Seq[ExtensionBundle]) {
  
  /** The file suffixes recognized by this parser.
   *  When transforming entire directories only files with
   *  names ending in one of the specified suffixes will
   *  be consired. 
   */
  val fileSuffixes: Set[String] = parsers.suffixes
  
  /** Returns a new Parse instance adding the specified parser factory.
   *  This factory is usually an object provided by the library
   *  or a plugin that is capable of parsing a specific markup
   *  format like Markdown or reStructuredText.
   *  
   *  This method is useful if you want to combine different markup
   *  formats within a single document tree. 
   * 
   *  @param factory the parser factory to add to the previously specified parsers
   */
  def or (factory: ParserFactory): Parse = new Parse(parsers.withFactory(factory), rewrite, bundles)

  /** Returns a new Parse instance with the specified extension bundles installed.
    *
    * Bundles are usually provided by libraries (by Laika itself or a 3rd-party extension library)
    * or as re-usable building blocks by application code.
    */
  def using (bundles: ExtensionBundle*): Parse = new Parse(parsers, rewrite, this.bundles ++ bundles)

  /** Returns a new Parse instance that produces raw document trees without applying
   *  the default rewrite rules. These rules resolve link and image references and 
   *  rearrange the tree into a hierarchy of sections based on the (flat) sequence
   *  of header instances found in the document.
   */
  def withoutRewrite: Parse = new Parse(parsers, false, bundles)
  
  /** Returns a document obtained from parsing the specified string.
   *  Any kind of input is valid, including an empty string. 
   */
  def fromString (str: String): Document = fromInput(Input.fromString(str))
  
  /** Returns a document obtained from parsing the input from the specified reader.
   */
  def fromReader (reader: Reader): Document = fromInput(Input.fromReader(reader))

  /** Returns a document obtained from parsing the file with the specified name.
   *  Any kind of character input is valid, including empty files.
   * 
   *  @param name the name of the file to parse
   *  @param codec the character encoding of the file, if not specified the platform default will be used.
   */
  def fromFile (name: String)(implicit codec: Codec): Document = fromInput(Input.fromFile(name)(codec))
  
  /** Returns a document obtained from parsing the specified file.
   *  Any kind of character input is valid, including empty files.
   * 
   *  @param file the file to use as input
   *  @param codec the character encoding of the file, if not specified the platform default will be used.
   */
  def fromFile (file: File)(implicit codec: Codec): Document = fromInput(Input.fromFile(file)(codec))
  
  /** Returns a document obtained from parsing the input from the specified stream.
   * 
   *  @param stream the stream to use as input for the parser
   *  @param codec the character encoding of the stream, if not specified the platform default will be used.
   */
  def fromStream (stream: InputStream)(implicit codec: Codec): Document = fromInput(Input.fromStream(stream)(codec))
  
  /** Returns a document obtained from parsing the specified input.
   *  
   *  This is a generic method based on Laika's IO abstraction layer that concrete
   *  methods delegate to. Usually not used directly in application code, but
   *  might come in handy for very special requirements.
   * 
   *  @param input the input for the parser
   */
  def fromInput (input: Input): Document = {
    
    val doc = IO(input)(parsers.forInput(input))

    if (rewrite) doc.rewrite(parsers.rewriteRulesFor(input, doc))
    else doc
  }
  
  /** Returns a document tree obtained by parsing files from the
   *  specified directory and its subdirectories.
   * 
   *  @param name the name of the directory to traverse
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   */
  def fromDirectory (name: String)(implicit codec: Codec): DocumentTree = fromTree(Directory(name, hiddenFileFilter)(codec))
  
  /** Returns a document tree obtained by parsing files from the
   *  specified directory and its subdirectories.
   * 
   *  @param name the name of the directory to traverse
   *  @param exclude the files to exclude from processing
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   */
  def fromDirectory (name: String, exclude: FileFilter)(implicit codec: Codec): DocumentTree = fromTree(Directory(name, exclude)(codec))

  /** Returns a document tree obtained by parsing files from the
   *  specified directory and its subdirectories.
   * 
   *  @param dir the root directory to traverse
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   */
  def fromDirectory (dir: File)(implicit codec: Codec): DocumentTree = fromTree(Directory(dir, hiddenFileFilter)(codec))

  /** Returns a document tree obtained by parsing files from the
   *  specified directory and its subdirectories.
   * 
   *  @param dir the root directory to traverse
   *  @param exclude the files to exclude from processing
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   */
  def fromDirectory (dir: File, exclude: FileFilter)(implicit codec: Codec): DocumentTree = fromTree(Directory(dir, exclude)(codec))
  
  /** Returns a document tree obtained by parsing files from the
   *  specified directories and its subdirectories, merging them into
   *  a tree with a single root.
   * 
   *  @param roots the root directories to traverse
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   */
  def fromDirectories (roots: Seq[File])(implicit codec: Codec): DocumentTree = fromTree(Directories(roots, hiddenFileFilter)(codec))
  
  /** Returns a document tree obtained by parsing files from the
   *  specified directories and its subdirectories, merging them into
   *  a tree with a single root.
   * 
   *  @param roots the root directories to traverse
   *  @param exclude the files to exclude from processing
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   */
  def fromDirectories (roots: Seq[File], exclude: FileFilter)(implicit codec: Codec): DocumentTree = 
    fromTree(Directories(roots, hiddenFileFilter)(codec))
  
  /** Returns a document tree obtained by parsing files from the
   *  current working directory.
   * 
   *  @param exclude the files to exclude from processing
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   */
  def fromDefaultDirectory (exclude: FileFilter = hiddenFileFilter)(implicit codec: Codec): DocumentTree = 
    fromTree(DefaultDirectory(exclude)(codec))
  
  /** Returns a document tree obtained by parsing files from the
   *  specified input configuration builder.
   *  
   *  @param builder a builder for the configuration for the input tree to process
   */
  def fromTree (builder: InputConfigBuilder): DocumentTree = {
    val bundle = bundles.reverse.reduceLeft(_ withBase _) // TODO - move this to OperationSupport.mergedBundle
    fromTree(builder.build(parsers.suffixes, bundle.docTypeMatcher))
  }
  
  /** Returns a document tree obtained by parsing files from the
   *  specified input configuration.
   *  
   *  @param config the configuration for the input tree to process
   */
  def fromTree (config: InputConfig): DocumentTree = {

    val bundle = bundles.reverse.reduceLeft(_ withBase _) // TODO - move this to OperationSupport.mergedBundle

    case class TreeConfig (path: Path, config: TConfig)
    
    type Operation[T] = () => (DocumentType, T)

    def parseMarkup (input: Input): Operation[Document] = () => (Markup, IO(input)(parsers.forInput(input)))
    
    def parseTemplate (docType: DocumentType)(input: Input): Operation[TemplateDocument] = () => (docType, IO(input)(config.templateParser.fromInput))

    def parseStyleSheet (format: String)(input: Input): Operation[StyleDeclarationSet] = () => {
      val parser = bundle.parserDefinitions.styleSheetParser.getOrElse(success(Set.empty[StyleDeclaration]))
      val docF = documentParserFunction(parser, StyleDeclarationSet.forPath)
      (StyleSheet(format), IO(input)(docF))
    }
    
    def parseTreeConfig (input: Input): Operation[TreeConfig] = () => (Config, TreeConfig(input.path, ConfigProvider.fromInput(input)))

    def collectOperations[T] (provider: InputProvider, f: InputProvider => Seq[Operation[T]]): Seq[Operation[T]] =
      f(provider) ++ (provider.subtrees flatMap (collectOperations(_, f)))
    
    val operations = collectOperations(config.provider, _.markupDocuments.map(parseMarkup)) ++
                     collectOperations(config.provider, _.templates.map(parseTemplate(Template))) ++
                     collectOperations(config.provider, _.dynamicDocuments.map(parseTemplate(Dynamic))) ++
                     collectOperations(config.provider, _.styleSheets.flatMap({ case (format,inputs) => inputs map parseStyleSheet(format) }).toSeq) ++
                     collectOperations(config.provider, _.configDocuments.find(_.path.name == "directory.conf").toList.map(parseTreeConfig)) // TODO - filename could be configurable
    
    val results = if (config.parallel) operations.par map (_()) seq else operations map (_())
    
    val docMap = (results collect {
      case (Markup, doc: Document) => (doc.path, doc)
    }) toMap
    
    val templateMap = (results collect {
      case (docType, doc: TemplateDocument) => ((docType, doc.path), doc)
    }) toMap
    
    val styleMap = (results collect {
      case (StyleSheet(_), style: StyleDeclarationSet) => (style.paths.head, style)
    }) toMap
    
    val treeConfigMap = (results collect {
      case (Config, config: TreeConfig) => (config.path, config.config)
    }) toMap
    
    def collectDocuments (provider: InputProvider, root: Boolean = false): DocumentTree = {
      val docs = provider.markupDocuments map (i => docMap(i.path))
      val trees = provider.subtrees map (collectDocuments(_))

      val templates = provider.templates map (i => templateMap((Template,i.path)))
      val styles = (provider.styleSheets mapValues (_.map(i => styleMap(i.path)).reduce(_++_))) withDefaultValue StyleDeclarationSet.empty

      val dynamic = provider.dynamicDocuments map (i => templateMap((Dynamic,i.path)))
      val static = provider.staticDocuments map StaticDocument
      val additionalContent: Seq[AdditionalContent] = dynamic ++ static

      val treeConfig = provider.configDocuments.find(_.path.name == "directory.conf").map(i => treeConfigMap(i.path))
      val rootConfig = if (root) Seq(bundle.baseConfig) else Nil
      val config = (treeConfig.toList ++ rootConfig) reduceLeftOption (_ withFallback _) getOrElse ConfigFactory.empty

      DocumentTree(provider.path, docs ++ trees, templates, styles, additionalContent, config, sourcePaths = provider.sourcePaths)
    }
    
    val tree = collectDocuments(config.provider, root = true)
    
    if (rewrite) {
      val rules = parsers.rewriteRules
      tree.rewrite(rules)
    } 
    else tree
  }
  
  
}

/** Serves as an entry point to the Parse API.
 * 
 *  @author Jens Halm
 */
object Parse {
  
  /** Returns a new Parse instance for the specified parser factory.
   *  This factory is usually an object provided by the library
   *  or a plugin that is capable of parsing a specific markup
   *  format like Markdown or reStructuredText. 
   * 
   *  @param factory the parser factory to use for all subsequent operations
   */
  def as (factory: ParserFactory): Parse = new Parse(new Parsers(factory), true, Seq(ExtensionBundle.LaikaDefaults))

  private[laika] class Parsers (parsers: Seq[ParserFactory]) {
    
    def this (factory: ParserFactory) = this(Seq(factory))
    
    def withFactory (factory: ParserFactory): Parsers = new Parsers(parsers :+ factory)
        
    private def suffix (name: String): String = name.lastIndexOf(".") match {
      case -1    => ""
      case index => name.drop(index+1)
    }  
    
    lazy val map: Map[String, ParserFactory] =
      parsers flatMap (p => p.fileSuffixes map ((_, p))) toMap
    
    lazy val suffixes: Set[String] = parsers flatMap (_.fileSuffixes) toSet
    
    private def parserForInput (input: Input): ParserFactory = {
      if (parsers.size == 1) parsers.head
      else map.getOrElse(suffix(input.name), throw new IllegalArgumentException("Unable to determine parser based on input name: ${input.name}"))
    }
    
    def rewriteRules: DocumentCursor => RewriteRule = RewriteRules.defaultsFor(parsers: _*)
    
    def rewriteRulesFor (input: Input, doc: Document): RewriteRule = RewriteRules.defaultsFor(parserForInput(input))(DocumentCursor(doc))
    
    def forInput (input: Input): Input => Document = parserForInput(input).newParser
    
  }
  
  
}
