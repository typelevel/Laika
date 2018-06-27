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
import laika.api.config.{OperationConfig, OperationConfigBuilder}
import laika.api.ext.{ConfigProvider, ExtensionBundle}
import laika.directive.ConfigParser
import laika.factory.ParserFactory
import laika.io.DocumentType._
import laika.io.InputTree._
import laika.io.{DocumentType, IO, Input, InputTree}
import laika.parse.core.Parser
import laika.parse.core.combinator.Parsers.{documentParserFunction, success}
import laika.parse.core.text.TextParsers.{opt, unsafeParserFunction}
import laika.parse.css.Styles.{StyleDeclaration, StyleDeclarationSet}
import laika.tree.Documents._
import laika.tree.Elements.{InvalidSpan, SystemMessage}
import laika.tree.Paths.Path
import laika.tree.Templates.{TemplateElement, TemplateRoot, TemplateString}
import laika.util.~

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
class Parse private (parsers: Seq[ParserFactory], protected[api] val config: OperationConfig, rewrite: Boolean) extends OperationConfigBuilder {

  type ThisType = Parse

  protected[api] def withConfig(newConfig: OperationConfig): ThisType = new Parse(parsers, newConfig, rewrite)

  private lazy val mergedBundle: ExtensionBundle = ExtensionBundle.mergeBundles(config.bundles)

  /** The file suffixes recognized by this parser.
   *  When transforming entire directories only files with
   *  names ending in one of the specified suffixes will
   *  be consired. 
   */
  val fileSuffixes: Set[String] = parsers flatMap (_.fileSuffixes) toSet
  
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
  def or (factory: ParserFactory): Parse = new Parse(parsers :+ factory, config.withBundles(factory.extensions), rewrite)

  /** Returns a new Parse instance that produces raw document trees without applying
   *  the default rewrite rules. These rules resolve link and image references and 
   *  rearrange the tree into a hierarchy of sections based on the (flat) sequence
   *  of header instances found in the document.
   */
  def withoutRewrite: Parse = new Parse(parsers, config, rewrite = false)
  
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

    val doc = IO(input)(parserLookup.forInput(input))

    if (rewrite) doc.rewrite(config.rewriteRuleFor(doc))
    else doc
  }
  
  /** Returns a document tree obtained by parsing files from the
   *  specified directory and its subdirectories.
   * 
   *  @param name the name of the directory to traverse
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   */
  def fromDirectory (name: String)(implicit codec: Codec): DocumentTree =
    fromDirectory(new File(name), hiddenFileFilter)(codec)
  
  /** Returns a document tree obtained by parsing files from the
   *  specified directory and its subdirectories.
   * 
   *  @param name the name of the directory to traverse
   *  @param exclude the files to exclude from processing
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   */
  def fromDirectory (name: String, exclude: FileFilter)(implicit codec: Codec): DocumentTree =
    fromDirectory(new File(name), exclude)(codec)

  /** Returns a document tree obtained by parsing files from the
   *  specified directory and its subdirectories.
   * 
   *  @param dir the root directory to traverse
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   */
  def fromDirectory (dir: File)(implicit codec: Codec): DocumentTree =
    fromDirectory(dir, hiddenFileFilter)(codec)

  /** Returns a document tree obtained by parsing files from the
   *  specified directory and its subdirectories.
   * 
   *  @param dir the root directory to traverse
   *  @param exclude the files to exclude from processing
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   */
  def fromDirectory (dir: File, exclude: FileFilter)(implicit codec: Codec): DocumentTree =
    fromDirectories(Seq(dir), exclude)(codec)

  /** Returns a document tree obtained by parsing files from the
   *  specified directories and its subdirectories, merging them into
   *  a tree with a single root.
   * 
   *  @param roots the root directories to traverse
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   */
  def fromDirectories (roots: Seq[File])(implicit codec: Codec): DocumentTree =
    fromDirectories(roots, hiddenFileFilter)(codec)
  
  /** Returns a document tree obtained by parsing files from the
   *  specified directories and its subdirectories, merging them into
   *  a tree with a single root.
   * 
   *  @param roots the root directories to traverse
   *  @param exclude the files to exclude from processing
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   */
  def fromDirectories (roots: Seq[File], exclude: FileFilter)(implicit codec: Codec): DocumentTree = 
    fromInputTree(forRootDirectories(roots, config.docTypeMatcher, exclude)(codec))

  /** Returns a document tree obtained by parsing files from the
   *  current working directory.
   *
   *  @param exclude the files to exclude from processing
   *  @param codec the character encoding of the files, if not specified the platform default will be used.
   */
  def fromDefaultDirectory (exclude: FileFilter = hiddenFileFilter)(implicit codec: Codec): DocumentTree =
    fromInputTree(forWorkingDirectory(config.docTypeMatcher, exclude)(codec))

  /** Returns a document tree obtained by parsing files from the
   *  specified input tree builder.
   *
   *  @param builder a builder for the input tree to process
   */
  def fromInputTree(builder: InputTreeBuilder): DocumentTree =
    fromInputTree(builder.build(config.docTypeMatcher))

  /** Returns a document tree obtained by parsing files from the
   *  specified input tree.
   *
   *  @param inputTree the input tree to process
   */
  def fromInputTree(inputTree: InputTree): DocumentTree = {

    case class TreeConfig (path: Path, config: TConfig)

    type Operation[T] = () => (DocumentType, T)

    def parseMarkup (input: Input): Operation[Document] = () => (Markup, IO(input)(parserLookup.forInput(input)))

    def parseTemplate (docType: DocumentType)(input: Input): Seq[Operation[TemplateDocument]] = mergedBundle.parserDefinitions.templateParser match {
      case None => Seq()
      case Some(rootParser) =>

        // TODO - move this logic to new DocumentParsers and ConfigHeaderParsers

        def configParser (path: Path): Parser[Either[InvalidSpan,TConfig]] =
          ConfigParser.forPath(path, {
            (ex: Exception, str: String) => InvalidSpan(SystemMessage(laika.tree.Elements.Error,
              "Error parsing config header: "+ex.getMessage), TemplateString(s"{%$str%}"))
          })

        def templateWithConfig (path: Path): Parser[(TConfig, TemplateRoot)] = opt(configParser(path)) ~ rootParser ^^ {
          case Some(Right(config)) ~ root => (config, root)
          case Some(Left(span)) ~ root    => (ConfigFactory.empty(), root.copy(content = TemplateElement(span) +: root.content))
          case None ~ root                => (ConfigFactory.empty(), root)
        }

        def parseDocument (input: Input): TemplateDocument = {
          val (config, root) = unsafeParserFunction(templateWithConfig(input.path))(input.asParserInput)
          TemplateDocument(input.path, root, config)
        }

        Seq(() => (docType, IO(input)(parseDocument)))
    }

    def parseStyleSheet (format: String)(input: Input): Operation[StyleDeclarationSet] = () => {
      val parser = mergedBundle.parserDefinitions.styleSheetParser.getOrElse(success(Set.empty[StyleDeclaration]))
      val docF = documentParserFunction(parser, StyleDeclarationSet.forPath)
      (StyleSheet(format), IO(input)(docF))
    }

    def parseTreeConfig (input: Input): Operation[TreeConfig] = () => (Config, TreeConfig(input.path, ConfigProvider.fromInput(input)))

    def collectOperations[T] (provider: InputTree, f: InputTree => Seq[Operation[T]]): Seq[Operation[T]] =
      f(provider) ++ (provider.subtrees flatMap (collectOperations(_, f)))

    val operations = collectOperations(inputTree, _.markupDocuments.map(parseMarkup)) ++
                     collectOperations(inputTree, _.templates.flatMap(parseTemplate(Template))) ++
                     collectOperations(inputTree, _.dynamicDocuments.flatMap(parseTemplate(Dynamic))) ++
                     collectOperations(inputTree, _.styleSheets.flatMap({ case (format,inputs) => inputs map parseStyleSheet(format) }).toSeq) ++
                     collectOperations(inputTree, _.configDocuments.find(_.path.name == "directory.conf").toList.map(parseTreeConfig)) // TODO - filename could be configurable

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

    def collectDocuments (provider: InputTree, root: Boolean = false): DocumentTree = {
      val docs = provider.markupDocuments map (i => docMap(i.path))
      val trees = provider.subtrees map (collectDocuments(_))

      val templates = provider.templates map (i => templateMap((Template,i.path)))
      val styles = (provider.styleSheets mapValues (_.map(i => styleMap(i.path)).reduce(_++_))) withDefaultValue StyleDeclarationSet.empty

      val dynamic = provider.dynamicDocuments map (i => templateMap((Dynamic,i.path)))
      val static = provider.staticDocuments map StaticDocument
      val additionalContent: Seq[AdditionalContent] = dynamic ++ static

      val treeConfig = provider.configDocuments.find(_.path.name == "directory.conf").map(i => treeConfigMap(i.path))
      val rootConfig = if (root) Seq(mergedBundle.baseConfig) else Nil
      val config = (treeConfig.toList ++ rootConfig) reduceLeftOption (_ withFallback _) getOrElse ConfigFactory.empty

      DocumentTree(provider.path, docs ++ trees, templates, styles, additionalContent, config, sourcePaths = provider.sourcePaths)
    }

    val tree = collectDocuments(inputTree, root = true)

    if (rewrite) tree.rewrite(config.rewriteRule)
    else tree
  }

  private object parserLookup {

    private def suffix (name: String): String = name.lastIndexOf(".") match {
      case -1    => ""
      case index => name.drop(index+1)
    }

    private lazy val map: Map[String, Input => Document] =
      parsers flatMap (p => p.fileSuffixes map ((_, p.newParser(mergedBundle.parserDefinitions)))) toMap

    def forInput (input: Input): Input => Document = {
      if (parsers.size == 1) map.head._2
      else map.getOrElse(suffix(input.name),
        throw new IllegalArgumentException("Unable to determine parser based on input name: ${input.name}"))
    }

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
  def as (factory: ParserFactory): Parse = new Parse(
    Seq(factory),
    OperationConfig.default.withBundles(factory.extensions),
    rewrite = true
  )

}
