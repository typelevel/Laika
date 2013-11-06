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
import laika.factory.ParserFactory

/** 
 *  @author Jens Halm
 */
trait InputProvider {

  
  lazy val name: String = path.name
  
  def path: Path
  
  def configDocuments: Seq[Input]

  def markupDocuments: Seq[Input]

  def dynamicDocuments: Seq[Input]

  def staticDocuments: Seq[Input]

  def templates: Seq[Input]
  
  def subtrees: Seq[InputProvider]
  
  
}


object InputProvider {
  
  private class DirectoryInputProvider (dir: File, val path: Path, docTypeMatcher: Path => DocumentType, codec: Codec) extends InputProvider {
    
    private def docType (f: File) = docTypeMatcher(path / f.getName)

    private def toInput (pairs: Array[(DocumentType,File)]) = pairs.map(p => Input.fromFile(p._2, path)(codec)).toList

    private lazy val files = dir.listFiles filter (_.isFile) map (f => (docType(f), f)) groupBy (_._1)
    
    private def documents (docType: DocumentType) = files.get(docType).map(toInput).getOrElse(Nil)
    
    // TODO - stream creation could be lazy
    
    lazy val configDocuments = documents(Config)
    
    lazy val markupDocuments = documents(Markup)
    
    lazy val dynamicDocuments = documents(Dynamic)
    
    lazy val staticDocuments = documents(Static)
    
    lazy val templates =  documents(Template)
    
    lazy val subtrees = {
      dir.listFiles filter (f => f.isDirectory && docType(f) != Ignored) map (d => new DirectoryInputProvider(d, path / d.getName, docTypeMatcher, codec)) toList
    }
    
  }
  
  def forRootDirectory (root: File, docTypeMatcher: Path => DocumentType)(implicit codec: Codec): InputProvider = {
    require(root.exists, "Directory "+root.getAbsolutePath()+" does not exist")
    require(root.isDirectory, "File "+root.getAbsolutePath()+" is not a directoy")
    
    new DirectoryInputProvider(root, Root, docTypeMatcher, codec)
  }
  
  
  case class InputConfig (provider: InputProvider, config: Seq[Input], templateParser: ParseTemplate, parallel: Boolean)
  
  trait ProviderBuilder {
    def build (docTypeMatcher: Path => DocumentType, codec: Codec): InputProvider
  }
  
  private[InputProvider] class DirectoryProviderBuilder (root: File) extends ProviderBuilder {
    def build (docTypeMatcher: Path => DocumentType, codec: Codec) = 
      InputProvider.forRootDirectory(root, docTypeMatcher)(codec)
  }
  
  class InputConfigBuilder private[InputProvider] (
      provider: ProviderBuilder,
      codec: Codec,
      docTypeMatcher: Option[Path => DocumentType] = None,
      templateParser: Option[ParseTemplate] = None,
      config: List[Input] = Nil,
      isParallel: Boolean = false) {
    
    def withTemplates (parser: ParseTemplate) = 
      new InputConfigBuilder(provider, codec, docTypeMatcher, Some(parser), config, isParallel)
    
    def withDocTypeMatcher (matcher: Path => DocumentType) =
      new InputConfigBuilder(provider, codec, Some(matcher), templateParser, config, isParallel)

    def withConfigFile (file: File) = withConfigInput(Input.fromFile(file)(codec))
    def withConfigFile (name: String) = withConfigInput(Input.fromFile(name)(codec))
    def withConfigString (source: String) = withConfigInput(Input.fromString(source))
    private def withConfigInput (input: Input) = 
      new InputConfigBuilder(provider, codec, docTypeMatcher, templateParser, input :: config, isParallel)
    
    def parallel = new InputConfigBuilder(provider, codec, docTypeMatcher, templateParser, config, true) // TODO - 2.10 - allow for custom TaskSupport when dropping 2.9 support
    
    def build (parser: ParserFactory) = {
      val matcher = docTypeMatcher getOrElse new DefaultDocumentTypeMatcher(parser.fileSuffixes, Seq("*.svn","*.git"))
      val templates = templateParser getOrElse ParseTemplate
      InputConfig(provider.build(matcher, codec), config, templates, isParallel)
    }
  }
  
  object Directory {
    def apply (name: String)(implicit codec: Codec) = new InputConfigBuilder(new DirectoryProviderBuilder(new File(name)), codec)
    def apply (file: File)(implicit codec: Codec) = new InputConfigBuilder(new DirectoryProviderBuilder(file), codec)
  }
  
  object DefaultDirectory {
    def apply (implicit codec: Codec) = Directory(System.getProperty("user.dir"))(codec)
  }
  
}