/*
 * Copyright 2012-2020 the original author or authors.
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

package laika.parse.markup

import cats.implicits._
import cats.data.NonEmptyChain
import laika.ast._
import laika.bundle.{ConfigProvider, MarkupExtensions}
import laika.config.{ConfigError, ConfigParser}
import laika.factory.MarkupFormat
import laika.parse.combinator.Parsers
import laika.parse.{Parser, SourceCursor}
import laika.parse.implicits._

/** Responsible for creating the top level parsers for text markup and template documents,
  * by combining the parser for the root element with a parser for an (optional) configuration
  * header.
  *
  * @author Jens Halm
  */
object DocumentParser {
  
  case class DocumentInput (path: Path, source: SourceCursor)
  
  case class ParserError (message: String, path: Path) extends 
    RuntimeException(s"Error parsing document '$path': $message")
  
  object ParserError {
    
    def apply(configError: ConfigError, path: Path): ParserError =
      ParserError(s"Configuration Error: ${configError.message}", path)
    
    def apply (document: InvalidDocument): ParserError =
      ParserError(s"One or more error nodes in result:\n ${document.messages.map(_.content).mkString_("\n ")}", document.path)
  }
  
  case class InvalidDocuments (documents: NonEmptyChain[InvalidDocument]) extends 
    RuntimeException(s"One or more invalid documents:\n${InvalidDocuments.format(documents)}")

  object InvalidDocuments {
    
    def format (documents: NonEmptyChain[InvalidDocument]): String = documents
      .map(invDoc => invDoc.messages.map(_.content).mkString_(invDoc.path + "\n  ", "\n  ", ""))
      .mkString_("\n")
    
    def from (root: DocumentTreeRoot, failOn: MessageFilter): Option[InvalidDocuments] = {
      val invalidDocs = root.allDocuments
        .flatMap(InvalidDocument.from(_, failOn))
      NonEmptyChain.fromSeq(invalidDocs)
        .map(InvalidDocuments(_))
    }
    
  }
  
  case class InvalidDocument (messages: NonEmptyChain[RuntimeMessage], path: Path) extends 
    RuntimeException(s"One or more errors processing document '$path': ${messages.map(_.content).mkString_("\n ")}")
  
  object InvalidDocument {
    
    def from (document: Document, failOn: MessageFilter): Option[InvalidDocument] = {
      val invalidElements = document.runtimeMessages(failOn)
      NonEmptyChain.fromSeq(invalidElements).map(InvalidDocument(_, document.path))
    }
    
  }

  private def create [D, R <: ElementContainer[_]] (rootParser: Parser[R], configProvider: ConfigProvider)
    (docFactory: (Path, ConfigParser, R) => D): DocumentInput => Either[ParserError, D] = {

    forParser { path =>
      val configHeader = configProvider.configHeader | Parsers.success(ConfigParser.empty)
      (configHeader ~ rootParser).mapN(docFactory(path, _, _))
    }
  }

  /** Combines the specified markup parsers and extensions and the parser for (optional) configuration
    * headers to create a parser function for an entire text markup document.
    */
  def forMarkup (markupParser: MarkupFormat,
                 markupExtensions: MarkupExtensions,
                 configProvider: ConfigProvider): DocumentInput => Either[ParserError, UnresolvedDocument] = {

    val rootParser = new RootParser(markupParser, markupExtensions).rootElement

    markupExtensions.parserHooks.preProcessInput andThen
      forMarkup(rootParser, configProvider) andThen
      { _.map(markupExtensions.parserHooks.postProcessDocument) }
  }

  /** Combines the specified parsers for the root element and for (optional) configuration
    * headers to create a parser function for an entire text markup document.
    */
  def forMarkup (rootParser: Parser[RootElement], configProvider: ConfigProvider): DocumentInput => Either[ParserError, UnresolvedDocument] =
    create(rootParser, configProvider) { (path, config, root) =>
      val fragments = root.collect { case f: DocumentFragment => (f.name, f.root) }.toMap
      UnresolvedDocument(Document(path, root, fragments), config)
   }

  /** Combines the specified parsers for the root element and for (optional) configuration
    * headers to create a parser function for an entire template document.
    */
  def forTemplate (rootParser: Parser[TemplateRoot], configProvider: ConfigProvider): DocumentInput => Either[ParserError, TemplateDocument] = 
    create(rootParser, configProvider) { (path, config, root) =>
      TemplateDocument(path, root, config)
    }

  /** Builds a document parser for CSS documents based on the specified parser for style declarations.
    */
  def forStyleSheets (parser: Parser[Set[StyleDeclaration]]): DocumentInput => Either[ParserError, StyleDeclarationSet] = 
    forParser { path => parser.map(res => StyleDeclarationSet.forPath(path, res)) }

  /** A document parser function for the specified parser that is expected to consume
    * all input.
    *
    * The specified function is invoked for each parsed document, so that a parser
    * dependent on the input path can be created.
    */
  def forParser[T] (p: Path => Parser[T]): DocumentInput => Either[ParserError, T] = { in =>
    Parsers
      .consumeAll(p(in.path))
      .parse(in.source)
      .toEither
      .left.map(ParserError(_, in.path))
  }

}
