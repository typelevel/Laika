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
import cats.data.{Chain, NonEmptyChain}
import laika.ast._
import laika.bundle.{ConfigProvider, MarkupExtensions}
import laika.config.{ConfigError, ConfigParser, TreeConfigErrors}
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
  
  object DocumentInput {
    def apply(path: Path, input: String): DocumentInput = new DocumentInput(path, SourceCursor(input, path))
  }

  sealed trait TransformationError
  
  case class RendererError (message: String, path: Path) extends
    RuntimeException(s"Error rendering document '$path': $message") with TransformationError

  object RendererError {
    def apply(configError: ConfigError, path: Path): RendererError =
      RendererError(s"Configuration Error: ${configError.message}", path)
  }
  
  case class ParserError (message: String, path: Path) extends 
    RuntimeException(s"Error parsing document '$path': $message") with TransformationError
  
  object ParserError {
    
    def apply(configError: ConfigError, path: Path): ParserError =
      ParserError(s"Configuration Error: ${configError.message}", path)
    
    def apply (document: InvalidDocument): ParserError = ParserError(
      s"One or more error nodes in result:\n${InvalidDocument.format(document)}".trim, 
      document.path
    )
  }
  
  case class InvalidDocuments (documents: NonEmptyChain[InvalidDocument]) extends 
    RuntimeException(s"One or more invalid documents:\n${InvalidDocuments.format(documents)}")

  object InvalidDocuments {
    
    def format (documents: NonEmptyChain[InvalidDocument]): String = {
      
      def formatDoc (doc: InvalidDocument): String =
        s"""${doc.path}
          |
          |${InvalidDocument.format(doc)}""".stripMargin
      
      documents.map(formatDoc).mkString_("").trim
    }

    def from (result: Either[TreeConfigErrors, DocumentTreeRoot], failOn: MessageFilter): Either[InvalidDocuments, DocumentTreeRoot] = {
      result.fold(
        errors => Left(InvalidDocuments(errors.failures.map(err => InvalidDocument(Left(err.failures), err.path)))),
        root   => from(root, failOn).toLeft(root)
      )
    }
    
    def from (root: DocumentTreeRoot, failOn: MessageFilter): Option[InvalidDocuments] = {
      val invalidDocs = root.allDocuments
        .flatMap(InvalidDocument.from(_, failOn))
      NonEmptyChain.fromSeq(invalidDocs)
        .map(InvalidDocuments(_))
    }
    
  }
  
  case class InvalidDocument (errors: Either[NonEmptyChain[ConfigError], NonEmptyChain[Invalid]], path: Path) extends 
    RuntimeException(s"One or more errors processing document '$path': ${InvalidDocument.format(errors, path)}")
  
  object InvalidDocument {
    
    def apply (path: Path, error: ConfigError, errors: ConfigError*): InvalidDocument =
      new InvalidDocument(Left(NonEmptyChain.fromChainPrepend(error, Chain.fromSeq(errors))), path)

    def apply (path: Path, error: Invalid, errors: Invalid*): InvalidDocument =
      new InvalidDocument(Right(NonEmptyChain.fromChainPrepend(error, Chain.fromSeq(errors))), path)

    def indent (lineContent: String): String = {
      val lines = lineContent.split('\n')
      lines.head + "\n  " + lines.last
    }

    def format (errors: Either[NonEmptyChain[ConfigError], NonEmptyChain[Invalid]], path: Path): String =
      errors.fold(
        configErrors => configErrors.map(_.message).mkString_("\n"),
        invalidElems => invalidElems.map(InvalidDocument.formatElement(path)).toList.mkString
      )
    
    def format (doc: InvalidDocument): String = format(doc.errors, doc.path)
    
    def formatElement (docPath: Path)(element: Invalid): String = {
      val pathStr = element.source.path.fold("") { srcPath =>
        if (srcPath == docPath) "" else srcPath.toString + ":"
      }
      s"""  [$pathStr${element.source.position.line}]: ${element.message.content}
         |
         |  ${indent(element.source.position.lineContentWithCaret)}
         |
         |""".stripMargin
    }

    def from (document: Document, failOn: MessageFilter): Option[InvalidDocument] = {
      val invalidElements = document.invalidElements(failOn)
      NonEmptyChain.fromSeq(invalidElements).map(inv => InvalidDocument(Right(inv), document.path))
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
