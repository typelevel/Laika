package laika.api.errors

import cats.syntax.all.*
import cats.data.{ Chain, NonEmptyChain }
import laika.api.config.ConfigError
import laika.ast.{ Document, DocumentTreeRoot, Invalid, Path }
import ConfigError.TreeErrors
import laika.config.MessageFilter
import laika.parse.Failure

sealed trait TransformationError {
  def message: String
}

sealed trait ParserError extends TransformationError

sealed trait RendererError extends TransformationError

case class InvalidConfig(error: ConfigError) extends ParserError with RendererError {
  val message: String = s"Configuration Error: ${error.message}"
}

case class InvalidInput(error: Failure) extends ParserError {
  val message: String = s"Error parsing input: ${error.message}"
}

case class InvalidElements(elements: NonEmptyChain[Invalid])
    extends ParserError {

  val message: String = {
    val formatted = elements.map(InvalidDocument.formatElement("", _)).toList.mkString
    s"One or more error nodes in result:\n$formatted".trim
  }

}

private[laika] case class InvalidDocument(
    errors: Either[NonEmptyChain[ConfigError], NonEmptyChain[Invalid]],
    path: Path
) extends RuntimeException(
      s"One or more errors processing document '$path': ${InvalidDocument.format(errors, path)}"
    )

private[laika] object InvalidDocument {

  def apply(path: Path, error: ConfigError, errors: ConfigError*): InvalidDocument =
    new InvalidDocument(Left(NonEmptyChain.fromChainPrepend(error, Chain.fromSeq(errors))), path)

  def apply(path: Path, error: Invalid, errors: Invalid*): InvalidDocument =
    new InvalidDocument(Right(NonEmptyChain.fromChainPrepend(error, Chain.fromSeq(errors))), path)

  def indent(lineContent: String): String = {
    val lines = lineContent.split('\n')
    lines.head + "\n  " + lines.last
  }

  def format(
      errors: Either[NonEmptyChain[ConfigError], NonEmptyChain[Invalid]],
      path: Path
  ): String =
    errors.fold(
      configErrors => configErrors.map(_.message).mkString_("\n"),
      invalidElems => invalidElems.map(InvalidDocument.formatElement(path)).toList.mkString
    )

  def format(doc: InvalidDocument): String = format(doc.errors, doc.path)

  private[laika] def formatElement(pathInfo: String, element: Invalid): String =
    s"""  [$pathInfo${element.source.position.line}]: ${element.message.content}
       |
       |  ${indent(element.source.position.lineContentWithCaret)}
       |
       |""".stripMargin

  private def formatElement(docPath: Path)(element: Invalid): String = {
    val pathStr = element.source.path.fold("") { srcPath =>
      if (srcPath == docPath) "" else srcPath.toString + ":"
    }
    formatElement(pathStr, element)
  }

  def from(document: Document, failOn: MessageFilter): Option[InvalidDocument] = {
    val invalidElements = document.invalidElements(failOn)
    NonEmptyChain.fromSeq(invalidElements).map(inv => InvalidDocument(Right(inv), document.path))
  }

}

private[laika] case class InvalidDocuments(documents: NonEmptyChain[InvalidDocument])
    extends RuntimeException(
      s"One or more invalid documents:\n${InvalidDocuments.format(documents)}"
    )

private[laika] object InvalidDocuments {

  def format(documents: NonEmptyChain[InvalidDocument]): String = {

    def formatDoc(doc: InvalidDocument): String =
      s"""${doc.path}
         |
         |${InvalidDocument.format(doc)}""".stripMargin

    documents.map(formatDoc).mkString_("").trim
  }

  def from(
      result: Either[TreeErrors, DocumentTreeRoot],
      failOn: MessageFilter
  ): Either[InvalidDocuments, DocumentTreeRoot] = {
    result.fold(
      errors =>
        Left(
          InvalidDocuments(
            errors.failures.map(err => InvalidDocument(Left(err.failures), err.path))
          )
        ),
      root => from(root, failOn).toLeft(root)
    )
  }

  def from(root: DocumentTreeRoot, failOn: MessageFilter): Option[InvalidDocuments] = {
    val invalidDocs = root.allDocuments
      .flatMap(InvalidDocument.from(_, failOn))
    NonEmptyChain.fromSeq(invalidDocs)
      .map(InvalidDocuments(_))
  }

}
