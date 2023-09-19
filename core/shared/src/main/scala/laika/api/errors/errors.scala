package laika.api.errors

import cats.syntax.all.*
import cats.data.{ Chain, NonEmptyChain }
import laika.api.config.ConfigError
import laika.ast.{ Document, DocumentTreeRoot, Invalid, Path }
import ConfigError.TreeErrors
import laika.config.MessageFilter

sealed trait TransformationError {
  def message: String
}

case class RendererError(message: String) extends TransformationError {
  override def toString: String = message
}

object RendererError {

  def apply(message: String): RendererError =
    new RendererError(s"Rendering Error: $message")

  def apply(configError: ConfigError): RendererError =
    RendererError(s"Configuration Error: ${configError.message}")

}

case class ParserError(message: String) extends TransformationError {
  override def toString: String = message
}

object ParserError {

  def apply(message: String): ParserError =
    new ParserError(s"Error parsing input: $message")

  def apply(configError: ConfigError): ParserError =
    ParserError(s"Configuration Error: ${configError.message}")

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

  def formatElement(docPath: Path)(element: Invalid): String = {
    val pathStr = element.source.path.fold("") { srcPath =>
      if (srcPath == docPath) "" else srcPath.toString + ":"
    }
    s"""  [$pathStr${element.source.position.line}]: ${element.message.content}
       |
       |  ${indent(element.source.position.lineContentWithCaret)}
       |
       |""".stripMargin
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
