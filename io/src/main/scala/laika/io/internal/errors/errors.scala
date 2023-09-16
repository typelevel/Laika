package laika.io.internal.errors

import laika.api.config.ConfigError
import laika.api.errors.InvalidDocument
import laika.ast.Path
import laika.io.model.FilePath

/** A ConfigError as a RuntimeException for use cases where a Throwable is required. */
private[laika] case class ConfigException(error: ConfigError) extends RuntimeException(
      error.message
    )

private[laika] case class NoMatchingParser(path: Path, suffixes: Set[String])
    extends RuntimeException(
      s"No matching parser available for path: $path - supported suffixes: ${suffixes.mkString(",")}"
    )

private[laika] case class DuplicatePath(path: Path, filePaths: Set[String] = Set.empty)
    extends RuntimeException(
      s"Duplicate path: $path ${DuplicatePath.filePathMessage(filePaths)}"
    )

private[laika] object DuplicatePath {

  private[errors] def filePathMessage(filePaths: Set[String]): String =
    if (filePaths.isEmpty) "(no matching file paths)"
    else s"with matching file paths: ${filePaths.mkString(", ")}"

}

private[laika] case class MissingDirectory(path: FilePath) extends RuntimeException(
      s"Path does not exist or is not a directory: ${path.toString}"
    )

private[laika] case class ParserErrors(errors: Set[Throwable]) extends RuntimeException(
      s"Multiple errors during parsing: ${errors.map(_.getMessage).mkString(", ")}"
    )

private[laika] case class RendererErrors(errors: Seq[Throwable]) extends RuntimeException(
      s"Multiple errors during rendering: ${errors.map(_.getMessage).mkString(", ")}"
    )

private[laika] case class DocumentRendererError(message: String, path: Path)
    extends RuntimeException(s"Error rendering document '$path': $message")

private[laika] object DocumentRendererError {

  def apply(configError: ConfigError, path: Path): DocumentRendererError =
    DocumentRendererError(s"Configuration Error: ${configError.message}", path)

}

private[laika] case class DocumentParserError(message: String, path: Path)
    extends RuntimeException(s"Error parsing document '$path': $message")

private[laika] object DocumentParserError {

  def apply(configError: ConfigError, path: Path): DocumentParserError =
    DocumentParserError(s"Configuration Error: ${configError.message}", path)

  def apply(document: InvalidDocument): DocumentParserError = DocumentParserError(
    s"One or more error nodes in result:\n${InvalidDocument.format(document)}".trim,
    document.path
  )

}
