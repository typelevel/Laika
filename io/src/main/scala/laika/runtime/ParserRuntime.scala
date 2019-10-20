package laika.runtime

import cats.data.{NonEmptyList, ValidatedNel}
import cats.effect.Async
import cats.implicits._
import laika.ast._
import laika.bundle.{ConfigProvider, UnresolvedConfig}
import laika.io.model.{DirectoryInput, InputCollection, ParsedTree, TextFileInput, TextInput}
import laika.parse.markup.DocumentParser.{ParserError, ParserInput}
import laika.api.MarkupParser
import laika.config.Config
import laika.ast.Path.Root
import laika.io.text.{ParallelParser, SequentialParser}

/** Internal runtime for parser operations, for parallel and sequential execution. 
  * 
  *  @author Jens Halm
  */
object ParserRuntime {
  
  /** Run the specified parser operation for a single input,
    * producing a single document.
    */
  def run[F[_]: Async: Runtime] (op: SequentialParser.Op[F]): F[Document] = {
    for {
      input       <- op.input
      parserInput <- InputRuntime.readParserInput(input)
      res         <- Async[F].fromEither(op.parser.parse(parserInput))
    } yield res
          
  }
  
  /** Run the specified parser operation for an entire input tree,
    * producing an AST tree.
    */
  def run[F[_]: Async: Runtime] (op: ParallelParser.Op[F]): F[ParsedTree] = {
    
    import DocumentType.{Config => ConfigType, _}
    import TreeResultBuilder._

    def selectParser (path: Path): ValidatedNel[Throwable, MarkupParser] = op.parsers match {
      case NonEmptyList(parser, Nil) => parser.validNel
      case multiple => op.parserMap
        .get(path.suffix)
        .toValidNel(NoMatchingParser(path, multiple.toList.flatMap(_.fileSuffixes).toSet))
    }
      
    def parseAll(inputs: InputCollection): F[ParsedTree] = {
      
      def validateInputPaths: F[Unit] = {
        val duplicates = (inputs.binaryInputs ++ inputs.textInputs)
          .map(i => i.path -> i)
          .groupBy(_._1)
          .collect { case (path, in) if in.size > 1 => 
            DuplicatePath(path, in.collect { case (_, i: TextFileInput) => i.file.getAbsolutePath }.toSet)
          }
        if (duplicates.isEmpty) Async[F].unit
        else Async[F].raiseError(ParserErrors(duplicates.toSeq))
      }

      def parseDocument[D] (input: TextInput, parse: ParserInput => Either[ParserError, D], result: D => ParserResult): F[ParserResult] =
        InputRuntime.readParserInput(input).flatMap(in => Async[F].fromEither(parse(in).map(result)))
      
      def parseConfig(input: ParserInput): Either[ParserError, UnresolvedConfig] =
        Right(op.config.configProvider.configDocument(input.context.input))
      
      val createOps: Either[Throwable, Vector[F[ParserResult]]] = inputs.textInputs.toVector.map { in => in.docType match {
        case Markup             => selectParser(in.path).map(parser => Vector(parseDocument(in, parser.parseUnresolved, MarkupResult)))
        case Template           => op.templateParser.map(parseDocument(in, _, TemplateResult)).toVector.validNel
        case StyleSheet(format) => Vector(parseDocument(in, op.styleSheetParser, StyleResult(_, format))).validNel
        case ConfigType         => Vector(parseDocument(in, parseConfig, ConfigResult(in.path, _))).validNel
      }}.combineAll.toEither.leftMap(es => ParserErrors(es.toList))
      
      def rewriteTree (root: DocumentTreeRoot): ParsedTree = { // TODO - 0.12 - move to TreeResultBuilder
        val finalTree = root.rewrite(op.config.rewriteRules)
        val finalRoot = finalTree.copy(staticDocuments = inputs.binaryInputs.map(_.path), sourcePaths = inputs.sourcePaths)
        ParsedTree(finalRoot, inputs.binaryInputs)
      }
      
      for {
        _       <- validateInputPaths
        ops     <- Async[F].fromEither(createOps)
        results <- Runtime[F].runParallel(ops)
        tree    <- Async[F].fromEither(buildTree(results, op.config.baseConfig).leftMap(e => ParserError(e.toString, Root))) // TODO - 0.12 - ConfigError to ParserError
      } yield rewriteTree(tree)
    }
    
    op.input flatMap {
      case col: InputCollection => Async[F].pure(col)
      case dir: DirectoryInput => DirectoryScanner.scanDirectories(dir)
    } flatMap parseAll
    
  }

  case class NoMatchingParser (path: Path, suffixes: Set[String]) extends
    RuntimeException(s"No matching parser available for path: $path - supported suffixes: ${suffixes.mkString(",")}")

  case class DuplicatePath (path: Path, filePaths: Set[String] = Set.empty) extends
    RuntimeException(s"Duplicate path: $path ${filePathMessage(filePaths)}")

  case class ParserErrors (errors: Seq[Throwable]) extends
    RuntimeException(s"Multiple errors during parsing: ${errors.map(_.getMessage).mkString(", ")}")

  private def filePathMessage (filePaths: Set[String]): String =
    if (filePaths.isEmpty) "(no matching file paths)"
    else s"with matching file paths: ${filePaths.mkString(", ")}"
}
