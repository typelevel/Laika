package laika.runtime

import cats.data.{NonEmptyList, ValidatedNel}
import cats.effect.Async
import cats.implicits._
import laika.ast._
import laika.bundle.ConfigProvider
import laika.io.model.{DirectoryInput, InputCollection, ParsedTree, TextFileInput, TextInput}
import laika.parse.markup.DocumentParser.{ParserError, ParserInput}
import laika.api.MarkupParser
import laika.api.config.Config
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
  
  private object interimModel {

    import laika.collection.TransitionalCollectionOps._
    
    sealed trait ParserResult extends Navigatable
    
    case class MarkupResult (doc: Document) extends ParserResult {
      val path: Path = doc.path
    }
    case class TemplateResult (doc: TemplateDocument)  extends ParserResult {
      val path: Path = doc.path
    }
    case class StyleResult (doc: StyleDeclarationSet, format: String) extends ParserResult {
      val path: Path = doc.paths.head
    }
    case class ConfigResult (path: Path, config: Config) extends ParserResult
    
    case class TreeResult (tree: DocumentTree) extends ParserResult {
      val path: Path = tree.path
    }

    def buildNode (baseConfig: Config)(path: Path, content: Seq[ParserResult]): TreeResult = {
      def isTitleDoc (doc: Document): Boolean = doc.path.basename == "title"
      val titleDoc = content.collectFirst { case MarkupResult(doc) if isTitleDoc(doc) => doc }
      val subTrees = content.collect { case TreeResult(doc) => doc }.sortBy(_.path.name)
      val treeContent = content.collect { case MarkupResult(doc) if !isTitleDoc(doc) => doc } ++ subTrees
      val templates = content.collect { case TemplateResult(doc) => doc }

      val treeConfig = content.collect { case ConfigResult(_, config) => config }
      val rootConfig = if (path == Path.Root) Seq(baseConfig) else Nil
      val fullConfig = (treeConfig.toList ++ rootConfig) reduceLeftOption (_ withFallback _) getOrElse Config.empty

      TreeResult(DocumentTree(path, treeContent, titleDoc, templates, fullConfig))
    }

    def buildTree (results: Seq[ParserResult], baseConfig: Config): DocumentTreeRoot = {
      val coverDoc = results.collectFirst {
        case MarkupResult(doc) if doc.path.parent == Root && doc.path.basename == "cover" => doc
      }
      val tree = TreeBuilder.build(results.filterNot(res => coverDoc.exists(_.path == res.path)), buildNode(baseConfig)).tree

      val styles = results
        .collect { case StyleResult(styleSet, format) => (format, styleSet) }
        .groupBy(_._1)
        .mapValuesStrict(_.map(_._2).reduce(_ ++ _))
        .withDefaultValue(StyleDeclarationSet.empty)

      DocumentTreeRoot(tree, coverDoc, styles)
    }
    
  }

  /** Run the specified parser operation for an entire input tree,
    * producing an AST tree.
    */
  def run[F[_]: Async: Runtime] (op: ParallelParser.Op[F]): F[ParsedTree] = {
    
    import DocumentType.{Config => ConfigType, _}
    import interimModel._

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
      
      val createOps: Either[Throwable, Vector[F[ParserResult]]] = inputs.textInputs.toVector.map { in => in.docType match {
        case Markup             => selectParser(in.path).map(parser => Vector(parseDocument(in, parser.parse, MarkupResult)))
        case Template           => op.templateParser.map(parseDocument(in, _, TemplateResult)).toVector.validNel
        case StyleSheet(format) => Vector(parseDocument(in, op.styleSheetParser, StyleResult(_, format))).validNel
        case ConfigType         => Vector(parseDocument(in, ConfigProvider.fromInput, ConfigResult(in.path, _))).validNel
      }}.combineAll.toEither.leftMap(es => ParserErrors(es.toList))
      
      def rewriteTree (root: DocumentTreeRoot): ParsedTree = {
        val finalTree = root.rewrite(op.config.rewriteRules)
        val finalRoot = finalTree.copy(staticDocuments = inputs.binaryInputs.map(_.path), sourcePaths = inputs.sourcePaths)
        ParsedTree(finalRoot, inputs.binaryInputs)
      }
      
      for {
        _       <- validateInputPaths
        ops     <- Async[F].fromEither(createOps)
        results <- implicitly[Runtime[F]].runParallel(ops)
      } yield rewriteTree(buildTree(results, op.config.baseConfig))
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
