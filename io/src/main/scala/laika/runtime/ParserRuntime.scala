package laika.runtime

import cats.data.{NonEmptyList, ValidatedNel}
import cats.effect.Async
import com.typesafe.config.ConfigFactory
import laika.ast.{TemplateDocument, _}
import laika.bundle.ConfigProvider
import laika.io.Parallel.ParallelParser
import laika.io.Sequential.SequentialParser
import laika.io.{DirectoryInput, InputCollection, ParsedTree, TextInput}
import laika.parse.markup.DocumentParser
import laika.parse.markup.DocumentParser.ParserInput
import com.typesafe.config.{Config => TConfig}
import cats.implicits._
import laika.api.MarkupParser
import laika.ast.Path.Root

/** 
  *  @author Jens Halm
  */
object ParserRuntime {
  
  def run[F[_]: Async] (op: SequentialParser.Op[F]): F[Document] = {
    for {
      input       <- op.input
      parserInput <- InputRuntime.readParserInput(input)
    } yield 
      op.parser.parse(parserInput)    
  }
  
  private object interimModel {
    
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
    case class ConfigResult (path: Path, config: TConfig) extends ParserResult
    
    case class TreeResult (tree: DocumentTree) extends ParserResult {
      val path: Path = tree.path
    }
    
    case class NoMatchingParser (path: Path, suffixes: Set[String]) extends
      RuntimeException(s"No matching parser available for path: $path - supported suffixes: ${suffixes.mkString(",")}")
    
    case class ParserErrors (errors: Seq[Throwable]) extends 
      RuntimeException(s"Multiple errors during parsing: ${errors.map(_.getMessage).mkString(", ")}")
  }

  def run[F[_]: Async] (op: ParallelParser.Op[F]): F[ParsedTree] = {
    
    import DocumentType._
    import interimModel._
    import laika.collection.TransitionalCollectionOps._

    def selectParser (path: Path): ValidatedNel[Throwable, MarkupParser] = op.parsers match {
      case NonEmptyList(parser, Nil) => parser.validNel
      case multiple => op.parserMap
        .get(path.suffix)
        .toValidNel(NoMatchingParser(path, multiple.toList.flatMap(_.fileSuffixes).toSet))
    }
      
    // TODO - 0.12 - create these in Op or OperationConfig
    lazy val templateParser: Option[ParserInput => TemplateDocument] = op.config.templateParser map { rootParser =>
      DocumentParser.forTemplate(rootParser, op.config.configHeaderParser)
    }
    lazy val styleSheetParser: ParserInput => StyleDeclarationSet = DocumentParser.forStyleSheets(op.config.styleSheetParser)
    
    def parseAll(inputs: InputCollection): F[ParsedTree] = {

      def parseDocument[D] (input: TextInput, parse: ParserInput => D, result: D => ParserResult): F[ParserResult] =
        InputRuntime.readParserInput(input).map(parse.andThen(result))
      
      val createOps: Either[Throwable, Vector[F[ParserResult]]] = inputs.textInputs.toVector.map { in => in.docType match {
        case Markup             => selectParser(in.path).map(parser => Vector(parseDocument(in, parser.parse, MarkupResult)))
        case Template           => templateParser.map(parseDocument(in, _, TemplateResult)).toVector.validNel
        case StyleSheet(format) => Vector(parseDocument(in, styleSheetParser, StyleResult(_, format))).validNel
        case Config             => Vector(parseDocument(in, ConfigProvider.fromInput, ConfigResult(in.path, _))).validNel
      }}.combineAll.toEither.leftMap(es => ParserErrors(es.toList))
      
      def buildNode (path: Path, content: Seq[ParserResult]): TreeResult = {
        def isTitleDoc (doc: Document): Boolean = doc.path.basename == "title"
        val titleDoc = content.collectFirst { case MarkupResult(doc) if isTitleDoc(doc) => doc }
        val subTrees = content.collect { case TreeResult(doc) => doc }.sortBy(_.path.name)
        val treeContent = content.collect { case MarkupResult(doc) if !isTitleDoc(doc) => doc } ++ subTrees
        val templates = content.collect { case TemplateResult(doc) => doc }

        val treeConfig = content.collect { case ConfigResult(_, config) => config }
        val rootConfig = if (path == Path.Root) Seq(op.config.baseConfig) else Nil
        val fullConfig = (treeConfig.toList ++ rootConfig) reduceLeftOption (_ withFallback _) getOrElse ConfigFactory.empty

        TreeResult(DocumentTree(path, treeContent, titleDoc, templates, fullConfig))
      }

      def processResults (results: Seq[ParserResult]): ParsedTree = {
        val coverDoc = results.collectFirst {
          case MarkupResult(doc) if doc.path.parent == Root && doc.path.basename == "cover" => doc
        }
        val tree = TreeBuilder.build(results.filterNot(res => coverDoc.exists(_.path == res.path)), buildNode).tree

        val styles = results // TODO - 0.12 - move this logic
          .collect { case StyleResult(styleSet, format) => (format, styleSet) }
          .groupBy(_._1)
          .mapValuesStrict(_.map(_._2).reduce(_ ++ _))
          .withDefaultValue(StyleDeclarationSet.empty)

        val finalTree = if (op.parsers.exists(_.rewrite)) tree.rewrite(op.config.rewriteRules) else tree

        val root = DocumentTreeRoot(finalTree, coverDoc, styles, inputs.binaryInputs.map(_.path), inputs.sourcePaths)
        ParsedTree(root, inputs.binaryInputs)
      }
      
      for {
        ops     <- Async[F].fromEither(createOps)
        results <- BatchRuntime.run(ops.toVector, 1, 1) // TODO - 0.12 - add parallelism option to builder
      } yield processResults(results)
      
    }
    
    op.input flatMap {
      case col: InputCollection => Async[F].pure(col)
      case dir: DirectoryInput => DirectoryScanner.scanDirectories(dir)
    } flatMap parseAll
    
  }

}
