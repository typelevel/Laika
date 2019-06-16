package laika.runtime

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
  }

  def run[F[_]: Async] (op: ParallelParser.Op[F]): F[ParsedTree] = {
    
    import DocumentType._
    import interimModel._
    import laika.collection.TransitionalCollectionOps._

    // TODO - 0.12 - create these in Op or OperationConfig
    lazy val templateParser: Option[ParserInput => TemplateDocument] = op.config.templateParser map { rootParser =>
      DocumentParser.forTemplate(rootParser, op.config.configHeaderParser)
    }
    lazy val styleSheetParser: ParserInput => StyleDeclarationSet = DocumentParser.forStyleSheets(op.config.styleSheetParser)
    
    def parseAll(inputs: InputCollection): F[ParsedTree] = {

      def parseDocument[D] (input: TextInput, parse: ParserInput => D, result: D => ParserResult): F[ParserResult] =
        InputRuntime.readParserInput(input).map(parse.andThen(result))
      
      val textOps: Seq[F[ParserResult]] = inputs.textInputs.flatMap { in => in.docType match {
        case Markup             => Seq(parseDocument(in, op.parser.parse, MarkupResult))
        case Template           => templateParser.map(parseDocument(in, _, TemplateResult)).toSeq
        case StyleSheet(format) => Seq(parseDocument(in, styleSheetParser, StyleResult(_, format)))
        case Config             => Seq(parseDocument(in, ConfigProvider.fromInput, ConfigResult(in.path, _)))
      }}
      
      def buildNode (path: Path, content: Seq[ParserResult], subTrees: Seq[DocumentTree]): DocumentTree = {
        def isTitleDoc (doc: Document): Boolean = doc.path.basename == "title"
        val titleDoc = content.collectFirst { case MarkupResult(doc) if isTitleDoc(doc) => doc }
        val treeContent = content.collect { case MarkupResult(doc) if !isTitleDoc(doc) => doc } ++ subTrees.sortBy(_.path.name)
        val templates = content.collect { case TemplateResult(doc) => doc }
        
        val treeConfig = content.collect { case ConfigResult(_, config) => config }
        val rootConfig = if (path == Path.Root) Seq(op.config.baseConfig) else Nil
        val fullConfig = (treeConfig.toList ++ rootConfig) reduceLeftOption (_ withFallback _) getOrElse ConfigFactory.empty
  
        DocumentTree(path, treeContent, titleDoc, templates, fullConfig)
      }

      BatchRuntime.run(textOps.toVector, 1, 1).map { results => // TODO - 0.12 - add parallelism option to builder
        val coverDoc = results.collectFirst { 
          case MarkupResult(doc) if doc.path.parent == Root && doc.path.basename == "cover" => doc
        }
        val tree = TreeBuilder.build(results.filterNot(res => coverDoc.exists(_.path == res.path)), buildNode) // TODO - 0.12 - use new buildComposite and remove the old builder

        val styles = results // TODO - 0.12 - move this logic
          .collect { case StyleResult(styleSet, format) => (format, styleSet) }
          .groupBy(_._1)
          .mapValuesStrict(_.map(_._2).reduce(_ ++ _))
          .withDefaultValue(StyleDeclarationSet.empty)

        val finalTree = if (op.parser.rewrite) tree.rewrite(op.config.rewriteRules) else tree

        val root = DocumentTreeRoot(finalTree, coverDoc, styles, inputs.binaryInputs.map(_.path), inputs.sourcePaths)
        ParsedTree(root, inputs.binaryInputs)
      } 
      
    }
    
    op.input flatMap {
      case col: InputCollection => Async[F].pure(col)
      case dir: DirectoryInput => DirectoryScanner.scanDirectories(dir)
    } flatMap parseAll
    
  }

//  private case class ParserLookup (parsers: Seq[MarkupFormat], config: OperationConfig) {
//
//    private def suffix (name: String): String = name.lastIndexOf(".") match {
//      case -1    => ""
//      case index => name.drop(index+1)
//    }
//
//    private lazy val map: Map[String, ParserInput => Document] =
//      parsers.flatMap { parser =>
//        val docParser = DocumentParser.forMarkup(parser, config.markupExtensions, config.configHeaderParser)
//        parser.fileSuffixes.map((_, docParser))
//      }.toMap
//
//    def forInput (input: TextInput): TextInput => Document = { input =>
//      val pi: ParserInput = null
//      if (parsers.size == 1) map.head._2(pi)
//      else map.getOrElse(suffix(input.name),
//        throw new IllegalArgumentException("Unable to determine parser based on input name: ${input.name}"))(pi)
//    }
//
//  }

}
