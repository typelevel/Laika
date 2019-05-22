package laika.execute

import com.typesafe.config.ConfigFactory
import laika.api.Parse
import laika.ast._
import laika.bundle.ConfigProvider
import laika.config.OperationConfig
import laika.factory.MarkupFormat
import laika.io.{DirectoryInput, IO, InputCollection, TextInput}
import laika.parse.combinator.Parsers.documentParserFunction
import laika.parse.markup.DocumentParser
import laika.parse.markup.DocumentParser.ParserInput

/** 
  *  @author Jens Halm
  */
object ParseExecutor {
  
  def execute (op: Parse.Op): Document = {

    val doc = IO(op.input)(ParserLookup(op.parsers, op.config).forInput(op.input))

    if (op.rewrite) doc.rewrite(op.config.rewriteRulesFor(doc))
    else doc
  }
  
  private object interimModel {
    sealed trait ParserResult extends Navigatable
    sealed trait NavigatableResult extends ParserResult {
      def doc: Navigatable
      def path: Path = doc.path
    }
    case class MarkupResult (doc: Document) extends NavigatableResult
    case class TemplateResult (doc: TemplateDocument) extends NavigatableResult
    case class StyleResult (doc: StyleDeclarationSet, format: String) extends ParserResult {
      val path: Path = doc.paths.head
    }
    case class ConfigResult (path: Path, config: TConfig) extends ParserResult
  }

  def execute (op: Parse.TreeOp): DocumentTreeRoot = {
    
    import DocumentType._
    import interimModel._
    import laika.collection.TransitionalCollectionOps._

    type Operation = () => ParserResult
    
    def parseMarkup (input: TextInput): Operation = () => MarkupResult(IO(input)(ParserLookup(op.parsers, op.config).forInput(input)))

    def parseTemplate (input: TextInput): Seq[Operation] = op.config.templateParser match {
      case None => Seq()
      case Some(rootParser) =>
        val docParser: TextInput => TemplateDocument = input => 
          DocumentParser.forTemplate(rootParser, op.config.configHeaderParser)(InputExecutor.asParserInput(input))
        Seq(() => TemplateResult(IO(input)(docParser)))
    }

    def parseStyleSheet (format: String)(input: TextInput): Operation = () => {
      val docF: TextInput => StyleDeclarationSet = input => 
        documentParserFunction(op.config.styleSheetParser, StyleDeclarationSet.forPath)(InputExecutor.asParserInput(input))
      StyleResult(IO(input)(docF), format)
    }

    def parseTreeConfig (input: TextInput): Operation = () => {
      val pi = InputExecutor.asParserInput(input)
      ConfigResult(input.path, ConfigProvider.fromInput(pi.context.input, pi.path))
    }

    val inputs: InputCollection = op.input match {
      case col: InputCollection => col
      case dir: DirectoryInput => InputExecutor.asInputCollection(dir)
    }

    val textOps = inputs.textInputs.flatMap { in => in.docType match {
      case Markup             => Seq(parseMarkup(in))
      case Template           => parseTemplate(in)
      case StyleSheet(format) => Seq(parseStyleSheet(format)(in))
      case Config             => Seq(parseTreeConfig(in))
    }}
    
    val results = BatchExecutor.execute(textOps, op.config.parallelConfig.parallelism, op.config.parallelConfig.threshold)

    def buildNode (path: Path, content: Seq[ParserResult], subTrees: Seq[DocumentTree]): DocumentTree = {
      def isTitleDoc (doc: Document): Boolean = doc.path.basename == "title"
      val titleDoc = content.collectFirst { case MarkupResult(doc) if isTitleDoc(doc) => doc }
      val treeContent = content.collect { case MarkupResult(doc) if !isTitleDoc(doc) => doc } ++ subTrees.sortBy(_.path.name)
      val templates = content.collect { case TemplateResult(doc) => doc }
      
      val treeConfig = content.collect { case ConfigResult(_, config) => config }
      val rootConfig = if (path == Root) Seq(op.config.baseConfig) else Nil
      val fullConfig = (treeConfig.toList ++ rootConfig) reduceLeftOption (_ withFallback _) getOrElse ConfigFactory.empty

      DocumentTree(path, treeContent, titleDoc, templates, fullConfig)
    }
    
    val tree = TreeBuilder.build(results, buildNode)

    val styles = results.collect { case StyleResult(styleSet, format) => (format, styleSet) }
      .groupBy(_._1).mapValuesStrict(_.map(_._2).reduce(_ ++ _)).withDefaultValue(StyleDeclarationSet.empty)
    
    val finalTree = if (op.rewrite) tree.rewrite(op.config.rewriteRules) else tree

    DocumentTreeRoot(finalTree, None, styles, inputs.binaryInputs, op.input.sourcePaths)
  }

  private case class ParserLookup (parsers: Seq[MarkupFormat], config: OperationConfig) {

    private def suffix (name: String): String = name.lastIndexOf(".") match {
      case -1    => ""
      case index => name.drop(index+1)
    }

    private lazy val map: Map[String, ParserInput => Document] =
      parsers.flatMap { parser =>
        val docParser = DocumentParser.forMarkup(parser, config.markupExtensions, config.configHeaderParser)
        parser.fileSuffixes.map((_, docParser))
      }.toMap

    def forInput (input: TextInput): TextInput => Document = { input =>
      val pi = InputExecutor.asParserInput(input)
      if (parsers.size == 1) map.head._2(pi)
      else map.getOrElse(suffix(input.name),
        throw new IllegalArgumentException("Unable to determine parser based on input name: ${input.name}"))(pi)
    }

  }

}
