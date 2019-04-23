/*
 * Copyright 2012-2019 the original author or authors.
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

package laika.execute

import com.typesafe.config.{ConfigFactory, Config => TConfig}
import laika.api.Parse
import laika.ast._
import laika.bundle.ConfigProvider
import laika.config.OperationConfig
import laika.factory.MarkupParser
import laika.io.{IO, Input, InputTree}
import laika.parse.combinator.Parsers.documentParserFunction
import laika.parse.markup.DocumentParser

/** 
  *  @author Jens Halm
  */
object ParseExecutor {
  
  def execute (op: Parse.Op): Document = {

    val doc = IO(op.input)(ParserLookup(op.parsers, op.config).forInput(op.input))

    if (op.rewrite) doc.rewrite(op.config.rewriteRulesFor(doc))
    else doc
  }

  def execute (op: Parse.TreeOp): DocumentTree = {
    
    import DocumentType._
    import laika.collection.TransitionalCollectionOps._

    case class TreeConfig (path: Path, config: TConfig)

    type Operation[T] = () => (DocumentType, T)

    def parseMarkup (input: Input): Operation[Document] = () => (Markup, IO(input)(ParserLookup(op.parsers, op.config).forInput(input)))

    def parseTemplate (docType: DocumentType)(input: Input): Seq[Operation[TemplateDocument]] = op.config.templateParser match {
      case None => Seq()
      case Some(rootParser) =>
        val docParser = DocumentParser.forTemplate(rootParser, op.config.configHeaderParser)
        Seq(() => (docType, IO(input)(docParser)))
    }

    def parseStyleSheet (format: String)(input: Input): Operation[StyleDeclarationSet] = () => {
      val docF = documentParserFunction(op.config.styleSheetParser, StyleDeclarationSet.forPath)
      (StyleSheet(format), IO(input)(docF))
    }

    def parseTreeConfig (input: Input): Operation[TreeConfig] = () => (Config, TreeConfig(input.path, ConfigProvider.fromInput(input.asParserInput.input, input.path)))

    def collectOperations[T] (provider: InputTree, f: InputTree => Seq[Operation[T]]): Seq[Operation[T]] =
      f(provider) ++ (provider.subtrees flatMap (collectOperations(_, f)))

    val operations = collectOperations(op.input, _.markupDocuments.map(parseMarkup)) ++
      collectOperations(op.input, _.templates.flatMap(parseTemplate(Template))) ++
      collectOperations(op.input, _.dynamicDocuments.flatMap(parseTemplate(Dynamic))) ++
      collectOperations(op.input, _.styleSheets.flatMap({ case (format,inputs) => inputs map parseStyleSheet(format) }).toSeq) ++
      collectOperations(op.input, _.configDocuments.find(_.path.name == "directory.conf").toList.map(parseTreeConfig))

    val results = BatchExecutor.execute(operations, op.config.parallelConfig.parallelism, op.config.parallelConfig.threshold)

    val docMap = (results collect {
      case (Markup, doc: Document) => (doc.path, doc)
    }) toMap

    val templateMap = (results collect {
      case (docType, doc: TemplateDocument) => ((docType, doc.path), doc)
    }) toMap

    val styleMap = (results collect {
      case (StyleSheet(_), style: StyleDeclarationSet) => (style.paths.head, style)
    }) toMap

    val treeConfigMap = (results collect {
      case (Config, config: TreeConfig) => (config.path, config.config)
    }) toMap

    def collectDocuments (provider: InputTree, root: Boolean = false): DocumentTree = {
      val docs = provider.markupDocuments map (i => docMap(i.path))
      val trees = provider.subtrees map (collectDocuments(_))

      val templates = provider.templates map (i => templateMap((Template,i.path)))
      val styles = (provider.styleSheets mapValuesStrict (_.map(i => styleMap(i.path)).reduce(_++_))) withDefaultValue StyleDeclarationSet.empty

      val dynamic = provider.dynamicDocuments map (i => templateMap((Dynamic,i.path)))
      val static = provider.staticDocuments map StaticDocument
      val additionalContent: Seq[AdditionalContent] = dynamic ++ static

      val treeConfig = provider.configDocuments.find(_.path.name == "directory.conf").map(i => treeConfigMap(i.path))
      val rootConfig = if (root) Seq(op.config.baseConfig) else Nil
      val fullConfig = (treeConfig.toList ++ rootConfig) reduceLeftOption (_ withFallback _) getOrElse ConfigFactory.empty

      DocumentTree(provider.path, docs ++ trees, templates, styles, additionalContent, fullConfig, sourcePaths = provider.sourcePaths)
    }

    val tree = collectDocuments(op.input, root = true)

    if (op.rewrite) tree.rewrite(op.config.rewriteRules)
    else tree
  }

  private case class ParserLookup (parsers: Seq[MarkupParser], config: OperationConfig) {

    private def suffix (name: String): String = name.lastIndexOf(".") match {
      case -1    => ""
      case index => name.drop(index+1)
    }

    private lazy val map: Map[String, Input => Document] =
      parsers.flatMap { parser =>
        val docParser = DocumentParser.forMarkup(parser, config.markupExtensions, config.configHeaderParser)
        parser.fileSuffixes.map((_, docParser))
      }.toMap

    def forInput (input: Input): Input => Document = {
      if (parsers.size == 1) map.head._2
      else map.getOrElse(suffix(input.name),
        throw new IllegalArgumentException("Unable to determine parser based on input name: ${input.name}"))
    }

  }

}
