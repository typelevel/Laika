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

import java.io.File

import laika.api.Render
import laika.api.Render.Done
import laika.ast.Path.Root
import laika.ast._
import laika.factory.RenderContext
import laika.io._
import laika.rewrite.TemplateRewriter

import scala.collection.mutable

/**
  *  @author Jens Halm
  */
object RenderExecutor {

  def execute[FMT] (op: Render.Op[FMT], styles: Option[StyleDeclarationSet]): String = {

    val theme = op.config.themeFor(op.format)

    val effectiveStyles = styles.getOrElse(theme.defaultStyles)

    val renderFunction: (FMT, Element) => String = (fmt, element) => 
      theme.customRenderer.applyOrElse[(FMT,Element),String]((fmt, element), { case (f, e) => op.format.defaultRenderer(f, e) })
    
    val renderContext = RenderContext(renderFunction, op.element, effectiveStyles, op.output.path, op.config)

    val fmt = op.format.formatterFactory(renderContext)
    
    val result = renderFunction(fmt, op.element)

    result // TODO - 0.12 - deal with different types of Output
  }
  
  def execute[FMT] (op: Render.MergeOp[FMT]): Done = {
    val template = op.config.themeFor(op.processor.format).defaultTemplateOrFallback // TODO - 0.12 - look for templates in root tree
    val preparedTree = op.processor.prepareTree(op.tree)
    val renderedTree  = execute(Render.TreeOp(op.processor.format, op.config, preparedTree, StringTreeOutput))
    op.processor.process(renderedTree, op.output)
    Done
  }

  def execute[FMT] (op: Render.TreeOp[FMT]): RenderedTreeRoot = {

    type Operation = () => RenderContent

    val theme = op.config.themeFor(op.format)
    val styles = theme.defaultStyles ++ op.tree.styles(op.format.fileSuffix)
    
    def outputPath (path: Path): Path = path.withSuffix(op.format.fileSuffix)
    
    def textOutputFor (path: Path): TextOutput = op.output match {
      case StringTreeOutput => StringOutput(new mutable.StringBuilder, outputPath(path)) // TODO - 0.12 - temporary solution
      case DirectoryOutput(dir, codec) => TextFileOutput(new File(dir, outputPath(path).toString.drop(1)), outputPath(path), codec)
    }
    def binaryOutputFor (path: Path): Seq[BinaryOutput] = op.output match {
      case StringTreeOutput => Nil
      case DirectoryOutput(dir, codec) => Seq(BinaryFileOutput(new File(dir, path.toString.drop(1)), path))
    }

    def renderDocument (document: Document): Operation = {
      val textOp = Render.Op(op.format, op.config, document.content, textOutputFor(document.path))
      () => RenderedDocument(outputPath(document.path), document.title, document.sections, execute(textOp, Some(styles)))
    }

//    def copy (document: BinaryInput): Seq[Operation] = binaryOutputFor(document.path).map { out =>
//      () => {
//        IO.copy(document, out)
//        CopiedDocument(document)
//      }
//    }

      // TODO - 0.12 - resurrect check for output tree
//      def isOutputRoot (source: DocumentTree) = (source.sourcePaths.headOption, op.output) match {
//        case (Some(inPath), out: DirectoryOutput) => inPath == out.directory.getAbsolutePath
//        case _ => false
//      }
      
    val templateName = "default.template." + op.format.fileSuffix // TODO - 0.12 - add to API: getDefaultTemplate(format) + withDefaultTemplate(format)
    val (treeWithTpl, template) = op.tree.tree.selectTemplate(Path.Current / templateName).fold(
      (op.tree.tree.copy(templates = op.tree.tree.templates :+ TemplateDocument(Path.Root / templateName,
        theme.defaultTemplateOrFallback)), theme.defaultTemplateOrFallback)
    )(tpl => (op.tree.tree, tpl.content))
    val rewrittenTree = TemplateRewriter.applyTemplates(treeWithTpl, op.format.fileSuffix)
    
    val finalTree = theme.staticDocuments.merge(rewrittenTree)
    val operations = op.tree.copy(tree = finalTree).allDocuments.map(renderDocument) /* ++ op.tree.staticDocuments.flatMap(copy) */  // TODO - 0.12 - handle static docs

    val results = BatchExecutor.execute(operations, op.config.parallelConfig.parallelism, op.config.parallelConfig.threshold)
    
    def buildNode (path: Path, content: Seq[RenderContent], subTrees: Seq[RenderedTree]): RenderedTree = 
      RenderedTree(path, finalTree.selectSubtree(path.relativeTo(Root)).fold(Seq.empty[Span])(_.title), content ++ subTrees) // TODO - 0.12 - handle title document
    
    val resultRoot = TreeBuilder.build(results, buildNode)

    RenderedTreeRoot(resultRoot, template, finalTree.config) // TODO - 0.12 - handle cover document
  }

}
