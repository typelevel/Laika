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

import laika.api.Render
import laika.api.Render.Done
import laika.ast._
import laika.io.OutputTree.DirectoryOutputTree
import laika.io.{BinaryInput, IO, Output, OutputTree}
import laika.rewrite.TemplateRewriter

/**
  *  @author Jens Halm
  */
object RenderExecutor {

  def execute[Writer] (op: Render.Op[Writer], styles: Option[StyleDeclarationSet] = None): Done = {

    val theme = op.config.themeFor(op.format)
    
    val effectiveStyles = styles.getOrElse(theme.defaultStyles)
    
    class Renderer (out: Output) extends (Element => Unit) {
      lazy val (writer, renderF) = op.format.newRenderer(out, op.element, this, effectiveStyles, op.config)

      lazy val mainF: Element => Unit = theme.customRenderer(writer).applyOrElse(_, renderF)

      def apply (element: Element) = mainF(element)
    }

    IO(op.output) { out =>
      new Renderer(out).apply(op.element)
      out.flush()
    }
    
    Done
  }

  def execute[Writer] (op: Render.BinaryOp[Writer]): Done = {
    val template = op.config.themeFor(op.processor.format).defaultTemplateOrFallback
    val renderOp: (DocumentTree, OutputTree) => Unit = (tree, out) => {
      execute(Render.TreeOp(op.processor.format, op.config, tree, out))
    }
    op.processor.process(op.tree, renderOp, template, op.output.asBinaryOutput)
    
    Done
  }

  def execute[Writer] (op: Render.TreeOp[Writer]): Done = {

    type Operation = () => Unit

    val theme = op.config.themeFor(op.format)

    def renderTree (outputTree: OutputTree, styles: StyleDeclarationSet, path: Path, content: RootElement): Operation = {
      val output = outputTree.newOutput(path.basename +"."+ op.format.fileSuffix)
      () => execute(Render.Op(op.format, op.config, content, output), Some(styles))
    }

    def copy (outputTree: OutputTree)(input: BinaryInput): Operation = {
      val output = outputTree.newOutput(input.path.name)
      () => IO.copy(input, output)
    }

    def collectOperations (outputTree: OutputTree, parentStyles: StyleDeclarationSet, docTree: DocumentTree): Seq[Operation] = {

      def isOutputRoot (source: DocumentTree) = (source.sourcePaths.headOption, outputTree) match {
        case (Some(inPath), out: DirectoryOutputTree) => inPath == out.directory.getAbsolutePath
        case _ => false
      }

      val styles = parentStyles ++ docTree.styles(op.format.fileSuffix)

      (docTree.content flatMap {
        case doc: Document => Seq(renderTree(outputTree, styles, doc.path, doc.content))
        case tree: DocumentTree if !isOutputRoot(tree) => collectOperations(outputTree.newChild(tree.name), styles, tree)
        case _ => Seq()
      }) ++
        (docTree.additionalContent flatMap {
          case doc: DynamicDocument => Seq(renderTree(outputTree, styles, doc.path, doc.content))
          case static: StaticDocument if outputTree.acceptsStaticFiles => Seq(copy(outputTree)(static.input))
          case _ => Seq()
        })
    }

    val templateName = "default.template." + op.format.fileSuffix
    val treeWithTpl = if (op.tree.selectTemplate(Path.Current / templateName).isDefined) op.tree
    else op.tree.copy(templates = op.tree.templates :+ TemplateDocument(Path.Root / templateName,
      theme.defaultTemplateOrFallback))
    val treeWithTplApplied = TemplateRewriter.applyTemplates(treeWithTpl, op.format.fileSuffix)
    val finalTree = theme.staticDocuments.merge(treeWithTplApplied)
    val operations = collectOperations(op.output, theme.defaultStyles, finalTree)

    BatchExecutor.execute(operations, op.config.parallelConfig.parallelism, op.config.parallelConfig.threshold)
    
    Done
  }
  
}
