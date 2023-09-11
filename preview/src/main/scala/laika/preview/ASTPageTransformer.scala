/*
 * Copyright 2012-2023 the original author or authors.
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

package laika.preview

import laika.api.Renderer
import laika.api.builder.OperationConfig
import laika.ast.{
  Block,
  BlockContainer,
  BlockSequence,
  CodeBlock,
  Document,
  DocumentTreeRoot,
  Path,
  RewritePhase,
  RootElement,
  Section,
  Title
}
import laika.bundle.{ BundleOrigin, ExtensionBundle }
import laika.format.{ AST, HTML }
import laika.parse.{ Failure, Success }
import laika.parse.code.languages.LaikaASTSyntax
import laika.rewrite.OutputContext
import laika.rewrite.nav.PathTranslator

import scala.annotation.tailrec

private[preview] object ASTPageTransformer {

  object ASTPathTranslator extends ExtensionBundle {

    override val origin: BundleOrigin = BundleOrigin.Library
    val description: String           = "AST URL extension for preview server"
    private val outputName            = "ast"

    def translateASTPath(path: Path): Path = {
      val base = path.withoutFragment / outputName
      path.fragment.fold(base)(base.withFragment)
    }

    override def extendPathTranslator
        : PartialFunction[ExtensionBundle.PathTranslatorExtensionContext, PathTranslator] = {
      case context =>
        PathTranslator.postTranslate(context.baseTranslator) { path =>
          if (path.suffix.contains("html")) translateASTPath(path)
          else path
        }
    }

  }

  private val renderer = Renderer.of(AST).build

  private val highlighter = LaikaASTSyntax.rootParser

  private val messagePrefix = "Rendering of AST failed: "

  private def callout(message: String): Block =
    BlockSequence(messagePrefix + message).withStyles("callout", "error")

  private def sequenceToASTBlock(bs: BlockContainer): Block = {
    renderer.render(bs) match {
      case Left(error) => callout(error.message)
      case Right(ast)  =>
        highlighter.parse(ast) match {
          case Success(spans, _) => CodeBlock("laika-ast", spans)
          case f: Failure        => callout(f.message)
        }

    }
  }

  private def splitAtFirstSection(blocks: Seq[Block]): (Seq[Block], Seq[Block]) =
    blocks.span(b => !b.isInstanceOf[Section])

  @tailrec
  private def transformBlocks(blocks: Seq[Block], acc: Seq[Block] = Nil): Seq[Block] = {

    val (start, end) = splitAtFirstSection(blocks)

    // due to the nature of Laika's internal section builder start is expected to always be empty,
    // but there is no need to fail on unexpected content
    val noSection       =
      if (start.isEmpty) None
      else Some(sequenceToASTBlock(BlockSequence(start)))
    val (section, rest) =
      if (end.isEmpty) (None, Nil)
      else (Some(transformSection(end.head.asInstanceOf[Section])), end.tail)

    val newAcc = acc ++ noSection.toSeq ++ section.toSeq
    if (rest.isEmpty) newAcc
    else transformBlocks(rest, newAcc)
  }

  private def transformSection(section: Section): Section = {
    val (first, remaining) = splitAtFirstSection(section.content)

    val firstBlock = sequenceToASTBlock(section.withContent(first))

    section.withContent(firstBlock +: transformBlocks(remaining))
  }

  private def transformRoot(root: RootElement): RootElement = {

    val (first, remaining) = splitAtFirstSection(root.content)

    val title      = first.collectFirst { case title: Title => title }
    val firstBlock = sequenceToASTBlock(root.withContent(first))
    val rest       = firstBlock +: transformBlocks(remaining)

    root.withContent(title.toSeq ++ rest)
  }

  private def transformDocument(doc: Document): Document =
    doc.copy(content = transformRoot(doc.content))

  def transform(tree: DocumentTreeRoot, config: OperationConfig): DocumentTreeRoot = {
    val rules = config.rewriteRulesFor(tree, RewritePhase.Render(OutputContext(HTML)))
    tree.rewrite(rules) match {
      case Right(rewritten) => rewritten.modifyTree(_.mapDocuments(transformDocument))
      case Left(error) => tree.mapDocuments(_.copy(content = RootElement(callout(error.message))))
    }
  }

}
