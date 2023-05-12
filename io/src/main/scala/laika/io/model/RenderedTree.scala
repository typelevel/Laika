/*
 * Copyright 2012-2020 the original author or authors.
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

package laika.io.model

import laika.ast._
import laika.config.Config
import laika.rewrite.OutputContext
import laika.rewrite.nav.{ PathTranslator, TargetFormats }

/** A titled, positional element in the tree of rendered documents.
  */
sealed trait RenderContent extends Navigatable {

  def title: Option[SpanSequence]

  /** Creates the navigation structure for this instance up to the specified depth.
    * The returned instance can be used as part of a bigger navigation structure comprising of trees, documents and their sections.
    *
    * @param context captures the navigation depth, reference path and styles for the navigation tree being built
    * @return a navigation item that can be used as part of a bigger navigation structure comprising of trees, documents and their sections
    */
  def asNavigationItem(
      context: NavigationBuilderContext = NavigationBuilderContext()
  ): NavigationItem

}

/** Represents a node of the tree of rendered documents.
  *
  * @param path the full, absolute path of this (virtual) document tree
  * @param title the title of this tree, either obtained from the title document or configuration
  * @param content the rendered documents and subtrees in a recursive structure
  * @param titleDocument the optional title document of this tree
  */
case class RenderedTree(
    path: Path,
    title: Option[SpanSequence],
    content: Seq[RenderContent],
    titleDocument: Option[RenderedDocument] = None
) extends RenderContent {

  /** All documents contained in this tree, fetched recursively, depth-first.
    */
  lazy val allDocuments: Seq[RenderedDocument] = {

    def collect(tree: RenderedTree): Seq[RenderedDocument] =
      tree.titleDocument.toSeq ++ tree.content.flatMap {
        case doc: RenderedDocument => Seq(doc)
        case sub: RenderedTree     => collect(sub)
      }

    collect(this)
  }

  /** Indicates whether this tree does not contain any markup document.
    * Template documents do not count, as they would be ignored in rendering
    * when there is no markup document.
    */
  lazy val isEmpty: Boolean = {

    def nonEmpty(tree: RenderedTree): Boolean = tree.titleDocument.nonEmpty || tree.content.exists {
      case _: RenderedDocument => true
      case sub: RenderedTree   => nonEmpty(sub)
    }

    !nonEmpty(this)
  }

  def asNavigationItem(
      context: NavigationBuilderContext = NavigationBuilderContext()
  ): NavigationItem = {
    def hasLinks(item: NavigationItem): Boolean =
      item.link.nonEmpty || item.content.exists(hasLinks)
    val children                                =
      if (context.isComplete) Nil
      else content.map(_.asNavigationItem(context.nextLevel)).filter(hasLinks)
    val navTitle                                = title.getOrElse(SpanSequence(path.name))
    context.newNavigationItem(navTitle, titleDocument, children, TargetFormats.All)
  }

}

/** A single rendered document with the content as a plain string in the target format.
  *
  * The title and section info are still represented as an AST, so they be used in any subsequent
  * step that needs to produce navigation structures.
  */
case class RenderedDocument(
    path: Path,
    title: Option[SpanSequence],
    sections: Seq[SectionInfo],
    content: String,
    config: Config
) extends RenderContent with DocumentNavigation {
  val targetFormats: TargetFormats = TargetFormats.All
}

/** Represents the root of a tree of rendered documents. In addition to the recursive structure of documents
  * it holds additional items like static or cover documents, which may contribute to the output of a site or an e-book.
  *
  * @param tree the recursive structure of documents, usually obtained from parsing text markup
  * @param defaultTemplate the default template configured for the output format, which may be used by a post-processor
  * @param config the root configuration of the rendered tree
  * @param outputContext the context for the output format used in rendering (file suffix and format selector)
  * @param pathTranslator the path translator specific to the output format produced by the renderer
  * @param coverDocument the cover document (usually used with e-book formats like EPUB and PDF)
  * @param staticDocuments the paths of documents that were neither identified as text markup, config or templates,
  *                        and will potentially be embedded or copied as is to the final output, depending on the output format
  */
case class RenderedTreeRoot[F[_]](
    tree: RenderedTree,
    defaultTemplate: TemplateRoot,
    config: Config,
    outputContext: OutputContext,
    pathTranslator: PathTranslator,
    styles: StyleDeclarationSet = StyleDeclarationSet.empty,
    coverDocument: Option[RenderedDocument] = None,
    staticDocuments: Seq[BinaryInput[F]] = Nil
) {

  /** The title of the tree, either obtained from the title document or configuration
    */
  val title: Option[SpanSequence] = tree.title

  /** The optional title document of the tree.
    */
  val titleDocument: Option[RenderedDocument] = tree.titleDocument

  /** All documents contained in this tree, fetched recursively, depth-first.
    */
  lazy val allDocuments: Seq[RenderedDocument] = coverDocument.toSeq ++ tree.allDocuments
}
