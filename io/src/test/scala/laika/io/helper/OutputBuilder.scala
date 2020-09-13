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

package laika.io.helper

import laika.ast.{Element, ElementContainer, NoOpt, Options, Path}
import laika.io

object OutputBuilder {

  
  /* translating render results to Elements gives us a nicely formatted AST for free */
  case class RenderedDocumentView (path: Path, content: String, options: Options = NoOpt) extends Element {
    type Self = RenderedDocumentView
    def withOptions(options: Options): Self = copy(options = options)
  }
  
  trait TreeContentView extends Element
  
  case class TitleDocument (doc: RenderedDocumentView, options: Options = NoOpt) extends TreeContentView {
    type Self = TitleDocument
    def withOptions(options: Options): Self = copy(options = options)
  }
  case class DocumentViews (content: Seq[RenderedDocumentView], options: Options = NoOpt) extends ElementContainer[RenderedDocumentView] with TreeContentView {
    type Self = DocumentViews
    def withOptions(options: Options): Self = copy(options = options)
  }
  case class SubtreeViews (content: Seq[RenderedTreeView], options: Options = NoOpt) extends ElementContainer[RenderedTreeView] with TreeContentView {
    type Self = SubtreeViews
    def withOptions(options: Options): Self = copy(options = options)
  }
  case class RenderedTreeView (path: Path, content: Seq[TreeContentView], options: Options = NoOpt) extends ElementContainer[TreeContentView] {
    type Self = RenderedTreeView
    def withOptions(options: Options): Self = copy(options = options)
  }
  case class RenderedTreeViewRoot (tree: RenderedTreeView, coverDocument: Option[RenderedDocumentView] = None, staticDocuments: Seq[Path] = Nil)
  
  object RenderedTreeViewRoot {

    def apply[F[_]] (root: io.model.RenderedTreeRoot[F]) : RenderedTreeViewRoot = 
      RenderedTreeViewRoot(
        toTreeView(root.tree), 
        root.coverDocument.map(doc => RenderedDocumentView(doc.path, doc.content)), 
        root.staticDocuments.map(_.path)
      )
    
    private def toTreeView (tree: io.model.RenderedTree) : RenderedTreeView = {
      val titleDocument = tree.titleDocument.map(doc => TitleDocument(RenderedDocumentView(doc.path, doc.content))).toSeq
      val content = List(
        DocumentViews(tree.content.collect { case doc: io.model.RenderedDocument => RenderedDocumentView(doc.path, doc.content) }),
        SubtreeViews(tree.content.collect { case tree: io.model.RenderedTree => toTreeView(tree) })
      ) filterNot { _.content.isEmpty }
      
      new RenderedTreeView(tree.path, titleDocument ++ content)
    }
    
  }

}
