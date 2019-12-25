package laika.io.helper

import laika.ast.{Element, ElementContainer, Path}
import laika.io

object OutputBuilder {

  
  /* translating render results to Elements gives us a nicely formatted AST for free */
  case class RenderedDocumentView (path: Path, content: String) extends Element
  
  trait TreeContentView extends Element
  
  case class TitleDocument (doc: RenderedDocumentView) extends TreeContentView
  case class DocumentViews (content: Seq[RenderedDocumentView]) extends ElementContainer[RenderedDocumentView] with TreeContentView
  case class SubtreeViews (content: Seq[RenderedTreeView]) extends ElementContainer[RenderedTreeView] with TreeContentView
  
  case class RenderedTreeView (path: Path, content: Seq[TreeContentView]) extends ElementContainer[TreeContentView]
  case class RenderedTreeViewRoot (tree: RenderedTreeView, coverDocument: Option[RenderedDocumentView] = None, staticDocuments: Seq[Path] = Nil)
  
  
  object RenderedTreeView {

    def toTreeView[F[_]] (root: io.model.RenderedTreeRoot[F]) : RenderedTreeViewRoot = 
      RenderedTreeViewRoot(
        toTreeView(root.tree), 
        root.coverDocument.map(doc => RenderedDocumentView(doc.path, doc.content)), 
        root.staticDocuments.map(_.path)
      )
    
    def toTreeView (tree: io.model.RenderedTree) : RenderedTreeView = {
      val titleDocument = tree.titleDocument.map(doc => TitleDocument(RenderedDocumentView(doc.path, doc.content))).toSeq
      val content = List(
        DocumentViews(tree.content.collect { case doc: io.model.RenderedDocument => RenderedDocumentView(doc.path, doc.content) }),
        SubtreeViews(tree.content.collect { case tree: io.model.RenderedTree => toTreeView(tree) })
      ) filterNot { _.content.isEmpty }
      
      new RenderedTreeView(tree.path, titleDocument ++ content)
    }
    
  }

}
