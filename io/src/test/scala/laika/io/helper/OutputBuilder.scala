package laika.io.helper

import java.io.{BufferedWriter, File, FileWriter}

import laika.ast.{Element, ElementContainer, Path}
import laika.io

import scala.annotation.tailrec
import scala.io.{Codec, Source}

object OutputBuilder {

  
  /* translating render results to Elements gives us a nicely formatted AST for free */
  case class RenderedDocumentView (path: Path, content: String) extends Element
  
  trait TreeContentView extends Element
  
  case class TitleDocument (doc: RenderedDocumentView) extends TreeContentView
  case class DocumentViews (content: Seq[RenderedDocumentView]) extends ElementContainer[RenderedDocumentView, DocumentViews] with TreeContentView
  case class SubtreeViews (content: Seq[RenderedTreeView]) extends ElementContainer[RenderedTreeView, SubtreeViews] with TreeContentView
  
  case class RenderedTreeView (path: Path, content: Seq[TreeContentView]) extends ElementContainer[TreeContentView, RenderedTreeView]
  case class RenderedTreeViewRoot (tree: RenderedTreeView, coverDocument: Option[RenderedDocumentView] = None, staticDocuments: Seq[Path] = Nil)
  
  
  object RenderedTreeView {

    def toTreeView (root: io.RenderedTreeRoot) : RenderedTreeViewRoot = 
      RenderedTreeViewRoot(
        toTreeView(root.tree), 
        root.coverDocument.map(doc => RenderedDocumentView(doc.path, doc.content)), 
        root.staticDocuments.map(_.path)
      )
    
    def toTreeView (tree: io.RenderedTree) : RenderedTreeView = {
      val titleDocument = tree.titleDocument.map(doc => TitleDocument(RenderedDocumentView(doc.path, doc.content))).toSeq
      val content = List(
        DocumentViews(tree.content.collect { case doc: io.RenderedDocument => RenderedDocumentView(doc.path, doc.content) }),
        SubtreeViews(tree.content.collect { case tree: io.RenderedTree => toTreeView(tree) })
      ) filterNot { case c: ElementContainer[_,_] => c.content.isEmpty }
      
      new RenderedTreeView(tree.path, titleDocument ++ content)
    }
    
  }

  def createTempDirectory (baseName: String): File = {
    val maxAttempts = 100
    val baseDir = new File(System.getProperty("java.io.tmpdir"))
    val name = System.currentTimeMillis.toString + "-";
    
    def abort () = throw new IllegalStateException("Failed to create directory within "
        + maxAttempts + " attempts (tried "
        + baseName + "0 to " + baseName + (maxAttempts - 1) + ')')
    
    @tailrec def createDir (num: Int): File = {
      val tempDir = new File(baseDir, name + num);
      if (tempDir.mkdir()) tempDir
      else if (num >= maxAttempts) abort()
      else createDir(num + 1)
    }
    
    createDir(1)
  }
  
  def readFile (base: String): String = readFile(new File(base))
  
  def readFile (f: File): String = readFile(f, Codec.UTF8)

  def readFile (f: File, codec: Codec): String = {
    val source = Source.fromFile(f)(codec)
    val fileContent = source.mkString
    source.close()
    fileContent
  }

  def writeFile (f: File, content: String): Unit = {
    val bw = new BufferedWriter(new FileWriter(f))
    try {
      bw.write(content)
    }
    finally {
      bw.close()
    }
  }
  
}
