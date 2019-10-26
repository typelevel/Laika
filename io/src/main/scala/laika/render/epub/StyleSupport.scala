package laika.render.epub

import laika.ast.{DocumentTreeRoot, Path}
import laika.io.model.RenderedTreeRoot

/** Processes CSS inputs for EPUB containers.
  *
  * @author Jens Halm
  */
object StyleSupport {

  /** Path for the fallback styles that will be inserted
    * when the user has not added any CSS documents to the input tree.
    */
  val fallbackStylePath: Path = Path.Root / "styles" / "fallback.css"

  /** Collects all CSS inputs (recursively) in the provided document tree.
    * CSS inputs are recognized by file suffix).
    */
  def collectStylePaths[F[_]] (root: RenderedTreeRoot[F]): Seq[Path] = 
    root.staticDocuments.collect { case doc if doc.path.suffix == "css" => doc.path }

  /** Verifies that the specified document tree contains at least one CSS file
    * (determined by file suffix). If this is the case the tree is returned unchanged,
    * otherwise a new tree with a minimal fallback CSS inserted into the root is returned instead.
    */
  def ensureContainsStyles (root: DocumentTreeRoot): DocumentTreeRoot = {
    if (root.staticDocuments.exists(_.suffix == "css")) root
    else root.copy(staticDocuments = root.staticDocuments :+ fallbackStylePath)
  }

}
