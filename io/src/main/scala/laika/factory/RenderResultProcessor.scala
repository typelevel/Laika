package laika.factory

import laika.ast.DocumentTreeRoot
import laika.io.{BinaryOutput, RenderedTreeRoot}

/** Post processor for the result output of a renderer.
 *  Useful for scenarios where interim formats will be generated
 *  (e.g. XSL-FO for a PDF target or XHTML for an EPUB target).
 *  
 *  @author Jens Halm
 */
trait RenderResultProcessor[FMT] {


  /** The render format for the interim result.
   */
  def format: RenderFormat[FMT]

  /** A hook that allows this processor to modify or enhance the given
    * document tree. A common use case is to add navigation information
    * for e-book formats like EPUB or PDF for example.
    * 
    * The modified tree produced by this method will be used for
    * rendering the interim format specified with the `format` property.
    */
  def prepareTree (tree: DocumentTreeRoot): DocumentTreeRoot

  /** Processes the interim render result and writes it to the specified final output.
   */
  def process (result: RenderedTreeRoot, output: BinaryOutput): Unit
  
}
