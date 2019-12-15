package laika.factory

import laika.ast.DocumentTreeRoot

/** Render format based on a render phase for an interim result and a post processor.
  * 
 *  Examples for such a format are PDF (with XSL-FO as the interim format)
 *  or EPUB (with XHTML as the interim format).
 *  
 *  @author Jens Halm
 */
trait TwoPhaseRenderFormat[FMT, PP] {

  /** The render format for the interim result, the first phase of this renderer.
   */
  def interimFormat: RenderFormat[FMT]

  /** A hook that allows this processor to modify or enhance the given
    * document tree. A common use case is to add navigation information
    * for e-book formats like EPUB or PDF for example.
    * 
    * The modified tree produced by this method will be used for
    * rendering the interim format specified with the `format` property.
    */
  def prepareTree (tree: DocumentTreeRoot): Either[Throwable, DocumentTreeRoot]

  /** Post processor that produces the final result based on the interim format.
   */
  def postProcessor: PP
  
}
