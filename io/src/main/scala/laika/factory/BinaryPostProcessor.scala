package laika.factory

import cats.effect.Async
import laika.io.model.{BinaryOutput, RenderedTreeRoot}
import laika.io.runtime.Runtime

/** Post processor for the result output of a renderer.
 *  Useful for scenarios where interim formats will be generated
 *  (e.g. XSL-FO for a PDF target or XHTML for an EPUB target).
 *  
 *  @author Jens Halm
 */
trait BinaryPostProcessor {

  /** Processes the interim render result and writes it to the specified final output.
   */
  def process[F[_]: Async: Runtime] (result: RenderedTreeRoot[F], output: BinaryOutput[F]): F[Unit]
  
}
