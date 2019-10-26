package laika.io.runtime

import cats.Applicative
import cats.implicits._

/** Utility for explicitly producing a batch of operations as an optimization step
  * over relying solely on the thread pool.
  *
  * @author Jens Halm
  */
object BatchRuntime {

  /** Splits the specified operations into batches based on the given
    * desired parallelism.
    */
  def createBatches[F[_]: Applicative, A] (ops: Vector[F[A]], parallelism: Int): Vector[F[Vector[A]]] = {
    val mod = ops.size % parallelism
    val loSize = ops.size / parallelism
    val hiSize = loSize + 1
    val (hi,lo) = ops.splitAt(mod * hiSize)
    val hiBatch = if (mod > 0)    hi.grouped(hiSize) else Vector()
    val loBatch = if (loSize > 0) lo.grouped(loSize) else Vector()
    hiBatch.toVector.map(_.sequence) ++ loBatch.toVector.map(_.sequence)
  }

}
