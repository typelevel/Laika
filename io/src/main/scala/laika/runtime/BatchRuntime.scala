package laika.runtime

import cats.effect.Async
import cats.implicits._

/** Abstraction for sequential or parallel execution of tasks.
  *
  * @author Jens Halm
  */
object BatchRuntime {

  /** Runs the specified batch either sequentially or in parallel,
    * depending on the specified parameters.
    *
    * @param ops the operations to execute
    * @param parallelism the number of batches to be executed in parallel, use 1 for sequential
    *                    execution
    * @param parallelThreshold the minimum number of operations required for parallel execution
    * @return the result of the executed batch
    */
  def run[F[_]: Async, A] (ops: Vector[F[A]], parallelism: Int, parallelThreshold: Int): F[Vector[A]] = { // TODO - 0.12 - do not fail-fast
    if (parallelism == 1 || ops.size < parallelThreshold) ops.sequence
    else {
      //createBatches(ops, parallelism).parSequence.map(_.flatten)
      ops.sequence
    }
  }

  /** Splits the specified operations into batches based on the given
    * desired parallelism.
    */
  def createBatches[F[_]: Async, A] (ops: Vector[F[A]], parallelism: Int): Vector[F[Vector[A]]] = {
    val mod = ops.size % parallelism
    val loSize = ops.size / parallelism
    val hiSize = loSize + 1
    val (hi,lo) = ops.splitAt(mod * hiSize)
    val hiBatch = if (mod > 0)    hi.grouped(hiSize) else Vector()
    val loBatch = if (loSize > 0) lo.grouped(loSize) else Vector()
    hiBatch.toVector.map(_.sequence) ++ loBatch.toVector.map(_.sequence)
  }

}
