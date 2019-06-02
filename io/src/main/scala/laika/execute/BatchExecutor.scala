package laika.execute

import cats.effect.Async

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/** Temporary abstraction for sequential or parallel execution of tasks.
  * Later versions will support a more flexible approach, possibly based on cats-effects.
  *
  * @author Jens Halm
  */
object BatchExecutor {

  type Batch[T] = Seq[() => T]

  /** Executes the specified batch either sequentially or in parallel,
    * depending on the specified parameters.
    *
    * @param ops the operations to execute
    * @param parallelism the number of batches to be executed in parallel, use 1 for sequential
    *                    execution
    * @param parallelThreshold the minimum number of operations required for parallel execution
    * @return the result (synchronously) of the executed batch
    */
  def execute[T] (ops: Batch[T], parallelism: Int, parallelThreshold: Int): Seq[T] = {
    if (parallelism == 1 || ops.size < parallelThreshold) ops map (_.apply)
    else {
      val batches = createBatches(ops, parallelism).map(batch => Future(batch map (_.apply)))
      val result = Future.sequence(batches)
      Await.result(result, 1.minute).flatten
    }
  }

  def execute[F[_]: Async, A] (ops: Seq[F[A]], parallelism: Int, parallelThreshold: Int): F[Seq[A]] = ???

  /** Splits the specified operations into batches based on the given
    * desired parallelism.
    */
  def createBatches[T] (ops: Batch[T], parallelism: Int): Seq[Batch[T]] = {
    val mod = ops.size % parallelism
    val loSize = ops.size / parallelism
    val hiSize = loSize + 1
    val (hi,lo) = ops.splitAt(mod * hiSize)
    val hiBatch = if (mod > 0)    hi.grouped(hiSize) else Nil
    val loBatch = if (loSize > 0) lo.grouped(loSize) else Nil
    hiBatch.toSeq ++ loBatch.toSeq
  }

}
