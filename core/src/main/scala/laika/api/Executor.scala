/*
 * Copyright 2013-2018 the original author or authors.
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

package laika.api

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


/** Temporary abstraction for sequential or parallel execution of tasks.
  * Later versions will support a more flexible approach, possibly based on cats-effects.
  *
  * @author Jens Halm
  */
private[api] object Executor {

  type Batch[T] = Seq[() => T]

  /** The default level of parallelism, corresponding to the number
    * of CPUs.
    */
  lazy val defaultParallelism = Runtime.getRuntime.availableProcessors

  /** The default minimum number of operations for parallel execution.
    */
  lazy val defaultParallelThreshold = Math.max(2, defaultParallelism / 2)

  /** Executes the specified batch either sequentially or in parallel,
    * depending on the boolean flag provided.
    */
  def execute[T] (ops: Batch[T], parallel: Boolean): Seq[T] =
    if (parallel) execute(ops, defaultParallelism, defaultParallelThreshold)
    else execute(ops, 1, Int.MaxValue)

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

  /** Splits the specified operations into batches based on the given
    * desired parallelism.
    */
  def createBatches[T] (ops: Seq[() => T], parallelism: Int): Seq[Batch[T]] = {
    val mod = ops.size % parallelism
    val loSize = ops.size / parallelism
    val hiSize = loSize + 1
    val (hi,lo) = ops.splitAt(mod * hiSize)
    val hiBatch = if (mod > 0)    hi.grouped(hiSize) else Nil
    val loBatch = if (loSize > 0) lo.grouped(loSize) else Nil
    hiBatch.toSeq ++ loBatch.toSeq
  }

}
