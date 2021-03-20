/*
 * Copyright 2012-2020 the original author or authors.
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

package laika.io.runtime

import cats.effect.Sync
import cats.implicits._
import cats.{Monad, Parallel, Traverse}

/** Type class for the effect F that encapsulates the mechanism 
  * and configuration for executing a batch of operations, either sequentially or in parallel.
  * 
  * @author Jens Halm
  */
trait Batch[F[_]] {

  /** The `Sync` instance for the type F.
    */
  implicit def F: Sync[F]

  /** The optional `Parallel` instance for the type F.
    * 
    * If it is missing all parallel invocation will downgrade to sequential execution.
    */
  def parallelInstance: Option[Parallel[F]]

  /** The desired level of parallelism for all parallel operations.
    */
  def parallelism: Int

  /** Runs the specified batch in parallel, but may fall back to sequential execution
    * depending on configuration.
    */
  def runParallel[A] (fas: Vector[F[A]]): F[Vector[A]] = parallelInstance match {
    case Some(par) if parallelism > 1 && fas.size > 1 => BatchRuntime
      .createBatches(fas, parallelism)
      .parSequence(implicitly[Monad[F]], implicitly[Traverse[Vector]], par)
      .map(_.flatten)
    case _ => fas.sequence
  } 
    

}

/** Constructors for Runtime instances for parallel or sequential execution.
  */
object Batch {

  /** Summoner for implicit Runtime instances.
    */
  def apply[F[_]: Batch]: Batch[F] = implicitly[Batch[F]]

  /** Creates a Runtime instance for sequential execution based on the specified
    * contexts for CPU-bound and blocking operations.
    */
  def sequential[F[_]: Sync]: Batch[F] = new Batch[F] {
    val F = implicitly[Sync[F]]
    val parallelInstance = None
    val parallelism = 1
  }

  /** Creates a Runtime instance for parallel execution based on the specified
    * contexts for CPU-bound and blocking operations.
    */
  def parallel[F[_]: Sync: Parallel] (parallelismParam: Int): Batch[F] = new Batch[F] {
    val F = implicitly[Sync[F]]
    val parallelInstance = Some(implicitly[Parallel[F]])
    val parallelism = parallelismParam
  }

}
