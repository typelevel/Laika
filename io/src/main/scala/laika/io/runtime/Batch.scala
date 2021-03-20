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
import cats.Parallel

/** Type class for the effect F that encapsulates the mechanism 
  * and configuration for executing a batch of operations, either sequentially or in parallel.
  * 
  * @author Jens Halm
  */
trait Batch[F[_]] {

  /** Runs the specified batch, either sequentially or in parallel, depending on implementation.
    */
  def execute[A] (fas: Vector[F[A]]): F[Vector[A]]

}

/** Constructors for Batch instances for parallel or sequential execution.
  */
object Batch {

  /** Summoner for implicit Batch instances.
    */
  def apply[F[_]: Batch]: Batch[F] = implicitly[Batch[F]]

  /** Creates a Batch instance for sequential execution.
    */
  def sequential[F[_]: Sync]: Batch[F] = new Batch[F] {
    def execute[A] (fas: Vector[F[A]]): F[Vector[A]] = fas.sequence
  }

  /** Creates a Batch instance for parallel execution.
    */
  def parallel[F[_]: Sync: Parallel] (parallelism: Int): Batch[F] = new Batch[F] {
    def execute[A] (fas: Vector[F[A]]): F[Vector[A]] = 
      if (parallelism > 1 && fas.size > 1) BatchRuntime
        .createBatches(fas, parallelism)
        .parSequence
        .map(_.flatten)
      else fas.sequence
  }

}
