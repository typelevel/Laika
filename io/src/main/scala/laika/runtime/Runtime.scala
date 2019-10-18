/*
 * Copyright 2012-2019 the original author or authors.
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

package laika.runtime

import cats.{Monad, Parallel, Traverse}
import cats.effect.{Async, ContextShift}
import cats.implicits._

/** Type class for the effect F that encapsulates the mechanism 
  * and configuration for running blocking and/or parallel operations.
  * 
  * @author Jens Halm
  */
trait Runtime[F[_]] {

  /** The `Async` instance for the type F.
    */
  implicit def F: Async[F]

  /** The optional `Par` instance for the type F.
    * 
    * If it is missing all parallel invocation will downgrade
    * to sequential execution.
    */
  def parallelInstance: Option[Parallel[F]]

  /** The desired level of parallelism for all parallel operations.
    */
  def parallelism: Int

  /** The execution context for all CPU-bound processing.
    */
  def processingContext: ContextShift[F]

  /** The execution context for all blocking IO operations.
    */
  def blockingContext: ContextShift[F]

  /** Runs the specified effect on the execution context for blocking IO
    * and subsequently switches back to the processing context.
    */
  def runBlocking[A] (fa: F[A]): F[A] = for {
    _   <- blockingContext.shift
    res <- fa
    _   <- processingContext.shift
  } yield res

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
object Runtime {

  /** Creates a Runtime instance for sequential execution based on the specified
    * contexts for CPU-bound and blocking operations.
    */
  def sequential[F[_]: Async] (processingC: ContextShift[F], blockingC: ContextShift[F]): Runtime[F] = new Runtime[F] {
    val F = implicitly[Async[F]]
    val parallelInstance = None
    val parallelism = 1
    val processingContext = processingC
    val blockingContext = blockingC
  }

  /** Creates a Runtime instance for parallel execution based on the specified
    * contexts for CPU-bound and blocking operations.
    */
  def parallel[F[_]: Async: Parallel, G[_]] (processingC: ContextShift[F], blockingC: ContextShift[F], 
                                   parallelismSetting: Int): Runtime[F] = new Runtime[F] {
    val F = implicitly[Async[F]]
    val parallelInstance = Some(implicitly[Parallel[F]])
    val parallelism = parallelismSetting
    val processingContext = processingC
    val blockingContext = blockingC
  }

}
