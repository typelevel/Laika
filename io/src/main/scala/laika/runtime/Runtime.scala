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

/**
  * @author Jens Halm
  */
trait Runtime[F[_]] {
  
  implicit def F: Async[F]
  
  def parallelInstance: Option[Runtime.Par[F]]
  
  def parallelism: Int
  
  def processingContext: ContextShift[F]
  
  def blockingContext: ContextShift[F]
  
  def runBlocking[A] (fa: F[A]): F[A] = for {
    _   <- blockingContext.shift
    res <- fa
    _   <- processingContext.shift
  } yield res

  def runParallel[A] (fas: Vector[F[A]]): F[Vector[A]] = parallelInstance match {
    case Some(par) if parallelism > 1 && fas.size > 1 => BatchRuntime
      .createBatches(fas, parallelism)
      .parSequence(implicitly[Monad[F]], implicitly[Traverse[Vector]], par.parallel)
      .map(_.flatten)
    case _ => fas.sequence
  } 
    

}

object Runtime {
  
  def sequential[F[_]: Async] (processingC: ContextShift[F], blockingC: ContextShift[F]): Runtime[F] = new Runtime[F] {
    val F = implicitly[Async[F]]
    val parallelInstance = None
    val parallelism = 1
    val processingContext = processingC
    val blockingContext = blockingC
  }

  def parallel[F[_]: Async, G[_]] (processingC: ContextShift[F], blockingC: ContextShift[F], 
                                   parallelismSetting: Int)(implicit P: Parallel[F, G]): Runtime[F] = new Runtime[F] {
    val F = implicitly[Async[F]]
    val parallelInstance = Some(Par.fromParallel)
    val parallelism = parallelismSetting
    val processingContext = processingC
    val blockingContext = blockingC
  }

  trait Par[F[_]]{
    type ParG[A]
    def parallel: Parallel[F, ParG]
  }

  object Par {
    def fromParallel[F[_], G[_]](implicit P: Parallel[F, G]): Par[F] =
      new Par[F]{
        type ParG[A] = G[A]
        def parallel: Parallel[F, ParG] = P
      }
  }
  
}
