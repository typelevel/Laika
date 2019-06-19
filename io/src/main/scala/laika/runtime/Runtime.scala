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

import cats.effect.{Async, ContextShift}
import cats.implicits._

/**
  * @author Jens Halm
  */
trait Runtime[F[_]] {
  
  implicit def F: Async[F]

  def processingContext: ContextShift[F]
  
  def blockingContext: ContextShift[F]
  
  def runBlocking[A] (fa: F[A]): F[A] = for {
    _   <- blockingContext.shift
    res <- fa
    _   <- processingContext.shift
  } yield res

}

object Runtime {
  
  def sequential[F[_]: Async] (processingC: ContextShift[F], blockingC: ContextShift[F]): Runtime[F] = new Runtime[F] {
    val F = implicitly[Async[F]]
    val processingContext = processingC
    val blockingContext = blockingC
  }
  
}
