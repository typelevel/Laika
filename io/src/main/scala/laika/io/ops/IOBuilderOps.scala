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

package laika.io.ops

import cats.effect.{ Async, Sync }
import laika.io.runtime.Batch

/** Builder step that allows to choose between sequential and parallel execution and specify the effect type.
  *
  * @author Jens Halm
  */
abstract class IOBuilderOps[T[_[_]]] {

  /** Creates a builder for sequential execution.
    */
  def sequential[F[_]: Async]: T[F] = {
    implicit val runtime: Batch[F] = Batch.sequential
    build
  }

  /** Creates a builder for parallel execution.
    *
    * This builder creates instances with a level of parallelism matching the available cores.
    * For explicit control of parallelism use the other `parallel` method.
    */
  def parallel[F[_]: Async]: T[F] = parallel(Runtime.getRuntime.availableProcessors)

  /** Creates a builder for parallel execution.
    *
    * This builder creates instances with the specified level of parallelism.
    */
  def parallel[F[_]: Async](parallelism: Int): T[F] = {
    implicit val runtime: Batch[F] = Batch.parallel(parallelism)
    build
  }

  protected def build[F[_]: Async: Batch]: T[F]

}
