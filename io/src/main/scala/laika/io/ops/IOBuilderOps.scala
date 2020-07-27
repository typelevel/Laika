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

import cats.Parallel
import cats.effect.{Sync, ContextShift}

/** Builder step that allows to choose between sequential and parallel execution and specify the effect type. 
  * 
  * @author Jens Halm
  */
trait IOBuilderOps[SEQ[_[_]], PAR[_[_]]] {

  /** Creates a builder for sequential execution.
    * 
    * The resulting API will offer methods like `fromFile`, `fromStream`, etc.,
    * in contrast to the parallel builder where you can specify entire directories as input.
    * 
    * Despite the name, the calling code can of course create multiple effects of this kind and run
    * them in parallel.
    */
  def sequential[F[_]: Sync: ContextShift]: SEQ[F]

  /** Creates a builder for parallel execution.
    *
    * The resulting API will offer methods like `fromDirectory`, `toDirectory`, etc.,
    * in contrast to the sequential builder where you specify individual files or streams as input.
    *
    * This builder creates instances with a level of parallelism matching the available cores.
    * For explicit control of parallelism use the other `parallel` method.
    */
  def parallel[F[_]: Sync: ContextShift: Parallel]: PAR[F] = parallel(java.lang.Runtime.getRuntime.availableProcessors)

  /** Creates a builder for parallel execution.
    *
    * The resulting API will offer methods like `fromDirectory`, `toDirectory`, etc.,
    * in contrast to the sequential builder where you specify individual files or streams as input.
    *
    * This builder creates instances with the specified level of parallelism.
    */
  def parallel[F[_]: Sync: ContextShift: Parallel](parallelism: Int): PAR[F]

}
