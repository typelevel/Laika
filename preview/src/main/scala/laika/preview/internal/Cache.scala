/*
 * Copyright 2012-2021 the original author or authors.
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

package laika.preview.internal

import cats.effect.{ Concurrent, Ref }
import cats.syntax.all.*
import laika.preview.internal.Cache.Result

private[preview] class Cache[F[_]: Concurrent, V](factory: F[V], ref: Ref[F, Result[V]]) {

  def get: F[V] = ref.get.rethrow

  def update: F[Unit] =
    factory.attempt.flatMap { result =>
      ref.set(result)
    }

}

private[preview] object Cache {

  type Result[A] = Either[Throwable, A]

  def create[F[_]: Concurrent, V](factory: F[V]): F[Cache[F, V]] = factory.attempt.flatMap {
    result =>
      Concurrent[F].ref(result).map { ref => new Cache[F, V](factory, ref) }
  }

}
