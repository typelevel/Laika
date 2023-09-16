/*
 * Copyright 2012-2022 the original author or authors.
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

package laika.io.model

import cats.effect.Async
import cats.syntax.all.*

/** File filter that defines the filter function in `[F[_]]` to allow for effectful filter logic.
  */
trait FileFilter { self =>

  def filter[F[_]: Async](file: FilePath): F[Boolean]

  /** Creates a new instance that filters file paths where either
    * this filter or the specified other filter applies.
    */
  def orElse(other: FileFilter): FileFilter = new FileFilter {

    def filter[G[_]: Async](file: FilePath) = self.filter[G](file).ifM(
      Async[G].pure(true),
      other.filter[G](file)
    )

  }

}

object FileFilter {

  def lift(f: FilePath => Boolean): FileFilter = new FileFilter {
    def filter[F[_]: Async](file: FilePath) = Async[F].pure(f(file))
  }

}
