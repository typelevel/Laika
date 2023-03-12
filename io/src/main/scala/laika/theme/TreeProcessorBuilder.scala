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

package laika.theme

import cats.data.Kleisli
import cats.effect.Sync
import laika.io.model.ParsedTree
import laika.io.ops.TreeMapperOps
import laika.theme.Theme.TreeProcessor

/** Provides several shortcuts for constructing a `TreeProcessor` (which is just a type alias for a plain `Kleisli`).
  *
  * @author Jens Halm
  */
abstract class TreeProcessorBuilder[F[_]: Sync] extends TreeMapperOps[F] {

  type MapRes = TreeProcessor[F]

  def evalMapTree(f: ParsedTree[F] => F[ParsedTree[F]]): TreeProcessor[F] = Kleisli(f)
}

/** Entry point for the TreeProcessorBuilder API.
  */
object TreeProcessorBuilder {

  def apply[F[_]: Sync]: TreeProcessorBuilder[F] = new TreeProcessorBuilder[F] {}

}
