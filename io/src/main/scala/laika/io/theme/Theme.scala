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

package laika.io.theme

import cats.Applicative
import cats.data.Kleisli
import laika.bundle.ExtensionBundle
import laika.io.model.{InputTree, ParsedTree}

/**
  * @author Jens Halm
  */
trait Theme[F[_]] {

  def inputs: F[InputTree[F]]
  
  def extensions: Seq[ExtensionBundle]
  
  def treeTransformer: Kleisli[F, ParsedTree[F], ParsedTree[F]]
  
}

object Theme {

  def empty[F[_]: Applicative]: Theme[F] = new Theme[F] {
    def inputs: F[InputTree[F]] = Applicative[F].pure(InputTree.empty)
    def extensions: Seq[ExtensionBundle] = Nil
    def treeTransformer: Kleisli[F, ParsedTree[F], ParsedTree[F]] = Kleisli(Applicative[F].pure)
  }

}
