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
import laika.bundle.ExtensionBundle
import laika.factory.Format
import laika.io.model.{InputTree, ParsedTree}
import laika.io.runtime.Runtime
import laika.theme.Theme.TreeProcessor

/**
  * @author Jens Halm
  */
trait Theme[F[_]] {

  def inputs: InputTree[F]
  
  def extensions: Seq[ExtensionBundle]
  
  def treeProcessor: Format => TreeProcessor[F]
  
}

object Theme {

  type TreeProcessor[F[_]] = Kleisli[F, ParsedTree[F], ParsedTree[F]]

  def empty: ThemeProvider = new ThemeProvider {
    def build[F[_]: Sync: Runtime] = ThemeBuilder("Empty Theme").build
  }

}
