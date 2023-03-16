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

package laika.io.helper

import cats.effect.Async
import laika.ast.Document
import laika.bundle.ExtensionBundle
import laika.factory.Format
import laika.io.model.InputTreeBuilder
import laika.theme.{ ThemeBuilder, ThemeProvider, TreeProcessorBuilder }

object TestThemeBuilder {

  trait Inputs {
    def build[F[_]: Async]: InputTreeBuilder[F]
  }

  def forInputs(themeInputs: Inputs): ThemeProvider = new ThemeProvider {
    def build[F[_]: Async] = ThemeBuilder("test").addInputs(themeInputs.build).build
  }

  def forBundle(bundle: ExtensionBundle): ThemeProvider = new ThemeProvider {
    def build[F[_]: Async] = ThemeBuilder("test").addExtensions(bundle).build
  }

  def forBundles(bundles: Seq[ExtensionBundle]): ThemeProvider = new ThemeProvider {
    def build[F[_]: Async] = ThemeBuilder("test").addExtensions(bundles: _*).build
  }

  def forDocumentMapper(f: Document => Document): ThemeProvider = new ThemeProvider {

    def build[F[_]: Async] = ThemeBuilder("test")
      .processTree { case _ => TreeProcessorBuilder[F].mapDocuments(f) }
      .build

  }

  def forDocumentMapper(format: Format)(f: Document => Document): ThemeProvider =
    new ThemeProvider {

      def build[F[_]: Async] = {
        ThemeBuilder("test")
          .processTree(TreeProcessorBuilder[F].mapDocuments(f), format)
          .build
      }

    }

}
