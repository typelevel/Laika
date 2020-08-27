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

import cats.effect.Sync
import laika.ast.Document
import laika.bundle.ExtensionBundle
import laika.factory.Format
import laika.io.model.InputTreeBuilder
import laika.io.runtime.Runtime
import laika.theme.{ThemeBuilder, ThemeProvider, TreeProcessor}


object TestThemeBuilder {
  
  trait Inputs {
    def build[F[_]: Sync: Runtime]: InputTreeBuilder[F]
  }
  
  def forInputs (themeInputs: Inputs): ThemeProvider = new ThemeProvider {
    def build[F[_]: Sync: Runtime] = ThemeBuilder("test").addInputs(themeInputs.build).build
  }

  def forBundle (bundle: ExtensionBundle): ThemeProvider = new ThemeProvider {
    def build[F[_]: Sync: Runtime] = ThemeBuilder("test").addExtensions(bundle).build
  }

  def forBundles (bundles: Seq[ExtensionBundle]): ThemeProvider = new ThemeProvider {
    def build[F[_]: Sync: Runtime] = ThemeBuilder("test").addExtensions(bundles:_*).build
  }
  
  def forDocumentMapper (f: Document => Document): ThemeProvider = new ThemeProvider {
    def build[F[_]: Sync: Runtime] = ThemeBuilder("test")
      .processTree { case _ => TreeProcessor[F].mapDocuments(f) }
      .build
  }
 

  def forDocumentMapper (format: Format)(f: Document => Document): ThemeProvider = new ThemeProvider {
    def build[F[_]: Sync: Runtime] = {
      val MatchedFormat = format
      ThemeBuilder("test")
        .processTree { case MatchedFormat => TreeProcessor[F].mapDocuments(f) }
        .build
    }
  }
  
}
