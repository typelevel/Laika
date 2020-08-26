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

import cats.effect.{IO, Resource}
import laika.ast.Document
import laika.bundle.ExtensionBundle
import laika.factory.Format
import laika.io.model.InputTreeBuilder
import laika.theme.{Theme, ThemeBuilder, TreeProcessor}


object TestThemeBuilder {
  
  def forInputs (themeInputs: InputTreeBuilder[IO]): Resource[IO, Theme[IO]] = 
    ThemeBuilder[IO]("test").addInputs(themeInputs).build

  def forBundle (bundle: ExtensionBundle): Resource[IO, Theme[IO]] = 
    ThemeBuilder[IO]("test").addExtensions(bundle).build

  def forBundles (bundles: Seq[ExtensionBundle]): Resource[IO, Theme[IO]] = 
    ThemeBuilder[IO]("test").addExtensions(bundles:_*).build
  
  def forDocumentMapper (f: Document => Document): Resource[IO, Theme[IO]] = ThemeBuilder[IO]("test")
    .processTree { case _ => TreeProcessor[IO].mapDocuments(f) }
    .build

  def forDocumentMapper (format: Format)(f: Document => Document): Resource[IO, Theme[IO]] = {
    val MatchedFormat = format
    ThemeBuilder[IO]("test")
      .processTree { case MatchedFormat => TreeProcessor[IO].mapDocuments(f) }
      .build
  }
    
}
