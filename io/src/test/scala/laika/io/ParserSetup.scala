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

package laika.io

import cats.effect.{IO, Resource}
import laika.api.MarkupParser
import laika.bundle.ExtensionBundle
import laika.format.Markdown
import laika.io.api.TreeParser
import laika.io.helper.TestThemeBuilder
import laika.io.implicits._
import laika.theme.Theme


trait ParserSetup {

  val defaultBuilder: TreeParser.Builder[IO] = MarkupParser
    .of(Markdown)
    .parallel[IO]
    .withTheme(Theme.empty)

  val defaultParser: Resource[IO, TreeParser[IO]] = defaultBuilder.build

  def parserWithBundle (bundle: ExtensionBundle): Resource[IO, TreeParser[IO]] =
    MarkupParser
      .of(Markdown)
      .using(bundle)
      .parallel[IO]
      .withTheme(Theme.empty)
      .build

  def parserWithTheme (bundle: ExtensionBundle): Resource[IO, TreeParser[IO]] =
    MarkupParser
      .of(Markdown)
      .parallel[IO]
      .withTheme(TestThemeBuilder.forBundle(bundle))
      .build

  def parserWithThemeAndBundle (themeBundle: ExtensionBundle, appBundle: ExtensionBundle): Resource[IO, TreeParser[IO]] =
    MarkupParser
      .of(Markdown)
      .using(appBundle)
      .parallel[IO]
      .withTheme(TestThemeBuilder.forBundle(themeBundle))
      .build
}
