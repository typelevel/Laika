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

package laika.rewrite.nav

import laika.ast.Path
import laika.bundle.{BundleOrigin, ExtensionBundle}

/** Extension specific to site rendering that translates the output path,
  * producing "pretty URLs" that do not contain the `html` file suffix.
  * 
  * A path like `foo/bar.html` for example would be translated to `foo/bar/index.html` so that
  * links can be simply rendered as `foo/bar/`.
  * 
  * When the render format is anything other than HTML, this extension has no effect.
  * 
  * The extension can be added to a transformer like any other extension:
  *
  * {{{
  * val transformer = Transformer
  *   .from(Markdown)
  *   .to(HTML)
  *   .using(GitHubFlavor)
  *   .using(SyntaxHighlighting)
  *   .using(PrettyURLs)
  *   .build
  * }}}
  * 
  * or via the `laikaExtensions` setting when using the sbt plugin:
  * 
  * {{{
  *   laikaExtensions += PrettyURLs
  * }}}
  *
  * @author Jens Halm
  */
object PrettyURLs extends ExtensionBundle {

  override val origin: BundleOrigin = BundleOrigin.Library

  val description: String = "Pretty URL extension for site rendering"
  
  private val outputBaseName = "index"
  private val formatSelector = "html"

  override def extendPathTranslator: PartialFunction[ExtensionBundle.PathTranslatorExtensionContext, PathTranslator] = {
    case context if context.outputContext.formatSelector == formatSelector =>
      val asPrettyURL: Path => Path = path =>
        if (path.basename == outputBaseName || !path.suffix.contains(context.outputContext.fileSuffix)) path
        else {
          val basePath = path.withoutFragment.withoutSuffix / outputBaseName
          val withSuffix = path.suffix.fold(basePath)(basePath.withSuffix)
          path.fragment.fold(withSuffix)(withSuffix.withFragment)
        }
      PathTranslator.postTranslate(context.baseTranslator)(asPrettyURL)
  }
}
