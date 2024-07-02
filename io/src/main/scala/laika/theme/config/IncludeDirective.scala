/*
 * Copyright 2012-2024 the original author or authors.
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

package laika.theme.config

import laika.api.bundle.TemplateDirectives
import laika.ast.{ Document, Path }
import laika.theme.config.internal.{
  ExternalCSS,
  ExternalJS,
  IncludeDirectiveBuilder,
  InlineCSS,
  InlineJS,
  InternalCSS,
  InternalJS,
  ScriptIncludes,
  StyleIncludes
}

/** Represents the configuration for the `@:includeCSS` directive
  * for a single format (e.g. HTML or EPUB).
  */
class IncludeCSSConfig private (
    private[config] val includes: StyleIncludes = StyleIncludes.empty
) {

  /** Links an external CSS resource from the specified URL.
    *
    * The `condition` attribute can be used to only include the CSS when some user-defined predicates are satisfied.
    */
  def externalCSS(
      url: String,
      attributes: StyleAttributes = StyleAttributes.defaults,
      condition: Document => Boolean = _ => true
  ): IncludeCSSConfig = {
    val newInclude = ExternalCSS(url, attributes, condition)
    new IncludeCSSConfig(includes.add(newInclude))
  }

  /** Auto-links CSS documents from the specified path, which may point to a single CSS document
    * or a directory.
    * In case of a directory it will be searched recursively and all CSS files found within it
    * will be linked in the HTML head.
    *
    * The `condition` attribute can be used to only include the CSS when some user-defined predicates are satisfied.
    */
  def internalCSS(
      searchPath: Path,
      attributes: StyleAttributes = StyleAttributes.defaults,
      condition: Document => Boolean = _ => true
  ): IncludeCSSConfig = {
    val newInclude = InternalCSS(searchPath, attributes, condition)
    new IncludeCSSConfig(includes.add(newInclude))
  }

  /** Inserts inline style declarations into the HTML head.
    *
    * The `condition` attribute can be used to only include the CSS when some user-defined predicates are satisfied.
    */
  def inlineCSS(
      content: String,
      condition: Document => Boolean = _ => true
  ): IncludeCSSConfig = {
    val newInclude = InlineCSS(content, condition)
    new IncludeCSSConfig(includes.add(newInclude))
  }

}

object IncludeCSSConfig {

  val empty: IncludeCSSConfig = new IncludeCSSConfig()

}

/** Represents the configuration for the `@:includeJS` directive
  * for a single format (e.g. HTML or EPUB).
  */
class IncludeJSConfig private (
    private[config] val includes: ScriptIncludes = ScriptIncludes.empty
) {

  /** Links an external JavaScript resource from the specified URL.
    *
    * The `condition` attribute can be used to only include the CSS when some user-defined predicates are satisfied.
    */
  def externalJS(
      url: String,
      attributes: ScriptAttributes = ScriptAttributes.defaults,
      condition: Document => Boolean = _ => true
  ): IncludeJSConfig = {
    val newInclude = ExternalJS(url, attributes, condition)
    new IncludeJSConfig(includes.add(newInclude))
  }

  /** Auto-links JavaScript documents from the specified path, which may point to a single JS document
    * or a directory.
    * In case of a directory it will be searched recursively and all `*.js` files found within it
    * will be linked in the HTML head.
    *
    * The `condition` attribute can be used to only include the CSS when some user-defined predicates are satisfied.
    */
  def internalJS(
      searchPath: Path,
      attributes: ScriptAttributes = ScriptAttributes.defaults,
      condition: Document => Boolean = _ => true
  ): IncludeJSConfig = {
    val newInclude = InternalJS(searchPath, attributes, condition)
    new IncludeJSConfig(includes.add(newInclude))
  }

  /** Inserts inline scripts into the HTML head.
    *
    * The `condition` attribute can be used to only include the CSS when some user-defined predicates are satisfied.
    */
  def inlineJS(
      content: String,
      isModule: Boolean = false,
      condition: Document => Boolean = _ => true
  ): IncludeJSConfig = {
    val newInclude = InlineJS(content, isModule, condition)
    new IncludeJSConfig(includes.add(newInclude))
  }

}

object IncludeJSConfig {

  val empty: IncludeJSConfig = new IncludeJSConfig()

}

/** Builders for the `@:includeCSS` and `@:includeJS` directives,
  * which can be used in templates for HTML and EPUB output.
  */
object IncludeDirective {

  /** Creates an instance of the `@:includeCSS` directive that
    * can be added to any `ExtensionBundle`.
    *
    * The configurations for HTML and EPUB are separate and either
    * of the two can be empty.
    *
    * They can also point to the same instance in case the two formats
    * should use the same style includes.
    */
  def forCSS(
      htmlConfig: IncludeCSSConfig,
      epubConfig: IncludeCSSConfig = IncludeCSSConfig.empty
  ): TemplateDirectives.Directive =
    IncludeDirectiveBuilder.includeCSS(htmlConfig.includes, epubConfig.includes)

  /** Creates an instance of the `@:includeJS` directive that
    * can be added to any `ExtensionBundle`.
    *
    * The configurations for HTML and EPUB are separate and either
    * of the two can be empty.
    *
    * They can also point to the same instance in case the two formats
    * should use the same script includes.
    */
  def forJS(
      htmlConfig: IncludeJSConfig,
      epubConfig: IncludeJSConfig = IncludeJSConfig.empty
  ): TemplateDirectives.Directive =
    IncludeDirectiveBuilder.includeJS(htmlConfig.includes, epubConfig.includes)

}
