/*
 * Copyright 2013-2016 the original author or authors.
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

package laika.factory

import laika.ast.{Element, MessageLevel, RenderFunction, StyleDeclarationSet, TemplateRoot}
import laika.bundle.{RenderTheme, StaticDocuments}
import laika.config.RenderConfig
import laika.io.Output

/** Responsible for creating renderer instances for a specific output format.
 *  A renderer is simply a function of type `Element => Unit`. In addition
 *  to the actual renderer function, the factory method also produces
 *  an instance of the generic `W` type which is the writer API to use
 *  for custom renderer functions and which is specific to the output format.
 *  
 *  @author Jens Halm
 */
trait RenderFormat[W] {

  
  /** The file suffix to use when rendering the output
   *  to a file. When transforming entire directories
   *  the suffix of the markup file will be automatically
   *  replaced by the suffix for the output format.
   */
  def fileSuffix: String

  /** The default theme to use if no theme is explicitly specified.
    */
  def defaultTheme: Theme

  /** Creates a new renderer and a new writer instance for the specified
   *  output and delegate renderer. The delegate function needs to be used
   *  whenever an element renders its children, as the user might have
   *  installed custom renderer overrides this instance is not aware of.
   *  If no custom renderer is responsible for the children, the invocation
   *  will fall back to calling this renderer again.
   * 
   *  In contrast to the parser function, a new render function will be created for
   *  each render operation. In addition
   *  to the actual renderer function, this method also produces
   *  an instance of the generic `W` type which is the writer API to use
   *  for custom renderer functions and which is specific to the output format.
   *  
   *  @param out the output to write to
   *  @param root the root element the new renderer will be used for
   *  @param delegate a render function to use for rendering the children of an element
   *  @param styles the styles the new renderer should apply to the rendered elements
   *  @param config additional configuration for the renderer
   *  @return a new writer API of type `W` and a new render function
   */
  def newRenderer (out: Output, root: Element, delegate: Element => Unit,
                   styles: StyleDeclarationSet, config: RenderConfig): (W, Element => Unit)


  case class Theme (customRenderer: W => RenderFunction = {_: W => PartialFunction.empty},
                    defaultTemplate: Option[TemplateRoot] = None,
                    defaultStyles: StyleDeclarationSet = StyleDeclarationSet.empty,
                    staticDocuments: StaticDocuments = StaticDocuments.empty) extends RenderTheme {

    type Writer = W

    def withBase (base: Theme): Theme = Theme(
      { w: W => customRenderer(w).orElse(base.customRenderer(w)) },
      defaultTemplate.orElse(base.defaultTemplate),
      base.defaultStyles ++ defaultStyles,
      StaticDocuments(staticDocuments.merge(base.staticDocuments.tree))
    )

  }

}
