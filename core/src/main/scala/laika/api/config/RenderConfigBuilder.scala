/*
 * Copyright 2013-2018 the original author or authors.
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

package laika.api.config

import laika.api.ext.{ExtensionBundle, Theme}
import laika.factory.RendererFactory
import laika.tree.Elements.{MessageLevel, RenderFunction}

/**
  * @author Jens Halm
  */
trait RenderConfigBuilder[Writer] extends OperationConfigBuilder {

  /** Specifies a custom render function that overrides one or more of the default
    *  renderers for the output format this instance uses.
    *
    *  This method expects a function that returns a partial function as the parameter.
    *  The outer function allows to capture the writer instance to write to and will
    *  only be invoked once. The partial function will then be invoked for each
    *  element it is defined at.
    *
    *  Simple example for customizing the HTML output for emphasized text, adding a specific
    *  style class:
    *
    *  {{{
    *  Transform from Markdown to HTML rendering { out =>
    *    { case Emphasized(content) => out << """&lt;em class="big">""" << content << "&lt;/em>" }
    *  } fromFile "hello.md" toFile "hello.html"
    *  }}}
    */
  def rendering (customRenderer: Writer => RenderFunction): ThisType = using(new ExtensionBundle {
    override val useInStrictMode: Boolean = true
    override def themeFor[W](rendererFactory: RendererFactory[W]): Theme[W] =
      Theme(customRenderers = Seq(customRenderer)).asInstanceOf[Theme[W]]
  })

  /**  Specifies the minimum required level for a system message
    *  to get included into the output by this renderer.
    */
  def withMessageLevel (level: MessageLevel): ThisType = withConfig(config.copy(minMessageLevel = level))

}
