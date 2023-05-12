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

package laika.format

import laika.ast.Element
import laika.factory.{ RenderContext, RenderFormat }
import laika.render.{ HTMLFormatter, HTMLRenderer }

/** A render format for HTML output. May be directly passed to the `Render` or `Transform` APIs:
  *
  *  {{{
  *  Renderer.of(HTML).build.render(document)
  *
  *  Transformer.from(Markdown).to(HTML).build.transform(inputString)
  *  }}}
  *
  *  @author Jens Halm
  */
case object HTML extends RenderFormat[HTMLFormatter] {

  val fileSuffix = "html"

  val defaultRenderer: (HTMLFormatter, Element) => String = HTMLRenderer

  val formatterFactory: RenderContext[HTMLFormatter] => HTMLFormatter = HTMLFormatter

}
