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
import laika.render.{ ASTRenderer, Formatter, Indentation }

/** A renderer for AST output (a formatted Abstract Syntax Tree), primarily useful for debugging purposes.
  *  May be directly passed to the `Render` or `Transform` APIs:
  *
  *  {{{
  *  Renderer.of(AST).build.render(document)
  *
  *  Transformer.from(Markdown).to(AST).build.transform(inputString)
  *  }}}
  *
  *  @author Jens Halm
  */
case object AST extends RenderFormat[Formatter] {

  val fileSuffix = "txt"

  val defaultRenderer: (Formatter, Element) => String = ASTRenderer

  val formatterFactory: RenderContext[Formatter] => Formatter = context => {
    Formatter.defaultFactory(context.withIndentation(Indentation.dotted))
  }

}
