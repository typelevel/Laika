/*
 * Copyright 2012-2019 the original author or authors.
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

import laika.ast._
import laika.factory.{RenderContext2, RenderFormat2}
import laika.render._

/** A renderer for AST output (a formatted Abstract Syntax Tree), primarily useful for debugging purposes.
 *  May be directly passed to the `Render` or `Transform` APIs:
 * 
 *  {{{
 *  Render as AST from document toString
 *  
 *  Transform from Markdown to AST fromFile "hello.md" toConsole
 *  }}}
 * 
 *  @author Jens Halm
 */
object AST2 extends RenderFormat2[TextFormatter] {

  val fileSuffix = "txt"

  val defaultRenderFunction: (TextFormatter, Element) => String = ASTRenderer2.render

  def newFormatter (context: RenderContext2[TextFormatter]): TextFormatter = {

    // TODO - 0.12 - introduce Writer constructors taking a RenderContext
    val indentation = if (context.config.renderFormatted) Indentation.default else Indentation.none
    TextFormatter(context.renderChild, List(context.root), indentation)
  }
  
  val defaultTheme = Theme()

}
