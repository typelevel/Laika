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

package laika.render

import laika.config.Key
import laika.ast.{TemplateContextReference, TemplateRoot, TemplateString}

/** The default template for HTML renderers.
  * 
  * @author Jens Halm
  */
object HTMLTemplate {

  private val templateText = """<!DOCTYPE html>
                               |<html>
                               |  <head>
                               |    <title>#</title>
                               |  </head>
                               |  <body>
                               |    <div class="content">
                               |      #
                               |    </div>
                               |  </body>
                               |</html>""".stripMargin

  /** The default template for HTML renderers.
    * 
    * It can be overridden by placing a custom template document
    * with the name `default.template.html` into the root directory
    * of the input files. Alternatively the default can also be overridden
    * for individual sub-directories with a corresponding file with the same name.
    */
  val default: TemplateRoot = {
    val templateSpans = templateText.split("#").map(TemplateString(_))
    TemplateRoot(Seq(
      templateSpans(0),
      TemplateContextReference(Key("document","title"), required = true),
      templateSpans(1),
      TemplateContextReference(Key("document","content"), required = true),
      templateSpans(2)
    ))
  }
  
}
