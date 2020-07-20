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

package laika.helium.generate

import laika.ast._
import laika.directive.StandardDirectives.StyleLinks
import laika.rewrite.ReferenceResolver.CursorKeys

/** The default template for EPUB XHTML renderers.
  *
  * @author Jens Halm
  */
object EPUBTemplate {
  
  private val templateText = """<?xml version="1.0" encoding="UTF-8"?>
                               |<!DOCTYPE html>
                               |<html xmlns="http://www.w3.org/1999/xhtml" xmlns:epub="http://www.idpf.org/2007/ops">
                               |  <head>
                               |    <meta charset="utf-8" />
                               |    <meta name="generator" content="laika" />
                               |    <title>#</title>
                               |    #
                               |  </head>
                               |  <body epub:type="bodymatter">
                               |    <div class="content">
                               |      #
                               |    </div>
                               |  </body>
                               |</html>""".stripMargin

  /** The default template for EPUB XHTML renderers.
    *
    * It can be overridden by placing a custom template document
    * with the name `default.template.epub.xhtml` into the root directory
    * of the input files. Alternatively the default can also be overridden
    * for individual sub-directories with a corresponding file with the same name.
    */
  val default: TemplateRoot = {
    val templateSpans = templateText.split("#").map(TemplateString(_))
    TemplateRoot(
      templateSpans(0),
      TemplateContextReference(CursorKeys.documentTitle, required = true),
      templateSpans(1),
      StyleLinks,
      templateSpans(2),
      TemplateContextReference(CursorKeys.documentContent, required = true),
      templateSpans(3)
    )
  }
  
}
