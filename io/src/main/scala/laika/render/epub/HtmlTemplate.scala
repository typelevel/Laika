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

package laika.render.epub

import laika.api.config.Key
import laika.ast._

/** The default template for EPUB XHTML renderers.
  *
  * @author Jens Halm
  */
object HtmlTemplate {
  
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

  // TODO - 0.12 - temporary duplication of the styleLink directive until the directive impl has been rewritten
  case object StyleLinkSpan extends SpanResolver with TemplateSpan {

    type Self = this.type 
    def withOptions (options: Options): this.type = this
    val options = NoOpt

    def resolve (cursor: DocumentCursor): TemplateElement = {
      val refPath = cursor.parent.target.path
      val allLinks = cursor.root.target.staticDocuments.filter(_.suffix == "css").map { staticPath =>
        val path = staticPath.relativeTo(refPath).toString
        s"""<link rel="stylesheet" type="text/css" href="$path" />"""
      }
      TemplateElement(RawContent(Seq("html","xhtml"), allLinks.mkString("\n    ")))
    }
  }

  /** The default template for EPUB XHTML renderers.
    *
    * It can be overridden by placing a custom template document
    * with the name `default.template.epub.xhtml` into the root directory
    * of the input files. Alternatively the default can also be overridden
    * for individual sub-directories with a corresponding file with the same name.
    */
  val default: TemplateRoot = {
    val templateSpans = templateText.split("#").map(TemplateString(_))
    TemplateRoot(Seq(
      templateSpans(0),
      TemplateContextReference(Key("document.title"), required = true),
      templateSpans(1),
      StyleLinkSpan,
      templateSpans(2),
      TemplateContextReference(Key("document.content"), required = true),
      templateSpans(3)
    ))
  }
  
}
