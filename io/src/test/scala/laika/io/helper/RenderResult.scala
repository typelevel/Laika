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

package laika.io.helper

import laika.ast.{TemplateRoot, TemplateString}
import laika.render.HTMLTemplate
import laika.render.epub.{HtmlTemplate => EPUBTemplate}
import laika.render.fo.TestTheme

object RenderResult {
  
  private def buildResult (template: TemplateRoot, insertions: Seq[String]): String = {
    val it = insertions.iterator
    template.content.map {
      case TemplateString(content, _) => content
      case _ => it.next()
    }.mkString
  }
    

  object html {
    def withDefaultTemplate(title: String, content: String): String = buildResult(HTMLTemplate.default, Seq(title, content))
  }

  object epub {
    def withDefaultTemplate(title: String, content: String): String = buildResult(EPUBTemplate.default, Seq(title, "", content))
  }
  
  object fo {
    def withDefaultTemplate(content: String): String = buildResult(TestTheme.foTemplate, Seq("", "", content))
    def withDefaultTemplate(result: String, bookmarks: String = ""): String = buildResult(TestTheme.foTemplate, Seq(bookmarks, "", result))
  }
  
}
