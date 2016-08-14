/*
 * Copyright 2015-2016 the original author or authors.
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

package laika.render.helper

import laika.io.Input
import laika.tree.Paths.Root

object RenderResult {

  object html {
    
    private lazy val defaultTemplate = Input.fromClasspath("/templates/default.template.html", Root / "default.template.html").asParserInput.source.toString
    
    def withDefaultTemplate(title: String, content: String): String =
      defaultTemplate.replace("{{document.title}}", title).replace("{{document.content}}", content)
    
  }
  
  object fo {
    
    private lazy val defaultTemplate = Input.fromClasspath("/templates/default.template.fo", Root / "default.template.fo").asParserInput.source.toString
    
    def withDefaultTemplate(content: String): String =
      defaultTemplate.replace("{{document.content}}", content).replace("{{document.fragments.bookmarks}}", "")
    
  }
  
}
