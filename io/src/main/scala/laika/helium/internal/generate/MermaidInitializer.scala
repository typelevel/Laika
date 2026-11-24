/*
 * Copyright 2012-2023 the original author or authors.
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

package laika.helium.internal.generate

import laika.ast.{ CodeBlock, Document }
import laika.helium.Helium

private[helium] object MermaidInitializer {

  private val script: String =
    """
      |import { mount } from 'https://cdn.jsdelivr.net/npm/pele@1/dist/pele.js';
      |for (const el of document.querySelectorAll('pre.mermaid')) mount(el, el.textContent);
      |""".stripMargin

  private def includeIn(doc: Document): Boolean =
    doc.content
      .collect { case CodeBlock("mermaid", _, _, _) => true }
      .nonEmpty

  def applyTo(helium: Helium): Helium =
    helium.site.inlineJS(
      content = script,
      isModule = true,
      condition = includeIn
    )

}
