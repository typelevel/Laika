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

  // format: off
  private def generate(helium: Helium): String = {

    val light = helium.siteSettings.colors.theme
    val dark  = helium.siteSettings.darkMode.fold(light)(_.theme)

    s"""
       |import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.esm.min.mjs';
       |const dark = window.matchMedia('(prefers-color-scheme: dark)').matches;
       |mermaid.initialize({ 
       |  startOnLoad: true,
       |  theme: "base",
       |  themeVariables: {
       |    'darkMode': dark,
       |    'primaryColor': dark ? '${dark.primaryLight.displayValue}' : '${light.primaryLight.displayValue}', 
       |    'primaryTextColor': dark ? '${dark.primary.displayValue}' : '${light.primary.displayValue}',
       |    'primaryBorderColor': dark ? '${dark.primaryMedium.displayValue}' : '${light.primaryMedium.displayValue}',
       |    'lineColor': dark ? '${dark.primary.displayValue}' : '${light.primary.displayValue}',
       |    'background': dark ? '${dark.background.displayValue}' : '${light.background.displayValue}',
       |  }
       |});
       |""".stripMargin
  }
  // format: on

  private def includeIn(doc: Document): Boolean =
    doc.content
      .collect { case CodeBlock("mermaid", _, _, _) => true }
      .nonEmpty

  def applyTo(helium: Helium): Helium =
    helium.site.inlineJS(
      content = generate(helium),
      isModule = true,
      condition = includeIn
    )

}
