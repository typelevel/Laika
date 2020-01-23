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

package laika.parse.code

import laika.bundle.{BundleOrigin, ExtensionBundle, ParserBundle}
import laika.parse.code.languages.{CSS, HOCON, HTML, JSON, Java, JavaScript, Python, Scala, TextMarkup, TypeScript, XML}

/** Registry for all code syntax highlighters provided out of the box.
  * 
  * @author Jens Halm
  */
case object DefaultSyntaxHighlighters extends ExtensionBundle {
  
  override val origin: BundleOrigin = BundleOrigin.Library

  val description: String = "Default Syntax Highlighters for Code"

  override def parsers: ParserBundle = ParserBundle(
    syntaxHighlighters = Seq(
      Scala.highlighter,
      Java.highlighter,
      Python.highlighter,
      TypeScript.highlighter,
      JavaScript.highlighter,
      HTML.highlighter,
      CSS.highlighter,
      JSON.highlighter,
      HOCON.highlighter,
      XML.highlighter,
      TextMarkup.md.highlighter,
      TextMarkup.rst.highlighter,
      TextMarkup.laikaExtensions.extendedMarkdown,
      TextMarkup.laikaExtensions.extendedRst,
      TextMarkup.laikaExtensions.extendedHTML
    )
  )
  
}
