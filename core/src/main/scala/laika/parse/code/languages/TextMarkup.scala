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

package laika.parse.code.languages

import laika.bundle.SyntaxHighlighter

/**
  * @author Jens Halm
  */
object TextMarkup {

  lazy val markdown: SyntaxHighlighter = SyntaxHighlighter.build("markdown", "md")(Seq(

  ))

  lazy val rst: SyntaxHighlighter = SyntaxHighlighter.build("reStructuredText", "rst")(Seq(

  ))

  lazy val laikaMarkdown: SyntaxHighlighter = SyntaxHighlighter.build("laikaMarkdown", "laika-md")(Seq(

  ))

  lazy val laikaRst: SyntaxHighlighter = SyntaxHighlighter.build("laikaReStructuredText", "laika-rst")(Seq(

  ))

  lazy val laikaHTML: SyntaxHighlighter = SyntaxHighlighter.build("laikaHTML", "laika-html")(Seq(

  ))
  
}
