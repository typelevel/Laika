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

import laika.bundle.{BundleOrigin, ExtensionBundle, ParserBundle, SyntaxHighlighter}
import laika.parse.code.languages._

/** Registry for all code syntax highlighters provided out of the box.
  * 
  * @author Jens Halm
  */
case object SyntaxHighlighting extends ExtensionBundle { self =>
  
  override val origin: BundleOrigin = BundleOrigin.Library

  val description: String = "Default Syntax Highlighters for Code"

  override def parsers: ParserBundle = ParserBundle(
    syntaxHighlighters = Seq(
      ScalaSyntax,
      JavaSyntax,
      PythonSyntax,
      TypeScriptSyntax,
      JavaScriptSyntax,
      HTMLSyntax,
      CSSSyntax,
      JSONSyntax,
      HOCONSyntax,
      XMLSyntax,
      MarkdownSyntax,
      ReStructuredTextSyntax,
      LaikaExtensionSyntax.forMarkdown,
      LaikaExtensionSyntax.forRst,
      LaikaExtensionSyntax.forHTML
    )
  )
  
  def withSyntax (syntax: SyntaxHighlighter*): ExtensionBundle = new ExtensionBundle {
    
    override val origin: BundleOrigin = BundleOrigin.User
    
    val description: String = "Customized collection of Syntax Highlighters for Code"

    override def parsers: ParserBundle = ParserBundle(
      syntaxHighlighters = self.parsers.syntaxHighlighters ++ syntax
    )
  }
  
}
