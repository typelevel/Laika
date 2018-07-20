/*
 * Copyright 2013-2016 the original author or authors.
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

package laika.parse.markdown

import laika.api.ext.ParserDefinitionBuilders
import laika.factory.ParserFactory
import laika.parse.core.markup.RootParserBase
import laika.parse.markdown.html.VerbatimHTML
  
/** A parser for Markdown text. Instances of this class may be passed directly
 *  to the `Parse` or `Transform` APIs:
 *  
 *  {{{
 *  val document = Parse as Markdown fromFile "hello.md"
 *  
 *  Transform from Markdown to HTML fromFile "hello.md" toFile "hello.html"
 *  }}}
 *  
 *  Since this library is not solely focused on producing HTML output,
 *  parsing verbatim HTML elements like defined by the official Markdown 
 *  syntax description is an optional feature, as some types of renderers 
 *  would not know what to do with HTML nodes in the document tree. 
 *  It must be enabled explicitly:
 *  
 *  {{{
 *  val document = Parse.as(Markdown).withRawContent.fromFile("hello.md")
 *  }}}
 *  
 *  To switch off all custom extensions like directives,
 *  configuration sections at the start of the document or automatic
 *  id generation for headers, you can run the parser in strict mode:
 *  
 *  {{{
 *  Transform.from(Markdown).to(HTML).strict
 *    .fromFile("hello.md").toFile("hello.html")
 *  }}}
 * 
 *  @author Jens Halm
 */
object Markdown extends ParserFactory {

  val fileSuffixes: Set[String] = Set("md", "markdown")
  
  val extensions = Seq(VerbatimHTML)
  
  /** The actual parser function, fully parsing the specified input and
   *  returning a document tree.
   */
  def newRootParser (parserExtensions: ParserDefinitionBuilders): RootParserBase =
    new RootParser(parserExtensions.blockParsers, parserExtensions.spanParsers)

}
