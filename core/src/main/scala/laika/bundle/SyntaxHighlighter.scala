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

package laika.bundle

import cats.data.NonEmptyList
import laika.parse.Parser
import laika.parse.code.common.EmbeddedCodeSpans
import laika.parse.code.{CodeCategory, CodeSpan, CodeSpanParsers}
import laika.parse.markup.InlineParsers
import laika.parse.text.DelimitedText

/** The parser for syntax highlighting a particular language.
  *
  * @param language the names of the language as used in text markup
  * @param spanParsers the root-level parsers for code spans written in this language
  */
case class SyntaxHighlighter (language: NonEmptyList[String], spanParsers: Seq[CodeSpanParsers]) extends EmbeddedCodeSpans {
  
  val embedded: Seq[CodeSpanParsers] = spanParsers
  
  val defaultCategories: Set[CodeCategory] = Set.empty
  
  lazy val rootParser: Parser[Seq[CodeSpan]] =
    InlineParsers.spans(DelimitedText.Undelimited, spanParserMap).map(_.flatMap(toCodeSpans))
  
}

object SyntaxHighlighter {

  /** Creates a syntax highlighter by combining the individual parsers for the various
    * kinds of recognized code spans. This is the default factory for most languages
    * where any unrecognized syntax will simply be put into a top-level default category.
    * 
    * For formats that have stricter rules and require a dedicated, hand-written root 
    * parser, you can use the `apply` method.
    */
  def build (language: String, aliases: String*)(parsers: CodeSpanParsers*): SyntaxHighlighter = {
    
    val languages = NonEmptyList.of(language, aliases:_*)

    SyntaxHighlighter(languages, parsers)
    
  }

//  /** Creates a syntax highlighter with the specified, dedicated root parser.
//    */
//  def apply (language: String, aliases: String*)(rootParser: Parser[Seq[CodeSpan]]): SyntaxHighlighter =
//    SyntaxHighlighter(NonEmptyList.of(language, aliases:_*), rootParser)
  
}
