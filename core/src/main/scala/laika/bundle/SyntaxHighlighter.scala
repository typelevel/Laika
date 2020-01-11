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
import laika.ast.Text
import laika.parse.Parser
import laika.parse.code.{CodeSpan, CodeSpanParsers, CodeSpanSequence}
import laika.parse.markup.InlineParsers
import laika.parse.text.DelimitedText

/** The parser for syntax highlighting a particular language.
  *
  * @param language the names of the language as used in text markup
  * @param parser the parser for code blocks written in this language
  */
case class SyntaxHighlighter (language: NonEmptyList[String], parser: Parser[Seq[CodeSpan]])

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
    
    val spanParserMap = parsers.flatMap(_.parsers).groupBy(_.startChar).map {
      case (char, definitions) => (char, definitions.map(_.parser).reduceLeft(_ | _))
    }
    
    val rootParser = InlineParsers.spans(DelimitedText.Undelimited, spanParserMap).map(
      _.flatMap {
        case Text(content, _) => Seq(CodeSpan(content))
        case codeSpan: CodeSpan => Seq(codeSpan)
        case codeSeq: CodeSpanSequence => codeSeq.collect { case cs: CodeSpan => cs }
      }
    )
    
    SyntaxHighlighter(languages, rootParser)
  }

  /** Creates a syntax highlighter with the specified, dedicated root parser.
    */
  def apply (language: String, aliases: String*)(rootParser: Parser[Seq[CodeSpan]]): SyntaxHighlighter =
    SyntaxHighlighter(NonEmptyList.of(language, aliases:_*), rootParser)
  
}
