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

package laika.parse.code.common

import laika.ast.{CodeSpan, CodeSpans, Span, ~}
import laika.bundle.SyntaxHighlighter
import laika.parse.Parser
import laika.parse.code.{CodeCategory, CodeSpanParsers}
import laika.parse.markup.InlineParsers
import laika.parse.text.{DelimitedText, PrefixedParser}
import laika.parse.text.TextParsers._

/** Factories for creating a span parser that detects other syntax as part of the span.
  * 
  * @author Jens Halm
  */
object EmbeddedCodeSpans {

  import NumberLiteral._
  
  /** Creates a map for the specified code span parsers that maps the start character
    * for each parser to its combined parsers. If a character maps to multiple parsers
    * they will be combined with `|` in the specified order. 
    */
  def parserMap (embedded: Seq[CodeSpanParsers]): Map[Char, Parser[Span]] = embedded.flatMap(_.parsers).groupBy(_.startChar).map {
    case (char, definitions) => (char, definitions.map(_.parser).reduceLeft(_ | _))
  }

  /** Creates a new parser for code spans based on the specified parser for delimited text
    * and the embedded "foreign" syntax.
    *
    * An example is a parser for an HTML script element, where the text parser is responsible
    * for detecting the end of the tag (`</script>`) whereas the embedded syntax is JavaScript, not HTML.
    */
  def parser (textParser: DelimitedText[String], embedded: SyntaxHighlighter): Parser[Seq[CodeSpan]] =
    parser(textParser, embedded.spanParsers)

  /** Creates a new parser for code spans based on the specified parser for delimited text
    * and the sequence of code span parsers that detect child spans within that delimited text.
    * 
    * The optional `defaultCategories` will be assigned to all text that is not recognized by the child span
    * parsers, but part of the delimited string.
    * 
    * An example is a parser for an HTML tag with attributes, where the text parser is responsible
    * for detecting the end of the tag (e.g. `/>` for an empty tag) whereas the embedded child parsers
    * are responsible for detecting attribute names and values and entity references.
    */
  def parser (textParser: DelimitedText[String], embedded: Seq[CodeSpanParsers], defaultCategories: Set[CodeCategory] = Set.empty): Parser[Seq[CodeSpan]] = {
    val embeddedParserMap = parserMap(embedded) // TODO - this map is created twice now
    val prefixedParsers = embedded.flatMap { codeParsers =>
      codeParsers.parsers.map { parser => PrefixedParser(parser.startChar)(parser.parser) }
    }
    val mainParser = InlineParsers.spans(textParser).embedAll(prefixedParsers)
    
    def extract(spans: Seq[Span]): List[CodeSpan] = spans.flatMap(CodeSpans.extract(defaultCategories)).toList
    
    embeddedParserMap.get('\n').fold(mainParser.map(extract)) { newLineParsers =>
      (opt(lookAhead(atStart) ~> newLineParsers).map(_.toList) ~ mainParser).concat.map(extract)
    }

  }

}
