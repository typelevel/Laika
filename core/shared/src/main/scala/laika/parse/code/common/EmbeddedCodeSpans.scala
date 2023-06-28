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

import laika.ast.{ CodeSpan, CodeSpans, Span }
import laika.bundle.SyntaxHighlighter
import laika.parse.Parser
import laika.parse.code.{ CodeCategory, CodeSpanParser }
import laika.parse.markup.InlineParsers
import laika.parse.text.DelimitedText
import laika.parse.builders._
import laika.parse.implicits._

/** Factories for creating a span parser that detects other syntax as part of the span.
  *
  * @author Jens Halm
  */
object EmbeddedCodeSpans {

  /** Creates a new parser for code spans based on the specified parser for delimited text
    * and the embedded "foreign" syntax.
    *
    * An example is a parser for an HTML script element, where the text parser is responsible
    * for detecting the end of the tag (`</script>`) whereas the embedded syntax is JavaScript, not HTML.
    */
  def parser(textParser: DelimitedText, embedded: SyntaxHighlighter): Parser[Seq[CodeSpan]] =
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
  def parser(
      textParser: DelimitedText,
      embedded: Seq[CodeSpanParser],
      defaultCategories: Set[CodeCategory] = Set.empty
  ): Parser[Seq[CodeSpan]] = {

    val codeSpanParsers = embedded.flatMap(_.parsers)
    val newLineParsers = codeSpanParsers.filter(_.startChars.contains('\n')).reduceLeftOption(_ | _)
    val mainParser     = InlineParsers.spans(textParser).embedAll(codeSpanParsers)

    def extract(spans: Seq[Span]): List[CodeSpan] =
      spans.flatMap(CodeSpans.extract(defaultCategories)).toList

    newLineParsers.fold(mainParser.map(extract)) { newLineParser =>
      (opt(atStart ~> newLineParser).map(_.toList) ~ mainParser).concat.map(extract)
    }

  }

}
