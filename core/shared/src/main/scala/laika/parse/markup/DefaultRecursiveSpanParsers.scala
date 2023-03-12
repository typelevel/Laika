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

package laika.parse.markup

import laika.ast.{ InvalidSpan, Span }
import laika.parse._
import laika.parse.combinator.Parsers
import laika.parse.text.{ DelimitedText, PrefixedParser }

/** Default implementation for parsing inline markup recursively.
  *
  * @author Jens Halm
  */
trait DefaultRecursiveSpanParsers extends RecursiveSpanParsers with DefaultEscapedTextParsers {

  /** All default span parsers registered for a host markup language.
    */
  protected def spanParsers: Seq[PrefixedParser[Span]]

  protected lazy val defaultSpanParser: InlineParser[Span, List[Span]] =
    InlineParsers.spans(DelimitedText.Undelimited).embedAll(spanParsers)

  private class TwoPhaseInlineParser(
      textParser: Parser[SourceFragment],
      delegate: => InlineParser[Span, List[Span]]
  ) extends InlineParser[Span, List[Span]] {

    private lazy val spanParser0 = delegate

    override def embed(parser: => PrefixedParser[Span]) =
      new TwoPhaseInlineParser(textParser, delegate.embed(parser))

    override def embedAll(parsers: => Seq[PrefixedParser[Span]]) =
      new TwoPhaseInlineParser(textParser, delegate.embedAll(parsers))

    override def parse(source: SourceCursor) = {
      textParser.parse(source) match {
        case Success(src, next) =>
          spanParser0.parse(src) match {
            case Success(spans, _) => Success(spans, next)
            case f: Failure        => f
          }
        case f: Failure         => f
      }
    }

  }

  def recursiveSpans(p: DelimitedText): InlineParser[Span, List[Span]] =
    InlineParsers.spans(p).embedAll(spanParsers)

  def recursiveSpans(parser: Parser[SourceFragment]): InlineParser[Span, List[Span]] =
    new TwoPhaseInlineParser(parser, defaultSpanParser)

  def recursiveSpans: RecursiveSpanParser = new RecursiveSpanParserDelegate(defaultSpanParser)

  protected class RecursiveSpanParserDelegate(delegate: Parser[Seq[Span]])
      extends RecursiveSpanParser {
    private val parser = Parsers.consumeAll(delegate)

    def parse(in: SourceFragment): Parsed[Seq[Span]] = parser.parse(in)

    def parseAndRecover(in: SourceFragment): Seq[Span] = parser.parse(in) match {
      case Success(blocks, _) => blocks
      case f: Failure         => List(InvalidSpan(f.message, in))
    }

  }

}
