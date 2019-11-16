/*
 * Copyright 2013-2018 the original author or authors.
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

package laika.parse.uri

import laika.ast.{ExternalLink, Reverse, Span, Text, ~}
import laika.bundle.{SpanParser, SpanParserBuilder}
import laika.parse.{Failure, Parser, Success}
import laika.parse.text.TextParsers._
import laika.parse.uri.URIParsers.{regName, path, query, fragment, flatten}

/** Parser for inline auto-links, which are urls or email addresses that are recognized and
  * inserted as links into the AST without any surrounding markup delimiters.
  *
  * The parsing of the http or email URIs is based on the corresponding RFCs.
  * See [[URIParsers]] for details.
  *
  * @author Jens Halm
  */
class AutoLinkParsers (reverseMarkupStart: Parser[Any],
                       afterEndMarkup: Parser[Any],
                       stripStartChars: Set[Char],
                       stripEndChars: Set[Char]) {

  private def reverse (offset: Int, p: => Parser[String]): Parser[String] = Parser { in =>
    p.parse(in.reverse.consume(offset)) match {
      case Success(result, _) => Success(result.reverse, in)
      case Failure(msg, _, _) => Failure(msg, in)
    }
  }

  private def trim (p: Parser[(String,String,String)]): Parser[Span] = p >> { res => Parser { in =>
    res match {
      case (start, sep, end) =>
        val startTrimmed = start.dropWhile(stripStartChars)
        val endTrimmed = end.reverse.dropWhile(stripEndChars).reverse
        val uri = startTrimmed + sep + endTrimmed
        val uriWithScheme = if (sep == "@" && !uri.startsWith("mailto:")) "mailto:"+uri else uri
        val nextIn = in.consume(endTrimmed.length - end.length)
        Success(Reverse(startTrimmed.length, ExternalLink(List(Text(uri)), uriWithScheme), Text(sep+endTrimmed)), nextIn)
    }
  }}

  private def uri (reverseParser: Parser[String], forwardParser: Parser[String], separator: String): Parser[Span] =
    trim(reverse(1, reverseParser <~ reverseMarkupStart) ~
      forwardParser <~ lookAhead(eol | afterEndMarkup) ^^ {
        case scheme ~ rest => (scheme, separator, rest)
      })

  /** Parses a standalone HTTP or HTTPS hyperlink (with no surrounding markup).
    */
  lazy val http: SpanParserBuilder = SpanParser.forStartChar(':')
    .standalone(uri("ptth" | "sptth", URIParsers.httpUriNoScheme, ":"))
    .withLowPrecedence

  /** Parses a standalone www hyperlink (with no surrounding markup).
    */
  lazy val www: SpanParserBuilder = SpanParser.forStartChar('.')
    .standalone(uri("www", (regName ~ path ~ opt('?' ~ query) ~ opt('#' ~ fragment)) ^^ flatten, "."))
    .withLowPrecedence

  /** Parses a standalone email address (with no surrounding markup).
    */
  lazy val email: SpanParserBuilder = SpanParser.forStartChar('@').standalone {
    trim(reverse(1, URIParsers.localPart <~ reverseMarkupStart) ~
      URIParsers.domain <~ lookAhead(eol | afterEndMarkup) ^? {
      case local ~ domain if local.nonEmpty && domain.nonEmpty => (local, "@", domain)
    })
  }.withLowPrecedence

}
