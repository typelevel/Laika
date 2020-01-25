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

import laika.ast.{CodeSpan, CodeSpans, ~}
import laika.parse.Parser
import laika.parse.code.{CodeCategory, CodeSpanParser, CodeSpanParsers}
import laika.parse.text.TextParsers.{delimitedBy, literal, success}

/**
  * @author Jens Halm
  */
trait TagBasedFormats {

  import NumberLiteral._

  val comment: CodeSpanParsers = Comment.multiLine("<!--", "-->")

  private val id = Identifier.standard.withIdStartChars('_',':').withIdPartChars('-','.')
  val nameParser: Parser[String] = id.standaloneParser.map(_.content)
  def name(category: CodeCategory): CodeSpanParsers = id.withCategoryChooser(_ => category)

  val ref: CodeSpanParsers =
    CodeSpanParsers(CodeCategory.EscapeSequence, '&') {
      ("#x" ~ DigitParsers.hex.min(1) ~ ";").concat
    } ++
      CodeSpanParsers(CodeCategory.EscapeSequence, '&') {
        ("#" ~ DigitParsers.decimal.min(1) ~ ";").concat
      } ++
      CodeSpanParsers(CodeCategory.Substitution, '&') {
        (nameParser ~ ";").concat
      }

  val string: CodeSpanParsers = StringLiteral.singleLine('\'') ++ StringLiteral.singleLine('"')
  val stringWithEntities: CodeSpanParsers = StringLiteral.singleLine('\'').embed(ref) ++ StringLiteral.singleLine('"').embed(ref)

  case class TagParser(tagCategory: CodeCategory,
                       start: String,
                       end: String,
                       tagName: Parser[String],
                       embedded: Seq[CodeSpanParsers] = Nil) extends CodeSpanParsers {

    val defaultCategories: Set[CodeCategory] = Set(CodeCategory.Tag.Punctuation)

    def embed(childSpans: CodeSpanParsers*): TagParser = {
      copy(embedded = embedded ++ childSpans)
    }
    
    def standaloneParser: Parser[Seq[CodeSpan]] = {
      def codeParser(p: Parser[String], category: CodeCategory): Parser[CodeSpan] = p.map(CodeSpan(_, category))

      val startParser = codeParser(if (start.length > 1) literal(start.tail) else success(""), CodeCategory.Tag.Punctuation)
      val tagNameParser = codeParser(tagName, tagCategory)
      val delim = if (end == "/>") delimitedBy(end).failOn('>') else delimitedBy(end) 

      (startParser ~ tagNameParser ~ EmbeddedCodeSpans.parser(delim, embedded, defaultCategories)).map {
        case startPunct ~ tagNameSpan ~ content =>
          CodeSpans.merge(start.head, Seq(startPunct, tagNameSpan) ++ content :+ CodeSpan(end, defaultCategories), defaultCategories)
      }
    }

    lazy val parsers: Seq[CodeSpanParser] = CodeSpanParsers(start.head)(standaloneParser).parsers

  }

  object TagParser {
    def apply (tagCategory: CodeCategory,
               start: String,
               end: String,
               tagName: String): TagParser =
      new TagParser(tagCategory, start, end, literal(tagName))
  }

  val emptyTag: CodeSpanParsers = TagParser(CodeCategory.Tag.Name, "<", "/>", nameParser).embed(
    stringWithEntities,
    name(CodeCategory.AttributeName)
  )

  val startTag: CodeSpanParsers = TagParser(CodeCategory.Tag.Name, "<", ">", nameParser).embed(
    stringWithEntities,
    name(CodeCategory.AttributeName)
  )

  val endTag: CodeSpanParsers = TagParser(CodeCategory.Tag.Name, "</", ">", nameParser)
  
}
