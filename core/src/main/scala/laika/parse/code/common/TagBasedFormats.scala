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

import laika.ast.~
import laika.parse.Parser
import laika.parse.code.{CodeCategory, CodeSpan, CodeSpanParsers}
import laika.parse.text.TextParsers.{delimitedBy, literal, success}

/**
  * @author Jens Halm
  */
trait TagBasedFormats {

  import NumberLiteral._

  val comment: CodeSpanParsers = Comment.multiLine("<!--", "-->")

  private val id = Identifier.standard.withIdStartChars('_',':').withIdPartChars('-','.')
  val nameParser: Parser[String] = id.standaloneParser.map(_.content)
  def name(category: CodeCategory): CodeSpanParsers = id.withCategoryChooser(_ => category).build

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

  val string: CodeSpanParsers = StringLiteral.singleLine('\'').build ++ StringLiteral.singleLine('"').build
  val stringWithEntities: CodeSpanParsers = StringLiteral.singleLine('\'').embed(ref).build ++ StringLiteral.singleLine('"').embed(ref).build

  case class TagParser(tagCategory: CodeCategory,
                       start: String,
                       end: String,
                       tagName: Parser[String],
                       embedded: Seq[CodeSpanParsers] = Nil) extends EmbeddedCodeSpans {

    val defaultCategories: Set[CodeCategory] = Set(CodeCategory.XML.Punctuation)

    def embed(childSpans: CodeSpanParsers*): TagParser = {
      copy(embedded = embedded ++ childSpans)
    }
    
    def standaloneParser: Parser[Seq[CodeSpan]] = {
      def codeParser(p: Parser[String], category: CodeCategory): Parser[CodeSpan] = p.map(CodeSpan(_, category))

      val startParser = codeParser(if (start.length > 1) literal(start.tail) else success(""), CodeCategory.XML.Punctuation)
      val tagNameParser = codeParser(tagName, tagCategory)
      val delim = if (end == "/>") delimitedBy(end).failOn('>') else delimitedBy(end) 

      (startParser ~ tagNameParser ~ contentParser(delim)).map {
        case startPunct ~ tagNameSpan ~ content =>
          mergeCodeSpans(start.head, Seq(startPunct, tagNameSpan) ++ content :+ CodeSpan(end, defaultCategories))
      }
    }

    def build: CodeSpanParsers = CodeSpanParsers(start.head)(standaloneParser)

  }

  object TagParser {
    def apply (tagCategory: CodeCategory,
               start: String,
               end: String,
               tagName: String): TagParser =
      new TagParser(tagCategory, start, end, literal(tagName))
  }

  val emptyTag: CodeSpanParsers = TagParser(CodeCategory.XML.TagName, "<", "/>", nameParser).embed(
    stringWithEntities,
    name(CodeCategory.AttributeName)
  ).build

  val startTag: CodeSpanParsers = TagParser(CodeCategory.XML.TagName, "<", ">", nameParser).embed(
    stringWithEntities,
    name(CodeCategory.AttributeName)
  ).build

  val endTag: CodeSpanParsers = TagParser(CodeCategory.XML.TagName, "</", ">", nameParser).build
  
}
