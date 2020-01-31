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

import laika.ast.{CategorizedCode, CodeSpan, CodeSpans, ~}
import laika.parse.code.{CodeCategory, CodeSpanParser}
import laika.parse.text.PrefixedParser
import laika.parse.text.TextParsers.{delimitedBy, literal}

/** Configurable base parsers for tag based formats like HTML or XML.
  * 
  * @author Jens Halm
  */
trait TagBasedFormats {

  import NumberLiteral._

  /** Parses a comment enclosed between `<!--` and `-->`. */
  val comment: CodeSpanParser = Comment.multiLine("<!--", "-->")

  private val id = Identifier.alphaNum.withIdStartChars('_',':').withIdPartChars('-','.')
  
  private val nameParser: PrefixedParser[String] = id.standaloneParser.map(_.content)

  /** Parses a valid attribute, tag or entity name.
    */
  def name(category: CodeCategory): CodeSpanParser = id.withCategoryChooser(_ => category)

  /** Parses a named entity reference like `&lt;` or a numeric character reference like `&#xff`.
    */
  val ref: CodeSpanParser =
    CodeSpanParser(CodeCategory.EscapeSequence) {
      ("&#x" ~ DigitParsers.hex.min(1) ~ ";").concat
    } ++
      CodeSpanParser(CodeCategory.EscapeSequence) {
        ("&#" ~ DigitParsers.decimal.min(1) ~ ";").concat
      } ++
      CodeSpanParser(CodeCategory.Substitution) {
        ("&" ~ nameParser ~ ";").concat
      }

  val string: CodeSpanParser = StringLiteral.singleLine('\'') ++ StringLiteral.singleLine('"')
  val stringWithEntities: CodeSpanParser = StringLiteral.singleLine('\'').embed(ref) ++ StringLiteral.singleLine('"').embed(ref)

  /** Configurable base parser for tags in formats like HTML or XML. */
  case class TagParser(tagCategory: CodeCategory,
                       start: String,
                       end: String,
                       tagName: PrefixedParser[String] = nameParser,
                       embedded: Seq[CodeSpanParser] = Nil) extends CodeParserBase {

    private val categories: Set[CodeCategory] = Set(CodeCategory.Tag.Punctuation)

    /** Embeds the specified parsers for child spans inside a tag.
      *
      * This is usually used for the detection of attribute names or entity references.
      */
    def embed(childSpans: CodeSpanParser*): TagParser = {
      copy(embedded = embedded ++ childSpans)
    }
    
    def underlying: PrefixedParser[Seq[CodeSpan]] = {
      
      def codeParser(p: PrefixedParser[String], category: CodeCategory): PrefixedParser[CodeSpan] = p.map(CodeSpan(_, category))

      val startParser = codeParser(literal(start), CodeCategory.Tag.Punctuation)
      val tagNameParser = codeParser(tagName, tagCategory)
      val delim = if (end == "/>") delimitedBy(end).failOn('>') else delimitedBy(end) 

      (startParser ~ tagNameParser ~ EmbeddedCodeSpans.parser(delim, embedded, categories)).map {
        case startPunct ~ tagNameSpan ~ content =>
          CodeSpans.merge(Seq(startPunct, tagNameSpan) ++ content :+ CodeSpan(end, categories))
      }
    }

  }

  object TagParser {
    def apply (tagCategory: CodeCategory,
               start: String,
               end: String,
               tagName: String): TagParser =
      new TagParser(tagCategory, start, end, literal(tagName))
  }

  /** Parses an empty tag (closed by `/>`) with optional attributes. */
  val emptyTag: CodeSpanParser = TagParser(CodeCategory.Tag.Name, "<", "/>").embed(
    stringWithEntities,
    name(CodeCategory.AttributeName)
  )

  /** Parses a start tag with optional attributes. */
  val startTag: CodeSpanParser = TagParser(CodeCategory.Tag.Name, "<", ">").embed(
    stringWithEntities,
    name(CodeCategory.AttributeName)
  )

  /** Parses an end tag.
    */
  val endTag: CodeSpanParser = TagParser(CodeCategory.Tag.Name, "</", ">")
  
}
