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

import laika.ast.{ CodeSpan, CodeSpans }
import laika.parse.Parser
import laika.parse.builders._
import laika.parse.code.common.Identifier.IdParser
import laika.parse.code.implicits._
import laika.parse.code.{ CodeCategory, CodeSpanParser }
import laika.parse.implicits._
import laika.parse.text.PrefixedParser
import laika.parse.text.TextParsers.{ delimitedBy, literal }

/** Configurable base parsers for tag based formats like HTML or XML.
  *
  * @author Jens Halm
  */
object TagFormats {

  import NumberLiteral._

  private val nameParser: IdParser =
    Identifier.alphaNum.withIdStartChars('_', ':').withIdPartChars('-', '.')

  /** Configurable parser for tags in formats like HTML or XML. */
  class TagParser private[TagFormats] (
      tagCategory: String => CodeCategory,
      start: String,
      end: String,
      tagName: PrefixedParser[String] = nameParser.map(_.content),
      embedded: Seq[CodeSpanParser] = Nil
  ) extends CodeParserBase {

    private val categories: Set[CodeCategory] = Set(CodeCategory.Tag.Punctuation)

    /** Creates a new parser that only recognizes tags with the specified name.
      */
    def forTagName(name: String): TagParser =
      new TagParser(tagCategory, start, end, literal(name), embedded)

    /** Applies the specified category to the result.
      */
    def withCategory(category: CodeCategory): TagParser =
      new TagParser(_ => category, start, end, tagName, embedded)

    /** Applies a category dynamically based on inspecting the name of the tag.
      */
    def withCategory(category: String => CodeCategory): TagParser =
      new TagParser(category, start, end, tagName, embedded)

    /** Embeds the specified parsers for child spans inside a tag.
      *
      * This is usually used for the detection of attribute names or entity references.
      */
    def embed(childSpans: CodeSpanParser*): TagParser = {
      new TagParser(tagCategory, start, end, tagName, embedded ++ childSpans)
    }

    def underlying: PrefixedParser[Seq[CodeSpan]] = {

      val startParser   = literal(start).asCode(CodeCategory.Tag.Punctuation)
      val tagNameParser = tagName.map(name => CodeSpan(name, tagCategory(name)))
      val delim         = if (end == "/>") delimitedBy(end).failOn('>') else delimitedBy(end)

      (startParser ~ tagNameParser ~ EmbeddedCodeSpans.parser(delim, embedded, categories)).mapN {
        (startPunct, tagNameSpan, content) =>
          CodeSpans.merge(Seq(startPunct, tagNameSpan) ++ content :+ CodeSpan(end, categories))
      }
    }

  }

  /** Parses a comment enclosed between `<!--` and `-->`. */
  val comment: CodeSpanParser = Comment.multiLine("<!--", "-->")

  /** Parses a valid attribute, tag or entity name.
    */
  def name(category: CodeCategory): CodeSpanParser = nameParser.withCategory(category)

  /** Parses a named entity reference like `&lt;` or a numeric character reference like `&#xff`.
    */
  val ref: CodeSpanParser =
    CodeSpanParser(CodeCategory.EscapeSequence) {
      ("&#x" ~ digits.hex.min(1) ~ ";").source
    } ++
      CodeSpanParser(CodeCategory.EscapeSequence) {
        ("&#" ~ digits.decimal.min(1) ~ ";").source
      } ++
      CodeSpanParser(CodeCategory.Substitution) {
        ("&" ~ nameParser.map(_.content) ~ ";").source
      }

  val string: CodeSpanParser = StringLiteral.singleLine('\'') ++ StringLiteral.singleLine('"')

  val stringWithEntities: CodeSpanParser =
    StringLiteral.singleLine('\'').embed(ref) ++
      StringLiteral.singleLine('"').embed(ref)

  /** Parses an empty tag (closed by `/>`) with optional attributes. */
  val emptyTag: TagParser = customTag("<", "/>").embed(
    stringWithEntities,
    name(CodeCategory.AttributeName)
  )

  /** Parses a start tag with optional attributes. */
  val startTag: TagParser = customTag("<", ">").embed(
    stringWithEntities,
    name(CodeCategory.AttributeName)
  )

  /** Parses an end tag.
    */
  val endTag: TagParser = customTag("</", ">")

  /** Parses the content and the end tag of an element, with optionally
    * embedding syntax for the content of the element.
    * Assumes that the start tag has already been parsed.
    */
  def elementRest(
      tagName: String,
      embedded: Seq[CodeSpanParser] = Nil,
      tagNameCategory: CodeCategory = CodeCategory.Tag.Name
  ): Parser[Seq[CodeSpan]] = {

    val endTag: Seq[CodeSpan] = Seq(
      CodeSpan("</", CodeCategory.Tag.Punctuation),
      CodeSpan(tagName, tagNameCategory)
    )
    (EmbeddedCodeSpans.parser(delimitedBy(s"</$tagName"), embedded) ~ (ws ~ ">").source).map {
      case content ~ close => content ++ endTag :+ CodeSpan(close, CodeCategory.Tag.Punctuation)
    }
  }

  def apply(tagCategory: CodeCategory, start: String, end: String): TagParser =
    new TagParser(_ => tagCategory, start, end)

  /** Creates a parser for a tag that does not follow the basic format for start or end tags.
    * This can be used for entities like DTD nodes, for example.
    * The specified start and end strings represent the delimiters, e.g. `<!` and `>`
    */
  def customTag(start: String, end: String): TagParser =
    new TagParser(_ => CodeCategory.Tag.Name, start, end)

}
