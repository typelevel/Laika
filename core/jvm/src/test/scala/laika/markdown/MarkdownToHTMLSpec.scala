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

package laika.markdown

import laika.api.Transformer
import laika.ast.*
import laika.config.MessageFilters
import laika.file.FileIO
import laika.format.{ HTML, Markdown }
import laika.html.TidyHTML
import munit.FunSuite

import scala.io.Codec

/** @author Jens Halm
  */
class MarkdownToHTMLSpec extends FunSuite {

  implicit val codec: Codec = Codec.UTF8

  /** Uses JTidy to remove cosmetic differences between Laika and Markdown output,
    *  plus a few additional, manual cleaning operations for purely cosmetic differences
    *  not covered by JTidy.
    */
  def tidyAndAdjust(html: String): String = {
    val cleaned = html
      .replace("\r\n", "\n")
      .replace("\r", "\n")
      .replaceAll("([^\n])</", "$1\n</") // Markdown often inserts a new line before a closing tag
      .replace(
        " class=\"arabic\"",
        ""
      ) // Standard Laika HTML renderer adds this class for ordered lists
      .replaceAll("\n[ ]+\n", "\n\n") // Markdown removes spaces from blank lines in code blocks

    TidyHTML(cleaned).replace(">\n\n<", ">\n<") // Markdown often adds blank lines between tags
  }

  def transformAndCompare(name: String): Unit = {

    def renderPath(relPath: VirtualPath): Target =
      if (relPath == RelativePath.CurrentDocument()) ExternalTarget("")
      else ExternalTarget(relPath.toString)
    val path   = FileIO.classPathResourcePath("/markdownTestSuite") + "/" + name
    val input  = FileIO.readFile(path + ".md")
    val actual = Transformer
      .from(Markdown).to(HTML)
      .strict.withRawContent
      .usingSpanRule { case LinkPathReference(content, relPath, _, title, opt) =>
        Replace(
          SpanLink(content, renderPath(relPath), title, opt)
        ) // We do not validate cross-links in these tests
      }
      .rendering {
        case (fmt, i @ InvalidSpan(_, _, Literal(fb, _), _)) =>
          fmt.child(i.copy(fallback = Text(fb)))
        case (fmt, qb: QuotedBlock)                          =>
          // Markdown always writes p tags inside blockquotes
          fmt.indentedElement("blockquote", qb)
        case (fmt, h @ Header(_, _, Id(_)))                  =>
          fmt.child(h.withOptions(NoOpt)) // Markdown classic does not generate header ids
        case (fmt, t @ Title(_, Id("unordered")))            => fmt.child(Header(2, t.content))
        case (fmt, t @ Title(_, Id(_)))                      => fmt.child(t.withOptions(NoOpt))
      }
      .withMessageFilters(
        MessageFilters.custom(failOn = MessageFilter.None, render = MessageFilter.None)
      )
      .build
      .transform(input)
      .map(tidyAndAdjust)

    val expected = tidyAndAdjust(FileIO.readFile(path + ".html"))

    assertEquals(actual, Right(expected))
  }

  test("pass for 'Amps and angle encoding'") {
    transformAndCompare("Amps and angle encoding")
  }

  test("pass for 'Auto links'") {
    transformAndCompare("Auto links")
  }

  test("pass for 'Backslash escapes'") {
    transformAndCompare("Backslash escapes")
  }

  test("pass for 'Blockquotes with code blocks'") {
    transformAndCompare("Blockquotes with code blocks")
  }

  test("pass for 'Code Blocks'") {
    transformAndCompare("Code Blocks")
  }

  test("pass for 'Code Spans'") {
    transformAndCompare("Code Spans")
  }

  test("pass for 'Hard-wrapped paragraphs with list-like lines'") {
    transformAndCompare("Hard-wrapped paragraphs with list-like lines")
  }

  test("pass for 'Horizontal rules'") {
    transformAndCompare("Horizontal rules")
  }

  test("pass for 'Inline HTML (Advanced)'") {
    transformAndCompare("Inline HTML (Advanced)")
  }

  test("pass for 'Inline HTML (Simple)'") {
    transformAndCompare("Inline HTML (Simple)")
  }

  test("pass for 'Inline HTML comments'") {
    transformAndCompare("Inline HTML comments")
  }

  test("pass for 'Links, inline style'") {
    transformAndCompare("Links, inline style")
  }

  test("pass for 'Links, reference style'") {
    transformAndCompare("Links, reference style")
  }

  test("pass for 'Links, shortcut references'") {
    transformAndCompare("Links, shortcut references")
  }

  test("pass for 'Literal quotes in titles'") {
    transformAndCompare("Literal quotes in titles")
  }

  test("pass for 'Nested blockquotes'") {
    transformAndCompare("Nested blockquotes")
  }

  test("pass for 'Ordered and unordered lists'") {
    transformAndCompare("Ordered and unordered lists")
  }

  test("pass for 'Strong and em together'") {
    transformAndCompare("Strong and em together")
  }

  test("pass for 'Tabs'") {
    transformAndCompare("Tabs")
  }

  test("pass for 'Tidyness'") {
    transformAndCompare("Tidyness")
  }

  test("pass for 'Full Docs - Basics'") {
    transformAndCompare("Markdown Documentation - Basics")
  }

  test("pass for 'Full Docs - Syntax'") {
    transformAndCompare("Markdown Documentation - Syntax")
  }

}
