/*
 * Copyright 2013-2016 the original author or authors.
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

package laika.parse.markdown

import laika.api.Transform
import laika.render.HTML
import laika.transform.helper.FileTransformerUtil
import laika.tree.Elements.QuotedBlock
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Codec

/**
 * @author Jens Halm
 */
class MarkdownToHTMLSpec extends FlatSpec 
                         with Matchers
                         with FileTransformerUtil {
  
  implicit val codec: Codec = Codec.UTF8
  
  /** Uses JTidy to remove cosmetic differences between Laika and Markdown output,
   *  plus a few additional, manual cleaning operations for purely cosmetic differences
   *  not covered by JTidy.
   */
  def tidyAndAdjust (html: String): String = {
    val cleaned = html
      .replace("\r\n", "\n")
      .replace("\r", "\n")
      .replaceAll("([^\n])</", "$1\n</") // Markdown often inserts a new line before a closing tag
      .replace(" class=\"arabic\"", "") // Standard Laika HTML renderer adds this class for ordered lists
      .replaceAll("\n[ ]+\n","\n\n") // Markdown removes spaces from blank lines in code blocks
    
    tidy(cleaned).replace(">\n\n<",">\n<") // Markdown often adds blank lines between tags
  }

  def transformAndCompare (name: String): Unit = {
    val path = classPathResource("/markdownTestSuite") + "/" + name
    val actual = Transform
      .from(Markdown).to(HTML)
      .strict.withRawContent
      .rendering { out => {
        case QuotedBlock(content, _, _) => out << "<blockquote>" <<|> content <<| "</blockquote>" // Markdown always writes p tags inside blockquotes
      }}
      .fromFile(path + ".md").toString
    val expected = readFile(path + ".html")
    tidyAndAdjust(actual) should be (tidyAndAdjust(expected))
  }
  
  
  "The official Markdown test suite" should "pass for 'Amps and angle encoding'" in {
    transformAndCompare("Amps and angle encoding")
  }
  
  it should "pass for 'Auto links'" in {
    transformAndCompare("Auto links")
  }
  
  it should "pass for 'Backslash escapes'" in {
    transformAndCompare("Backslash escapes")
  }
  
  it should "pass for 'Blockquotes with code blocks'" in {
    transformAndCompare("Blockquotes with code blocks")
  }
  
  it should "pass for 'Code Blocks'" in {
    transformAndCompare("Code Blocks")
  }
  
  it should "pass for 'Code Spans'" in {
    transformAndCompare("Code Spans")
  }
  
  it should "pass for 'Hard-wrapped paragraphs with list-like lines'" in {
    transformAndCompare("Hard-wrapped paragraphs with list-like lines")
  }
  
  it should "pass for 'Horizontal rules'" in {
    transformAndCompare("Horizontal rules")
  }
  
  it should "pass for 'Inline HTML (Advanced)'" in {
    transformAndCompare("Inline HTML (Advanced)")
  }
  
  it should "pass for 'Inline HTML (Simple)'" in {
    transformAndCompare("Inline HTML (Simple)")
  }
  
  it should "pass for 'Inline HTML comments'" in {
    transformAndCompare("Inline HTML comments")
  }
  
  it should "pass for 'Links, inline style'" in {
    transformAndCompare("Links, inline style")
  }
  
  it should "pass for 'Links, reference style'" in {
    transformAndCompare("Links, reference style")
  }
  
  it should "pass for 'Links, shortcut references'" in {
    transformAndCompare("Links, shortcut references")
  }
  
  it should "pass for 'Literal quotes in titles'" in {
    transformAndCompare("Literal quotes in titles")
  }
  
  it should "pass for 'Nested blockquotes'" in {
    transformAndCompare("Nested blockquotes")
  }
  
  it should "pass for 'Ordered and unordered lists'" in {
    transformAndCompare("Ordered and unordered lists")
  }
  
  it should "pass for 'Strong and em together'" in {
    transformAndCompare("Strong and em together")
  }
  
  it should "pass for 'Tabs'" in {
    transformAndCompare("Tabs")
  }
  
  it should "pass for 'Tidyness'" in {
    transformAndCompare("Tidyness")
  }
  
  
  it should "pass for 'Full Docs - Basics'" in {
    transformAndCompare("Markdown Documentation - Basics")
  }
  
  it should "pass for 'Full Docs - Syntax'" in {
    transformAndCompare("Markdown Documentation - Syntax")
  }
  
  
}
