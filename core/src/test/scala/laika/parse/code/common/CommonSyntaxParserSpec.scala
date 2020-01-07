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

import laika.ast.Code
import laika.bundle.SyntaxHighlighter
import laika.parse.Parser
import laika.parse.text.TextParsers._
import laika.parse.code.{CodeCategory, CodeSpan, CodeSpanParsers}
import laika.parse.helper.{DefaultParserHelpers, ParseResultHelpers}
import org.scalatest.{Matchers, WordSpec}

/**
  * @author Jens Halm
  */
class CommonSyntaxParserSpec extends WordSpec 
                             with Matchers
                             with ParseResultHelpers
                             with DefaultParserHelpers[Seq[CodeSpan]] {

  private val tempIdentifier: CodeSpanParsers = 
    CodeSpanParsers(CodeCategory.Identifier, ('a' to 'z').toSet)(anyIn('a' to 'z', 'A' to 'Z', '0' to '9', '_'))
  
  val defaultParser: Parser[Seq[CodeSpan]] = SyntaxHighlighter.build("test-lang")(
    Comment.multiLine("/*", "*/"),
    Comment.singleLine("//"),
    Keywords("foo", "bar", "baz"),
    tempIdentifier
  ).parser
  
  
  "The comment parser" should {
    
    "parse a single line comment" in {
      
      val input =
        """line1
          |line2 // comment
          |line3""".stripMargin
      
      Parsing(input) should produce (Seq(
        CodeSpan("line1", CodeCategory.Identifier),
        CodeSpan("\n"),
        CodeSpan("line2", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("// comment\n", CodeCategory.Comment),
        CodeSpan("line3", CodeCategory.Identifier),
      ))
    }

    "parse a multi-line comment" in {

      val input =
        """line1 /* moo
          |mar
          |maz */ line3""".stripMargin

      Parsing(input) should produce (Seq(
        CodeSpan("line1", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("/* moo\nmar\nmaz */", CodeCategory.Comment),
        CodeSpan(" "),
        CodeSpan("line3", CodeCategory.Identifier),
      ))
    }
    
  }
  
  "The keyword parser" should {
    
    "parse keywords" in {
      val input = "one foo three"

      Parsing(input) should produce (Seq(
        CodeSpan("one", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("foo", CodeCategory.Keyword),
        CodeSpan(" "),
        CodeSpan("three", CodeCategory.Identifier),
      ))
    }
    
    "ignore keywords when they are followed by more characters" in {
      val input = "one foo1 bar2 four"

      Parsing(input) should produce (Seq(
        CodeSpan("one", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("foo1", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("bar2", CodeCategory.Identifier),
        CodeSpan(" "),
        CodeSpan("four", CodeCategory.Identifier),
      ))
    }
    
  }
  
  
}
