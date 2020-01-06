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

import laika.bundle.SyntaxHighlighter
import laika.parse.Parser
import laika.parse.code.{CodeCategory, CodeSpan}
import laika.parse.helper.{DefaultParserHelpers, ParseResultHelpers}
import org.scalatest.{Matchers, WordSpec}

/**
  * @author Jens Halm
  */
class CommonSyntaxParserSpec extends WordSpec 
                             with Matchers
                             with ParseResultHelpers
                             with DefaultParserHelpers[Seq[CodeSpan]] {

  
  val defaultParser: Parser[Seq[CodeSpan]] = SyntaxHighlighter.build("test-lang")(Seq(
    Comment.multiLine("/*", "*/"),
    Comment.singleLine("//")
  ) ++ Keywords("foo", "bar", "baz")).parser
  
  
  "The comment parser" should {
    
    "parse a single line comment" in {
      
      val input =
        """line 1
          |line 2 // comment
          |line 3""".stripMargin
      
      Parsing(input) should produce (Seq(
        CodeSpan("line 1\nline 2 "),
        CodeSpan("// comment\n", CodeCategory.Comment),
        CodeSpan("line 3"),
      ))
    }

    "parse a multi-line comment" in {

      val input =
        """line 1 /* moo
          |mar
          |maz */ line 3""".stripMargin

      Parsing(input) should produce (Seq(
        CodeSpan("line 1 "),
        CodeSpan("/* moo\nmar\nmaz */", CodeCategory.Comment),
        CodeSpan(" line 3"),
      ))
    }
    
  }
  
  "The keyword parser" should {
    
    "parse keywords" in {
      val input = "one two foo three"

      Parsing(input) should produce (Seq(
        CodeSpan("one two "),
        CodeSpan("foo", CodeCategory.Keyword),
        CodeSpan(" three"),
      ))
    }
    
    "ignore keywords when they are followed by more characters" in {
      val input = "one two foo1 bar2 three"

      Parsing(input) should produce (Seq(
        CodeSpan("one two foo1 bar2 three")
      ))
    }
    
  }
  
  
}
