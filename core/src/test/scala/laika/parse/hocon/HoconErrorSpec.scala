/*
 * Copyright 2012-2019 the original author or authors.
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

package laika.parse.hocon

import laika.config.{ConfigParser, ConfigParserErrors}
import org.scalatest.{Assertion, Matchers, WordSpec}

/**
  * @author Jens Halm
  */
class HoconErrorSpec extends WordSpec with Matchers {
  
  def parseAndValidate(input: String, expectedMessage: String): Assertion = {

    ConfigParser.parse(input).resolve match {
      case Right(result) => fail(s"Unexpected parser success: $result")
      case Left(ConfigParserErrors(errors)) =>
        if (errors.size != 1) println(errors)
        errors.size shouldBe 1
        errors.head.toString shouldBe expectedMessage
      case Left(other) => fail(s"Unexpected parser error: $other")
    }

  }

  "Missing closing quotes" should {

    "be detected in a top level property" in {
      val input =
        """
          |a = "foo bar
          |
          |b = 9
        """.stripMargin
      val expectedMessage =
        """[2.13] failure: Expected closing quote
          |
          |a = "foo bar
          |            ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

    "be detected in an array property" in {
      val input =
        """
          |a = [
          | 3
          | 4
          | "some text
          |] 
          |
          |b = 9
        """.stripMargin
      val expectedMessage =
        """[5.12] failure: Expected closing quote
          |
          | "some text
          |           ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

    "be detected in a nested object property" in {
      val input =
        """
          |a {
          | aa = "some text
          | bb = 7
          |} 
          |
          |b = 9
        """.stripMargin
      val expectedMessage =
        """[3.17] failure: Expected closing quote
          |
          | aa = "some text
          |                ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

    "be detected in a substitution reference" in {
      val input =
        """
          |a = ${"foo.bar}
          |
          |b = 9
        """.stripMargin
      val expectedMessage =
        """[2.16] failure: Invalid key: Expected closing quote
          |
          |a = ${"foo.bar}
          |               ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

    "be detected in a property key" in {
      val input =
        """
          |"a = 7
          |
          |b = 9
        """.stripMargin
      val expectedMessage =
        """[2.7] failure: Invalid key: Expected closing quote
          |
          |"a = 7
          |      ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }
    
  }
  
  "Invalid characters for unquoted strings" should {

    "be detected in a top level property value" in {
      val input =
        """
          |a = foo ? bar
          |
          |b = 9
        """.stripMargin
      val expectedMessage =
        """[2.9] failure: Illegal character in unquoted string, expected delimiters are one of '}', ',', '\n', '#'
          |
          |a = foo ? bar
          |        ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

    "be detected in an array property" in {
      val input =
        """
          |a = [
          | 3
          | 4
          | some ? text
          |] 
          |
          |b = 9
        """.stripMargin
      val expectedMessage =
        """[5.7] failure: Illegal character in unquoted string, expected delimiters are one of ']', ',', '\n', '#'
          |
          | some ? text
          |      ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

    "be detected in a nested object property value" in {
      val input =
        """
          |a {
          | aa = some ? text
          | bb = 7
          |} 
          |
          |b = 9
        """.stripMargin
      val expectedMessage =
        """[3.12] failure: Illegal character in unquoted string, expected delimiters are one of '}', ',', '\n', '#'
          |
          | aa = some ? text
          |           ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }
    
    "be detected in a substitution reference" in {
      val input =
        """
          |a = ${foo = bar}
          |
          |b = 9
        """.stripMargin
      val expectedMessage =
        """[2.11] failure: Invalid key: Illegal character in unquoted string, expected delimiter is '}'
          |
          |a = ${foo = bar}
          |          ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

    "be detected in a property key" in {
      val input =
        """
          |a } c = 7
          |
          |b = 9
        """.stripMargin
      val expectedMessage =
        """[2.3] failure: Invalid key: Illegal character in unquoted string, expected delimiters are one of ':', '=', '{', '+='
          |
          |a } c = 7
          |  ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

    "be detected as a consequence of a missing separator between fields" in {
      val input =
        """
          |a {
          |  b { x = 5 y = 6 }
          |    
          |  c = 9
          |}
          |  
          |d = 7  
          |""".stripMargin
      val expectedMessage =
        """[3.15] failure: Illegal character in unquoted string, expected delimiters are one of '}', ',', '\n', '#'
          |
          |  b { x = 5 y = 6 }
          |              ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }
    
  }
  
  "Invalid escape sequences" should {

    "be detected in a top level property" in {
      val input =
        """
          |a = "foo \x bar"
          |
          |b = 9
        """.stripMargin
      val expectedMessage =
        """[2.11] failure: Invalid escape sequence: \x
          |
          |a = "foo \x bar"
          |          ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

  }
   
  "Missing closing brackets for arrays" should {

    "be detected in a top level property" in {
      val input =
        """
          |a = [3, 4, 5
          |
          |b = 9
          |""".stripMargin
      val expectedMessage =
        """[4.3] failure: Illegal character in unquoted string, expected delimiters are one of ']', ',', '\n', '#'
          |
          |b = 9
          |  ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

    "be detected in a top level property in a multiline array" in {
      val input =
        """
          |a = [
          | 3
          | 4
          | 5
          |
          |b = 9
        """.stripMargin
      val expectedMessage =
        """[7.3] failure: Illegal character in unquoted string, expected delimiters are one of ']', ',', '\n', '#'
          |
          |b = 9
          |  ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

    "be detected in a nested object property" in {
      val input =
        """
          |a {
          |  b = [
          |    3
          |    4
          |    5
          |
          |  c = 9
          |}
          |  
          |d = 7  
        """.stripMargin
      val expectedMessage =
        """[8.5] failure: Illegal character in unquoted string, expected delimiters are one of ']', ',', '\n', '#'
          |
          |  c = 9
          |    ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

  }

  "Missing closing braces for objects" should {

    "be detected in a top level property" in {
      val input =
        """
          |a {
          |  x = 5
          |
          |b = 9
          |""".stripMargin
      val expectedMessage =
        """[6.1] failure: Expected closing brace '}'
          |
          |
          |^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

    "be detected in a multiline array" in {
      val input =
        """
          |a = [
          | { x = 3
          | { x = 4 }
          | { x = 5 }
          |]
          |
          |b = 9
        """.stripMargin
      val expectedMessage =
        """[4.2] failure: Expected closing brace '}'
          |
          | { x = 4 }
          | ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

    "be detected in a nested object property" in {
      val input =
        """
          |a {
          |  b { x = 5
          |    
          |  c = 9
          |}
          |  
          |d = 7  
          |""".stripMargin
      val expectedMessage =
        """[9.1] failure: Expected closing brace '}'
          |
          |
          |^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

  }

  "Missing '=' or ':' between key and value" should {

    "be detected in a top level property" in {
      val input =
        """
          |a 5
          |
          |b = 9
          |""".stripMargin
      val expectedMessage =
        """[2.4] failure: Expected separator after key ('=', '+=', ':' or '{')
          |
          |a 5
          |   ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

    "be detected in a multiline array" in {
      val input =
        """
          |a = [
          | { x 3 }
          | { y = 4 }
          | { z = 5 }
          |]
          |
          |b = 9
        """.stripMargin
      val expectedMessage =
        """[3.8] failure: Expected separator after key ('=', '+=', ':' or '{')
          |
          | { x 3 }
          |       ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

    "be detected in a nested object property" in {
      val input =
        """
          |a {
          |  b { x 5 }
          |    
          |  c = 9
          |}
          |  
          |d = 7  
          |""".stripMargin
      val expectedMessage =
        """[3.11] failure: Expected separator after key ('=', '+=', ':' or '{')
          |
          |  b { x 5 }
          |          ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

  }

  "Missing closing braces for substitution references" should {

    "be detected in a top level property" in {
      val input =
        """
          |a = ${foo.bar
          |
          |b = 9
          |""".stripMargin
      val expectedMessage =
        """[2.14] failure: Invalid key: Illegal character in unquoted string, expected delimiter is '}'
          |
          |a = ${foo.bar
          |             ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

    "be detected in a multiline array" in {
      val input =
        """
          |a = [
          | ${foo.bar
          | 4
          | 5
          |]
          |
          |b = 9
        """.stripMargin
      val expectedMessage =
        """[3.11] failure: Invalid key: Illegal character in unquoted string, expected delimiter is '}'
          |
          | ${foo.bar
          |          ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

    "be detected in a nested object property" in {
      val input =
        """
          |a {
          |  b = ${foo.bar
          |    
          |  c = 9
          |}
          |  
          |d = 7  
          |""".stripMargin
      val expectedMessage =
        """[3.16] failure: Invalid key: Illegal character in unquoted string, expected delimiter is '}'
          |
          |  b = ${foo.bar
          |               ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

  }
  
  "Missing closing triple quotes" should {
    
    "be detected in a top level object" in {
      val input =
        """
          |a = +++foo bar
          |       baz baz
          |       
          |b = 9""".stripMargin.replaceAllLiterally("+", "\"")
      val expectedMessage =
        """[5.6] failure: Expected closing triple quote
          |
          |b = 9
          |     ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

    "be detected in a nested object" in {
      val input =
        """
          |a = {
          |  aa = +++foo bar
          |          baz baz
          |}
          |       
          |b = 9""".stripMargin.replaceAllLiterally("+", "\"")
      val expectedMessage =
        """[7.6] failure: Expected closing triple quote
          |
          |b = 9
          |     ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }
    
  }
  
  "Unsupported include syntax" should {

    "be detected with valid, but unsupported syntax" in {
      val input =
        """
          |include "foo.conf"
          |       
          |b = 9""".stripMargin
      val expectedMessage =
        """[1.1] failure: Processing include instructions is not implemented for the pure parser and will be added later to the laika-io module
          |
          |foo.conf
          |^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }
    
    "be detected with missing closing quotes" in {
      val input =
        """
          |include "foo.conf
          |       
          |b = 9""".stripMargin
      val expectedMessage =
        """[2.18] failure: Expected closing quote
          |
          |include "foo.conf
          |                 ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

    "be detected with missing closing quotes (file syntax)" in {
      val input =
        """
          |include file("foo.conf)
          |       
          |b = 9""".stripMargin
      val expectedMessage =
        """[2.24] failure: Expected closing quote
          |
          |include file("foo.conf)
          |                       ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

    "be detected with missing closing parenthesis (file syntax)" in {
      val input =
        """
          |include file("foo.conf"
          |       
          |b = 9""".stripMargin
      val expectedMessage =
        """[2.24] failure: Expected closing parenthesis
          |
          |include file("foo.conf"
          |                       ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

    "be detected with missing closing parenthesis (required/file syntax)" in {
      val input =
        """
          |include required(file("foo.conf")
          |       
          |b = 9""".stripMargin
      val expectedMessage =
        """[2.34] failure: Expected closing parenthesis
          |
          |include required(file("foo.conf")
          |                                 ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

    "be detected with missing quotes" in {
      val input =
        """
          |include file(foo.conf)
          |       
          |b = 9""".stripMargin
      val expectedMessage =
        """[2.14] failure: Expected quoted string
          |
          |include file(foo.conf)
          |             ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }
    
  }

  
}
