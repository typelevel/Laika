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
        """[2.13] failure: Expected closing quote
          |
          |a = "foo bar
          |            ^""".stripMargin
      //parseAndValidate(input, expectedMessage) TODO
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
        """[2.8] failure: Illegal character in unquoted string, expected delimiters are one of '}', ',', '\n', '#'
          |
          |a = foo ? bar
          |       ^""".stripMargin
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
        """[5.6] failure: Illegal character in unquoted string, expected delimiters are one of ']', ',', '\n', '#'
          |
          | some ? text
          |     ^""".stripMargin
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
        """[3.11] failure: Illegal character in unquoted string, expected delimiters are one of '}', ',', '\n', '#'
          |
          | aa = some ? text
          |          ^""".stripMargin
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
        """[2.10] failure: Invalid key: Illegal character in unquoted string, expected delimiter is '}'
          |
          |a = ${foo = bar}
          |         ^""".stripMargin
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
        """[2.2] failure: Invalid key: Illegal character in unquoted string, expected delimiters are one of ':', '=', '{', '+'
          |
          |a } c = 7
          | ^""".stripMargin
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
        """.stripMargin
      val expectedMessage =
        """[4.3] failure: Expected closing bracket ']'
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
        """[7.3] failure: Expected closing bracket ']'
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
        """[8.5] failure: Expected closing bracket ']'
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
          | { x = 5
          | 4
          | 5
          |]
          |
          |b = 9
        """.stripMargin
      val expectedMessage =
        """[4.2] failure: Expected closing brace '}'
          |
          | 4
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

  "Missing closing braces for substitution references" should {

    "be detected in a top level property" in {
      val input =
        """
          |a = ${foo.bar
          |
          |b = 9
          |""".stripMargin
      val expectedMessage =
        """[2.14] failure: Expected closing brace '}'
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
        """[3.11] failure: Expected closing brace '}'
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
        """[3.16] failure: Expected closing brace '}'
          |
          |  b = ${foo.bar
          |               ^""".stripMargin
      parseAndValidate(input, expectedMessage)
    }

  }

    //    "report a missing closing triple quote" in {
//      val input =
//        """
//          |a = +++foo bar
//          |       baz baz
//          |       
//          |b = 9
//        """.stripMargin.replaceAllLiterally("+", "\"")
//      val expectedMessage =
//        """[2.13] failure: Expected closing quote
//          |
//          |a = "foo bar
//          |            ^""".stripMargin
//      parseAndValidate(input, expectedMessage)
//    }

  
}
