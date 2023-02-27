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

package laika.parse.hocon

import laika.config.{ ConfigParser, ConfigParserErrors }
import munit.FunSuite

/** @author Jens Halm
  */
class HoconErrorSpec extends FunSuite {

  def run(input: String, expectedMessage: String)(implicit loc: munit.Location): Unit = {

    ConfigParser.parse(input).resolve() match {
      case Right(result)                    => fail(s"Unexpected parser success: $result")
      case Left(ConfigParserErrors(errors)) =>
        assertEquals(errors.size, 1)
        assertEquals(errors.head.toString, expectedMessage)
      case Left(other)                      => fail(s"Unexpected parser error: $other")
    }

  }

  test("missing closing quotes in a top level property") {
    val input           =
      """
        |a = "foo bar
        |
        |b = 9
      """.stripMargin
    val expectedMessage =
      """[2.13] failure: Expected closing '"'
        |
        |a = "foo bar
        |            ^""".stripMargin
    run(input, expectedMessage)
  }

  test("missing closing quotes in an array property") {
    val input           =
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
      """[5.12] failure: Expected closing '"'
        |
        | "some text
        |           ^""".stripMargin
    run(input, expectedMessage)
  }

  test("missing closing quotes in a nested object property") {
    val input           =
      """
        |a {
        | aa = "some text
        | bb = 7
        |} 
        |
        |b = 9
      """.stripMargin
    val expectedMessage =
      """[3.17] failure: Expected closing '"'
        |
        | aa = "some text
        |                ^""".stripMargin
    run(input, expectedMessage)
  }

  test("missing closing quotes in a substitution reference") {
    val input           =
      """
        |a = ${"foo.bar}
        |
        |b = 9
      """.stripMargin
    val expectedMessage =
      """[2.16] failure: Invalid key: Expected closing '"'
        |
        |a = ${"foo.bar}
        |               ^""".stripMargin
    run(input, expectedMessage)
  }

  test("missing closing quotes in a property key") {
    val input           =
      """
        |"a = 7
        |
        |b = 9
      """.stripMargin
    val expectedMessage =
      """[2.7] failure: Invalid key: Expected closing '"'
        |
        |"a = 7
        |      ^""".stripMargin
    run(input, expectedMessage)
  }

  test("invalid characters for unquoted strings in a top level property value") {
    val input           =
      """
        |a = foo ? bar
        |
        |b = 9
      """.stripMargin
    val expectedMessage =
      """[2.9] failure: Illegal character in unquoted string, expected delimiters are one of '#', ',', '\n', '}'
        |
        |a = foo ? bar
        |        ^""".stripMargin
    run(input, expectedMessage)
  }

  test("invalid characters for unquoted strings in an array property") {
    val input           =
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
      """[5.7] failure: Illegal character in unquoted string, expected delimiters are one of '#', ',', '\n', ']'
        |
        | some ? text
        |      ^""".stripMargin
    run(input, expectedMessage)
  }

  test("invalid characters for unquoted strings in a nested object property value") {
    val input           =
      """
        |a {
        | aa = some ? text
        | bb = 7
        |} 
        |
        |b = 9
      """.stripMargin
    val expectedMessage =
      """[3.12] failure: Illegal character in unquoted string, expected delimiters are one of '#', ',', '\n', '}'
        |
        | aa = some ? text
        |           ^""".stripMargin
    run(input, expectedMessage)
  }

  test("invalid characters for unquoted strings in a substitution reference") {
    val input           =
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
    run(input, expectedMessage)
  }

  test("invalid characters for unquoted strings in a property key") {
    val input           =
      """
        |a } c = 7
        |
        |b = 9
      """.stripMargin
    val expectedMessage =
      """[2.3] failure: Invalid key: Illegal character in unquoted string, expected delimiters are one of '+=', ':', '=', '{'
        |
        |a } c = 7
        |  ^""".stripMargin
    run(input, expectedMessage)
  }

  test(
    "invalid characters for unquoted strings as a consequence of a missing separator between fields"
  ) {
    val input           =
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
      """[3.15] failure: Illegal character in unquoted string, expected delimiters are one of '#', ',', '\n', '}'
        |
        |  b { x = 5 y = 6 }
        |              ^""".stripMargin
    run(input, expectedMessage)
  }

  test("invalid escape sequences in a top level property") {
    val input           =
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
    run(input, expectedMessage)
  }

  test("missing closing brackets for arrays in a top level property") {
    val input           =
      """
        |a = [3, 4, 5
        |
        |b = 9
        |""".stripMargin
    val expectedMessage =
      """[4.3] failure: Illegal character in unquoted string, expected delimiters are one of '#', ',', '\n', ']'
        |
        |b = 9
        |  ^""".stripMargin
    run(input, expectedMessage)
  }

  test("missing closing brackets for arrays in a top level property in a multiline array") {
    val input           =
      """
        |a = [
        | 3
        | 4
        | 5
        |
        |b = 9
      """.stripMargin
    val expectedMessage =
      """[7.3] failure: Illegal character in unquoted string, expected delimiters are one of '#', ',', '\n', ']'
        |
        |b = 9
        |  ^""".stripMargin
    run(input, expectedMessage)
  }

  test("missing closing brackets for arrays in a nested object property") {
    val input           =
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
      """[8.5] failure: Illegal character in unquoted string, expected delimiters are one of '#', ',', '\n', ']'
        |
        |  c = 9
        |    ^""".stripMargin
    run(input, expectedMessage)
  }

  test("missing closing brackets for objects in a top level property") {
    val input           =
      """
        |a {
        |  x = 5
        |
        |b = 9
        |""".stripMargin
    val expectedMessage =
      """[6.1] failure: Expected closing '}'
        |
        |
        |^""".stripMargin
    run(input, expectedMessage)
  }

  test("missing closing brackets for objects in a multiline array") {
    val input           =
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
      """[4.2] failure: Expected closing '}'
        |
        | { x = 4 }
        | ^""".stripMargin
    run(input, expectedMessage)
  }

  test("missing closing brackets for objects in a nested object property") {
    val input           =
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
      """[9.1] failure: Expected closing '}'
        |
        |
        |^""".stripMargin
    run(input, expectedMessage)
  }

  test("missing '=' or ':' between key and value in a top level property") {
    val input           =
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
    run(input, expectedMessage)
  }

  test("missing '=' or ':' between key and value in a multiline array") {
    val input           =
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
    run(input, expectedMessage)
  }

  test("missing '=' or ':' between key and value in a nested object property") {
    val input           =
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
    run(input, expectedMessage)
  }

  test("missing closing braces for substitution references in a top level property") {
    val input           =
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
    run(input, expectedMessage)
  }

  test("missing closing braces for substitution references in a multiline array") {
    val input           =
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
    run(input, expectedMessage)
  }

  test("missing closing braces for substitution references in a nested object property") {
    val input           =
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
    run(input, expectedMessage)
  }

  test("missing closing triple quotes in a top level object") {
    val input           =
      """
        |a = +++foo bar
        |       baz baz
        |       
        |b = 9""".stripMargin.replace("+", "\"")
    val expectedMessage =
      """[5.6] failure: Expected closing triple quote
        |
        |b = 9
        |     ^""".stripMargin
    run(input, expectedMessage)
  }

  test("missing closing triple quotes in a nested object") {
    val input           =
      """
        |a = {
        |  aa = +++foo bar
        |          baz baz
        |}
        |       
        |b = 9""".stripMargin.replace("+", "\"")
    val expectedMessage =
      """[7.6] failure: Expected closing triple quote
        |
        |b = 9
        |     ^""".stripMargin
    run(input, expectedMessage)
  }

  test("invalid include syntax - missing closing quotes") {
    val input           =
      """
        |include "foo.conf
        |       
        |b = 9""".stripMargin
    val expectedMessage =
      """[2.18] failure: Expected closing '"'
        |
        |include "foo.conf
        |                 ^""".stripMargin
    run(input, expectedMessage)
  }

  test("invalid include syntax - missing closing quotes (file syntax)") {
    val input           =
      """
        |include file("foo.conf)
        |       
        |b = 9""".stripMargin
    val expectedMessage =
      """[2.24] failure: Expected closing '"'
        |
        |include file("foo.conf)
        |                       ^""".stripMargin
    run(input, expectedMessage)
  }

  test("invalid include syntax - missing closing parenthesis (file syntax)") {
    val input           =
      """
        |include file("foo.conf"
        |       
        |b = 9""".stripMargin
    val expectedMessage =
      """[2.24] failure: Expected closing ')'
        |
        |include file("foo.conf"
        |                       ^""".stripMargin
    run(input, expectedMessage)
  }

  test("invalid include syntax - missing closing parenthesis (required/file syntax)") {
    val input           =
      """
        |include required(file("foo.conf")
        |       
        |b = 9""".stripMargin
    val expectedMessage =
      """[2.34] failure: Expected closing ')'
        |
        |include required(file("foo.conf")
        |                                 ^""".stripMargin
    run(input, expectedMessage)
  }

  test("invalid include syntax - missing quotes") {
    val input           =
      """
        |include file(foo.conf)
        |       
        |b = 9""".stripMargin
    val expectedMessage =
      """[2.14] failure: Expected quoted string
        |
        |include file(foo.conf)
        |             ^""".stripMargin
    run(input, expectedMessage)
  }

}
