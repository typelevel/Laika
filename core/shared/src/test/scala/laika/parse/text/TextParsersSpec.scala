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

package laika.parse.text

import laika.parse.Parser
import laika.parse.builders._
import munit.FunSuite
   
class TextParsersSpec extends FunSuite {


  def run[A] (parser: Parser[A], input: String, expected: A): Unit =
    assertEquals(parser.parse(input).toEither, Right(expected))

  def expectFailure[A] (parser: Parser[A], input: String): Unit =
    assert(parser.parse(input).toEither.isLeft)


  test("prevNot - succeed if the previous character is not one of those specified") {
    run(oneChar ~> prevNot('x'), "abc", ())
  }

  test("prevNot - fail if the previous character is one of those specified") {
    expectFailure(oneChar ~> prevNot('a'), "abc")
  }

  test("prevNot - succeed at the start of the input") {
    run(prevNot('x'), "abc", ())
  }

  test("prevNot - succeed if the predicate is not satisfied") {
    run(oneChar ~> prevNot(_ == 'b'), "abc", ())
  }

  test("prevNot - fail if the the predicate is satisfied") {
    (oneChar ~> prevNot(_ == 'a'), "abc")
  }


  test("nextNot - succeed if the next character is not one of those specified") {
    run(oneChar ~> nextNot('x'), "abc", ())
  }

  test("nextNot - fail if the next character is one of those specified") {
    expectFailure(oneChar ~> nextNot('b'), "abc")
  }

  test("nextNot - succeed at the end of the input") {
    run(oneChar ~> nextNot('x'), "a", ())
  }

  test("nextNot - succeed if the predicate is not satisfied") {
    run(oneChar ~> nextNot(_ == 'a'), "abc", ())
  }

  test("nextNot - fail if the the predicate is satisfied") {
    expectFailure(oneChar ~> nextNot(_ == 'b'), "abc")
  }


  test("prevIn - succeed if the previous character is one of those specified") {
    run(oneChar ~> prevIn('a', 'c'), "abc", ())
  }

  test("prevIn - fail if the previous character is not one of those specified") {
    expectFailure(oneChar ~> prevIn('x', 'y'), "abc")
  }

  test("prevIn - fail at the start of the input") {
    expectFailure(prevIn('x'), "abc")
  }

  test("prevIn - succeed if the predicate is satisfied") {
    run(oneChar ~> prevIs(_ == 'a'), "abc", ())
  }

  test("prevIn - fail if the the predicate is not satisfied") {
    expectFailure(oneChar ~> prevIs(_ == 'b'), "abc")
  }


  test("nextIn - succeed if the next character is one of those specified") {
    run(oneChar ~> nextIn('b'), "abc", ())
  }

  test("nextIn - fail if the next character is not one of those specified") {
    expectFailure(oneChar ~> nextIn('x'), "abc")
  }

  test("nextIn - fail at the end of the input") {
    expectFailure(oneChar ~> nextIn('x'), "a")
  }

  test("nextIs - succeed if the predicate is satisfied") {
    run(oneChar ~> nextIs(_ == 'b'), "abc", ())
  }

  test("nextIs - fail if the the predicate is not satisfied") {
    expectFailure(oneChar ~> nextIs(_ == 'a'), "abc")
  }


  test("literal - succeed with a matching string literal") {
    run(literal("abc"), "abcd", "abc")
  }

  test("literal - fail when the string literal does not match") {
    expectFailure(literal("bcd"), "abcd")
  }


  test("eol - succeed for \\n") {
    run(eol, "\naaa", ())
  }

  test("eol - succeed for \\r\\n") {
    run(eol, "\r\naaa", ())
  }

  test("eol - succeed at the end of the input") {
    run(eol, "", ())
  }

  test("eol - fail when not at the end of a line") {
    expectFailure(eol, "abc")
  }


  test("eof - succeed at the end of the input") {
    run(eof, "", "")
  }

  test("eof - fail when not at the end of the input") {
    expectFailure(eof, "\n")
  }


  test("atStart - succeed at the start of the input") {
    run(atStart, "abc", ())
  }

  test("atStart - fail when not at the start of the input") {
  }

  test("ws - succeed with all whitespace characters") {
    run(ws, " \t abcd", " \t ")
  }

  test("ws - succeed with an empty string in case of non-whitespace characters") {
    run(ws, "abcd", "")
  }

  test("wsEol - succeed with whitespace characters followed by newline") {
    run(wsEol, " \n abcd", ())
  }

  test("wsEol - succeed with just a newline character since whitespace is optional") {
    run(wsEol, "\n abcd", ())
  }

  test("wsEol - fail with non-whitespace chacracters") {
    expectFailure(wsEol, "abcd")
  }


  test("blankLine - succeed with whitespace characters followed by newline") {
    run(blankLine, " \n abcd", "")
  }

  test("blankLine - succeed with just a newline character since whitespace is optional") {
    run(blankLine, "\n abcd", "")
  }

  test("blankLine - fail with non-whitespace chacracters") {
    expectFailure(blankLine, "abcd")
  }


  test("blankLines - succeed with one blank line") {
    run(blankLines, " \n abcd", List(""))
  }

  test("blankLines - succeed with three blank lines") {
    run(blankLines, " \n\n \n abcd", List("", "", ""))
  }

  test("blankLines - fail with non-whitespace chacracters") {
    expectFailure(blankLines, "abcd")
  }


  test("restOfLine - succeed with all characters of the current line") {
    run(restOfLine, " aa\nabcd", " aa")
  }

  test("restOfLine - succeed with an empty result") {
    run(restOfLine, "\nabcd", "")
  }


  test("textLine - succeed with all characters of the current line") {
    run(textLine, " aa\nabcd", " aa")
  }

  test("textLine - fail on an empty line") {
    expectFailure(textLine, "\nabcd")
  }


  test("anyChars - always succeed consuming the entire input") {
    run(anyChars, "abcde $&", "abcde $&")
  }

  test("anyChars - only consume the specified maximum number of characters") {
    run(anyChars.max(3), "abcde $&", "abc")
  }


  test("anyOf - succeed with an empty result when no characters match") {
    run(anyOf('x'), "ababccab", "")
  }

  test("anyOf - succeed for the matching character when 1 character is specified") {
    run(anyOf('x'), "xxabc", "xx")
  }

  test("anyOf - succeed for all matching characters when 3 characters are specified") {
    run(anyOf('x', 'y', 'z'), "xxyzxabc", "xxyzx")
  }

  test("anyOf - succeed in case the end of the input is reached") {
    run(anyOf('x', 'y', 'z'), "xxyzx", "xxyzx")
  }

  test("anyOf - fail when it does not consume the specified minimum number of characters") {
    expectFailure(anyOf('x').min(3), "xxabc")
  }

  test("anyOf - succeed when it does consume the specified minimum number of characters") {
    run(anyOf('x').min(3), "xxxxabc", "xxxx")
  }

  test("anyOf - stop, but still succeed, when it has consumed the specified maximum number of characters") {
    run(anyOf('x').max(3), "xxxxxx", "xxx")
  }


  test("someOf - succeed for the matching character when 1 character is specified") {
    run(someOf('x'), "xxabc", "xx")
  }

  test("someOf - succeed for all matching characters when 3 characters are specified") {
    run(someOf('x', 'y', 'z'), "xxyzxabc", "xxyzx")
  }

  test("someOf - succeed in case the end of the input is reached") {
    run(someOf('x', 'y', 'z'), "xxyzx", "xxyzx")
  }

  test("someOf - fail when it does not consume the specified minimum number of characters") {
    expectFailure(someOf('x').min(3), "xxabc")
  }

  test("someOf - fail when it does not consume any characters as min(1) is implicit in someOf parsers") {
    expectFailure(someOf('x'), "abcde")
  }

  test("someOf - succeed when it does consume the specified minimum number of characters") {
    run(someOf('x').min(3), "xxxxabc", "xxxx")
  }

  test("someOf - stop, but still succeed, when it has consumed the specified maximum number of characters") {
    run(someOf('x').max(3), "xxxxxx", "xxx")
  }


  test("oneOf - succeed for the matching character when 1 character is specified") {
    run(TextParsers.oneOf('x'), "xxabc", "x")
  }

  test("oneOf - succeed for any of the specified 3 characters") {
    run(TextParsers.oneOf('x', 'y', 'z'), "yzxabc", "y")
  }

  test("oneOf - fail in case the end of the input is reached") {
    expectFailure(TextParsers.oneOf('x', 'y', 'z'), "")
  }

  test("oneOf - fail when the first character does not match") {
    expectFailure(TextParsers.oneOf('x'), "yxxabc")
  }


  test("anyNot - succeed for all non-matching characters when 1 character is specified") {
    run(anyNot('x'), "abcxxabc", "abc")
  }

  test("anyNot - succeed for all non-matching characters when 3 characters are specified") {
    run(anyNot('x', 'y', 'z'), "abczyxabc", "abc")
  }

  test("anyNot - succeed in case the end of the input is reached") {
    run(anyNot('x', 'y', 'z'), "abcabc", "abcabc")
  }

  test("anyNot - fail when it does not consume the specified minimum number of characters") {
    expectFailure(anyNot('x').min(3), "abxx")
  }

  test("anyNot - succeed when it does consume the specified minimum number of characters") {
    run(anyNot('x').min(3), "abcdxxxx", "abcd")
  }

  test("anyNot - stop, but still succeed, when it has consumed the specified maximum number of characters") {
    run(anyNot('x').max(3), "abcdxxxx", "abc")
  }


  test("someNot - succeed for all non-matching characters when 1 character is specified") {
    run(someNot('x'), "abcxxabc", "abc")
  }

  test("someNot - succeed for all non-matching characters when 3 characters are specified") {
    run(someNot('x', 'y', 'z'), "abczyxabc", "abc")
  }

  test("someNot - succeed in case the end of the input is reached") {
    run(someNot('x', 'y', 'z'), "abcabc", "abcabc")
  }

  test("someNot - fail when it does not consume the specified minimum number of characters") {
    expectFailure(someNot('x').min(3), "abxx")
  }

  test("someNot - fail when it does not consume any characters as min(1) is implicit in someNot parsers") {
    expectFailure(someNot('a', 'b'), "abcde")
  }

  test("someNot - succeed when it does consume the specified minimum number of characters") {
    run(someNot('x').min(3), "abcdxxxx", "abcd")
  }

  test("someNot - stop, but still succeed, when it has consumed the specified maximum number of characters") {
    run(someNot('x').max(3), "abcdxxxx", "abc")
  }


  test("oneNot - succeed for a non-matching character when 1 character is specified") {
    run(TextParsers.oneNot('a'), "xxabc", "x")
  }

  test("oneNot - succeed for any of the specified 3 characters") {
    run(TextParsers.oneNot('a', 'b', 'c'), "yzxabc", "y")
  }

  test("oneNot - fail in case the end of the input is reached") {
    expectFailure(TextParsers.oneNot('x', 'y', 'z'), "")
  }

  test("oneNot - fail when the first character does not match") {
    expectFailure(TextParsers.oneNot('x', 'y'), "yxxabc")
  }


  test("anyOf - succeed for any character within the specified range when 1 range is specified") {
    run(anyOf(range('a', 'd')), "abcde $&", "abcd")
  }

  test("anyOf - succeed for any character within the specified ranges when 2 ranges are specified") {
    run(anyOf(range('a', 'd') ++ range('x', 'z')), "abcdxyzff", "abcdxyz")
  }

  test("anyOf - succeed in case the end of the input is reached") {
    run(anyOf(range('a', 'd')), "abcabd", "abcabd")
  }


  test("anyWhile - succeed with an empty result when no characters match") {
    run(anyWhile(_ < 'd'), "xyzzyw", "")
  }

  test("anyWhile - succeed as long as the specified condition is met") {
    run(anyWhile(_ < 'd'), "abcde $&", "abc")
  }

  test("anyWhile - succeed in case the end of the input is reached") {
    run(anyWhile(_ < 'd'), "abcabc", "abcabc")
  }

  test("anyWhile - fail when it does not consume the specified minimum number of characters") {
    expectFailure(anyWhile(_ < 'd').min(3), "abxx")
  }

  test("anyWhile - succeed when it does consume the specified minimum number of characters") {
    run(anyWhile(_ < 'd').min(3), "abcdxxxx", "abc")
  }

  test("anyWhile - stop, but still succeed, when it has consumed the specified maximum number of characters") {
    run(anyWhile(_ < 'd').max(2), "abcdxxxx", "ab")
  }


  test("someWhile - succeed as long as the specified condition is met") {
    run(someWhile(_ < 'd'), "abcde $&", "abc")
  }

  test("someWhile - succeed in case the end of the input is reached") {
    run(someWhile(_ < 'd'), "abcabc", "abcabc")
  }

  test("someWhile - fail when it does not consume the specified minimum number of characters") {
    expectFailure(someWhile(_ < 'd').min(3), "abxx")
  }

  test("someWhile - fail when it does not consume any characters as min(1) is implicit in someNot parsers") {
    expectFailure(someWhile(_ < 'd'), "xxyyzz")
  }

  test("someWhile - succeed when it does consume the specified minimum number of characters") {
    run(someWhile(_ < 'd').min(3), "abcdxxxx", "abc")
  }

  test("someWhile - stop, but still succeed, when it has consumed the specified maximum number of characters") {
    run(someWhile(_ < 'd').max(2), "abcdxxxx", "ab")
  }


  test("oneIf - succeed as long as the specified condition is met") {
    run(oneIf(_ < 'd'), "abcde $&", "a")
  }

  test("oneIf - fail in case the end of the input is reached") {
    expectFailure(oneIf(_ < 'd'), "")
  }

  test("oneIf - fail in case the specified predicate is not met") {
    expectFailure(oneIf(_ < 'd'), "zxxxyyyz")
  }


  private val az = delimitedBy(CharGroup.lowerAlpha)

  test("delimitedBy - stop as soon as one of the specified delimiter characters is seen") {
    run(az, "123abc", "123")
  }

  test("delimitedBy - succeed even when the result is empty") {
    run(az, "abc", "")
  }

  test("delimitedBy - fail when the result is empty but nonEmpty is specified") {
    expectFailure(az.nonEmpty, "abc")
  }

  test("delimitedBy - fail when a stop char is seen before the end delimiter") {
    expectFailure(az.failOn('3', '4'), "1234abcd")
  }

  test("delimitedBy - fail in case the end of the input is reached before seeing a delimiter character") {
    expectFailure(az, "1234567")
  }


  private val lit = delimitedBy(">>>")

  test("delimitedBy - stop as soon as the specified string delimiter is seen") {
    run(lit, "123>>>", "123")
  }

  test("delimitedBy - succeed even when the result is empty") {
    run(lit, ">>>", "")
  }

  test("delimitedBy - fail when the result is empty but nonEmpty is specified") {
    expectFailure(lit.nonEmpty, ">>>")
  }

  test("delimitedBy - fail when a stop char is seen before the end delimiter") {
    expectFailure(lit.failOn('3', '4'), "1234>>>")
  }

  test("delimitedBy - fail in case the end of the input is reached before seeing the delimiter string") {
    expectFailure(lit, "1234567")
  }

  test("delimitedBy - succeed when the specified post condition is met") {
    import laika.parse.implicits._
    run(delimitedBy(">>>" <~ ws.min(1)), "123>>> ", "123")
  }

  test("delimitedBy - fail when the specified post condition is not met") {
    import laika.parse.implicits._
    expectFailure(delimitedBy(">>>" <~ ws.min(1)), "123>>>A")
  }

}
