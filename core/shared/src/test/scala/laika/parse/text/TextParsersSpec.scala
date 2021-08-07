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

import laika.parse.builders._
import laika.parse.helper.MigrationSpec
import laika.parse.{Parser, SourceCursor}
import org.scalatest.Assertion
   
class TextParsersSpec extends MigrationSpec {


  def run[A] (parser: Parser[A], input: String, expected: A): Assertion =
    assertEquals(parser.parse(input).toEither, Right(expected))

  def expectFailure[A] (parser: Parser[A], input: String): Assertion =
    assert(parser.parse(input).toEither.isLeft)
  

  "The prevNot validators" should {
    
    "succeed if the previous character is not one of those specified" in {
      run(oneChar ~> prevNot('x'), "abc", ())
    }

    "fail if the previous character is one of those specified" in {
      expectFailure(oneChar ~> prevNot('a'), "abc")
    }
    
    "succeed at the start of the input" in {
      run(prevNot('x'), "abc", ())
    }

    "succeed if the predicate is not satisfied" in {
      run(oneChar ~> prevNot(_ == 'b'), "abc", ())
    }

    "fail if the the predicate is satisfied" in {
      (oneChar ~> prevNot(_ == 'a'), "abc")
    }

  }

  "The nextNot validators" should {

    "succeed if the next character is not one of those specified" in {
      run(oneChar ~> nextNot('x'), "abc", ())
    }

    "fail if the next character is one of those specified" in {
      expectFailure(oneChar ~> nextNot('b'), "abc")
    }

    "succeed at the end of the input" in {
      run(oneChar ~> nextNot('x'), "a", ())
    }

    "succeed if the predicate is not satisfied" in {
      run(oneChar ~> nextNot(_ == 'a'), "abc", ())
    }

    "fail if the the predicate is satisfied" in {
      expectFailure(oneChar ~> nextNot(_ == 'b'), "abc")
    }

  }

  "The prevIn and prevIs validators" should {

    "succeed if the previous character is one of those specified" in {
      run(oneChar ~> prevIn('a','c'), "abc", ())
    }

    "fail if the previous character is not one of those specified" in {
      expectFailure(oneChar ~> prevIn('x','y'), "abc")
    }

    "fail at the start of the input" in {
      expectFailure(prevIn('x'), "abc")
    }

    "succeed if the predicate is satisfied" in {
      run(oneChar ~> prevIs(_ == 'a'), "abc", ())
    }

    "fail if the the predicate is not satisfied" in {
      expectFailure(oneChar ~> prevIs(_ == 'b'), "abc")
    }

  }

  "The nextIn and nextIs validators" should {

    "succeed if the next character is one of those specified" in {
      run(oneChar ~> nextIn('b'), "abc", ())
    }

    "fail if the next character is not one of those specified" in {
      expectFailure(oneChar ~> nextIn('x'), "abc")
    }

    "fail at the end of the input" in {
      expectFailure(oneChar ~> nextIn('x'), "a")
    }

    "succeed if the predicate is satisfied" in {
      run(oneChar ~> nextIs(_ == 'b'), "abc", ())
    }

    "fail if the the predicate is not satisfied" in {
      expectFailure(oneChar ~> nextIs(_ == 'a'), "abc")
    }

  }

  "The literal parser" should {

    "succeed with a matching string literal" in {
      run(literal("abc"), "abcd", "abc")
    }

    "fail when the string literal does not match" in {
      expectFailure(literal("bcd"), "abcd")
    }

  }

  "The eol parser" should {

    "succeed for \\n" in {
      run(eol, "\naaa", ())
    }

    "succeed for \\r\\n" in {
      run(eol, "\r\naaa", ())
    }

    "succeed at the end of the input" in {
      run(eol, "", ())
    }

    "fail when not at the end of a line" in {
      expectFailure(eol, "abc")
    }

  }

  "The eof parser" should {

    "succeed at the end of the input" in {
      run(eof, "", "")
    }

    "fail when not at the end of the input" in {
      expectFailure(eof, "\n")
    }

  }

  "The atStart parser" should {

    "succeed at the start of the input" in {
      run(atStart, "abc", ())
    }

    "fail when not at the start of the input" in {
      assert(atStart.parse(SourceCursor("abc").consume(1)).toEither.isLeft)
    }

  }

  "The ws parser" should {

    "succeed with all whitespace characters" in {
      run(ws, " \t abcd", " \t ")
    }

    "succeed with an empty string in case of non-whitespace characters" in {
      run(ws, "abcd", "")
    }

  }

  "The wsEol parser" should {

    "succeed with whitespace characters followed by newline" in {
      run(wsEol, " \n abcd", ())
    }

    "succeed with just a newline character since whitespace is optional" in {
      run(wsEol, "\n abcd", ())
    }

    "fail with non-whitespace chacracters" in {
      expectFailure(wsEol, "abcd")
    }

  }

  "The blankLine parser" should {

    "succeed with whitespace characters followed by newline" in {
      run(blankLine, " \n abcd", "")
    }

    "succeed with just a newline character since whitespace is optional" in {
      run(blankLine, "\n abcd", "")
    }

    "fail with non-whitespace chacracters" in {
      expectFailure(blankLine, "abcd")
    }

  }

  "The blankLines parser" should {

    "succeed with one blank line" in {
      run(blankLines, " \n abcd", List(""))
    }

    "succeed with three blank lines" in {
      run(blankLines, " \n\n \n abcd", List("","",""))
    }

    "fail with non-whitespace chacracters" in {
      expectFailure(blankLines, "abcd")
    }

  }

  "The restOfLine parser" should {

    "succeed with all characters of the current line" in {
      run(restOfLine, " aa\nabcd", " aa")
    }

    "succeed with an empty result" in {
      run(restOfLine, "\nabcd", "")
    }

  }

  "The textLine parser" should {

    "succeed with all characters of the current line" in {
      run(textLine, " aa\nabcd", " aa")
    }

    "fail on an empty line" in {
      expectFailure(textLine, "\nabcd")
    }

  }

  "The anyChars parser" should {

    "always succeed consuming the entire input" in {
      run(anyChars, "abcde $&", "abcde $&")
    }

    "only consume the specified maximum number of characters" in {
      run(anyChars.max(3), "abcde $&", "abc")
    }
  }

  "The anyOf parser" should {

    "succeed with an empty result when no characters match" in {
      run(anyOf('x'), "ababccab", "")
    }

    "succeed for the matching character when 1 character is specified" in {
      run(anyOf('x'), "xxabc", "xx")
    }

    "succeed for all matching characters when 3 characters are specified" in {
      run(anyOf('x','y','z'), "xxyzxabc", "xxyzx")
    }

    "succeed in case the end of the input is reached" in {
      run(anyOf('x','y','z'), "xxyzx", "xxyzx")
    }

    "fail when it does not consume the specified minimum number of characters" in {
      expectFailure(anyOf('x').min(3), "xxabc")
    }

    "succeed when it does consume the specified minimum number of characters" in {
      run(anyOf('x').min(3), "xxxxabc", "xxxx")
    }

    "stop, but still succeed, when it has consumed the specified maximum number of characters" in {
      run(anyOf('x').max(3), "xxxxxx", "xxx")
    }

  }

  "The someOf parser" should {

    "succeed for the matching character when 1 character is specified" in {
      run(someOf('x'), "xxabc", "xx")
    }

    "succeed for all matching characters when 3 characters are specified" in {
      run(someOf('x','y','z'), "xxyzxabc", "xxyzx")
    }

    "succeed in case the end of the input is reached" in {
      run(someOf('x','y','z'), "xxyzx", "xxyzx")
    }

    "fail when it does not consume the specified minimum number of characters" in {
      expectFailure(someOf('x').min(3), "xxabc")
    }

    "fail when it does not consume any characters as min(1) is implicit in someOf parsers" in {
      expectFailure(someOf('x'), "abcde")
    }

    "succeed when it does consume the specified minimum number of characters" in {
      run(someOf('x').min(3), "xxxxabc", "xxxx")
    }

    "stop, but still succeed, when it has consumed the specified maximum number of characters" in {
      run(someOf('x').max(3), "xxxxxx", "xxx")
    }

  }

  "The oneOf parser" should {

    "succeed for the matching character when 1 character is specified" in {
      run(TextParsers.oneOf('x'), "xxabc", "x")
    }

    "succeed for any of the specified 3 characters" in {
      run(TextParsers.oneOf('x','y','z'), "yzxabc", "y")
    }

    "fail in case the end of the input is reached" in {
      expectFailure(TextParsers.oneOf('x','y','z'), "")
    }

    "fail when the first character does not match" in {
      expectFailure(TextParsers.oneOf('x'), "yxxabc")
    }

  }
  
  "The anyNot parser" should {

    "succeed for all non-matching characters when 1 character is specified" in {
      run(anyNot('x'), "abcxxabc", "abc")
    }

    "succeed for all non-matching characters when 3 characters are specified" in {
      run(anyNot('x','y','z'), "abczyxabc", "abc")
    }

    "succeed in case the end of the input is reached" in {
      run(anyNot('x','y','z'), "abcabc", "abcabc")
    }

    "fail when it does not consume the specified minimum number of characters" in {
      expectFailure(anyNot('x').min(3), "abxx")
    }
    
    "succeed when it does consume the specified minimum number of characters" in {
      run(anyNot('x').min(3), "abcdxxxx", "abcd")
    }

    "stop, but still succeed, when it has consumed the specified maximum number of characters" in {
      run(anyNot('x').max(3), "abcdxxxx", "abc")
    }

  }

  "The someNot parser" should {

    "succeed for all non-matching characters when 1 character is specified" in {
      run(someNot('x'), "abcxxabc", "abc")
    }

    "succeed for all non-matching characters when 3 characters are specified" in {
      run(someNot('x','y','z'), "abczyxabc", "abc")
    }

    "succeed in case the end of the input is reached" in {
      run(someNot('x','y','z'), "abcabc", "abcabc")
    }

    "fail when it does not consume the specified minimum number of characters" in {
      expectFailure(someNot('x').min(3), "abxx")
    }

    "fail when it does not consume any characters as min(1) is implicit in someNot parsers" in {
      expectFailure(someNot('a','b'), "abcde")
    }

    "succeed when it does consume the specified minimum number of characters" in {
      run(someNot('x').min(3), "abcdxxxx", "abcd")
    }

    "stop, but still succeed, when it has consumed the specified maximum number of characters" in {
      run(someNot('x').max(3), "abcdxxxx", "abc")
    }

  }

  "The oneNot parser" should {

    "succeed for a non-matching character when 1 character is specified" in {
      run(TextParsers.oneNot('a'), "xxabc", "x")
    }

    "succeed for any of the specified 3 characters" in {
      run(TextParsers.oneNot('a','b','c'), "yzxabc", "y")
    }

    "fail in case the end of the input is reached" in {
      expectFailure(TextParsers.oneNot('x','y','z'), "")
    }

    "fail when the first character does not match" in {
      expectFailure(TextParsers.oneNot('x','y'), "yxxabc")
    }

  }
  
  "The anyOf parser with the range builder" should {

    "succeed for any character within the specified range when 1 range is specified" in {
      run(anyOf(range('a', 'd')), "abcde $&", "abcd")
    }

    "succeed for any character within the specified ranges when 2 ranges are specified" in {
      run(anyOf(range('a', 'd') ++ range('x', 'z')), "abcdxyzff", "abcdxyz")
    }

    "succeed in case the end of the input is reached" in {
      run(anyOf(range('a', 'd')), "abcabd", "abcabd")
    }

  }
  
  "The anyWhile parser" should {

    "succeed with an empty result when no characters match" in {
      run(anyWhile(_ < 'd'), "xyzzyw", "")
    }
    
    "succeed as long as the specified condition is met" in {
      run(anyWhile(_ < 'd'), "abcde $&", "abc")
    }

    "succeed in case the end of the input is reached" in {
      run(anyWhile(_ < 'd'), "abcabc", "abcabc")
    }

    "fail when it does not consume the specified minimum number of characters" in {
      expectFailure(anyWhile(_ < 'd').min(3), "abxx")
    }

    "succeed when it does consume the specified minimum number of characters" in {
      run(anyWhile(_ < 'd').min(3), "abcdxxxx", "abc")
    }

    "stop, but still succeed, when it has consumed the specified maximum number of characters" in {
      run(anyWhile(_ < 'd').max(2), "abcdxxxx", "ab")
    }

  }

  "The someWhile parser" should {

    "succeed as long as the specified condition is met" in {
      run(someWhile(_ < 'd'), "abcde $&", "abc")
    }

    "succeed in case the end of the input is reached" in {
      run(someWhile(_ < 'd'), "abcabc", "abcabc")
    }

    "fail when it does not consume the specified minimum number of characters" in {
      expectFailure(someWhile(_ < 'd').min(3), "abxx")
    }

    "fail when it does not consume any characters as min(1) is implicit in someNot parsers" in {
      expectFailure(someWhile(_ < 'd'), "xxyyzz")
    }

    "succeed when it does consume the specified minimum number of characters" in {
      run(someWhile(_ < 'd').min(3), "abcdxxxx", "abc")
    }

    "stop, but still succeed, when it has consumed the specified maximum number of characters" in {
      run(someWhile(_ < 'd').max(2), "abcdxxxx", "ab")
    }

  }

  "The oneIf parser" should {

    "succeed as long as the specified condition is met" in {
      run(oneIf(_ < 'd'), "abcde $&", "a")
    }

    "fail in case the end of the input is reached" in {
      expectFailure(oneIf(_ < 'd'), "")
    }

    "fail in case the specified predicate is not met" in {
      expectFailure(oneIf(_ < 'd'), "zxxxyyyz")
    }

  }

  "The DelimitedBy parser for character delimiters" should {

    val az = delimitedBy(CharGroup.lowerAlpha)

    "stop as soon as one of the specified delimiter characters is seen" in {
      run(az, "123abc", "123")
    }

    "succeed even when the result is empty" in {
      run(az, "abc", "")
    }

    "fail when the result is empty but nonEmpty is specified" in {
      expectFailure(az.nonEmpty, "abc")
    }

    "fail when a stop char is seen before the end delimiter" in {
      expectFailure(az.failOn('3','4'), "1234abcd")
    }

    "fail in case the end of the input is reached before seeing a delimiter character" in {
      expectFailure(az, "1234567")
    }

  }

  "The DelimitedBy parser for string literal delimiters" should {
    
    import laika.parse.implicits._

    val lit = delimitedBy(">>>")

    "stop as soon as the specified string delimiter is seen" in {
      run(lit, "123>>>", "123")
    }

    "succeed even when the result is empty" in {
      run(lit, ">>>", "")
    }

    "fail when the result is empty but nonEmpty is specified" in {
      expectFailure(lit.nonEmpty, ">>>")
    }

    "fail when a stop char is seen before the end delimiter" in {
      expectFailure(lit.failOn('3','4'), "1234>>>")
    }

    "fail in case the end of the input is reached before seeing the delimiter string" in {
      expectFailure(lit, "1234567")
    }

    "succeed when the specified post condition is met" in {
      run(delimitedBy(">>>" <~ ws.min(1)), "123>>> ", "123")
    }

    "fail when the specified post condition is not met" in {
      expectFailure(delimitedBy(">>>" <~ ws.min(1)), "123>>>A")
    }

  }

  
}
