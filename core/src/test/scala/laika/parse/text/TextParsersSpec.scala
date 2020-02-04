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

import laika.parse.helper.{ParseResultHelpers, StringParserHelpers}
import laika.parse.text.TextParsers._
import laika.parse.{Failure, ParserContext}
import org.scalatest.{Matchers, WordSpec}
   
class TextParsersSpec extends WordSpec with Matchers with ParseResultHelpers with StringParserHelpers {


  "The char parser" should {

    "succeed with a matching character" in {
      Parsing ("aaa") using 'a' should produce ('a')
    }

    "fail when the character does not match" in {
      Parsing ("aaa") using 'b' should cause [Failure]
    }

  }

  "The literal parser" should {

    "succeed with a matching string literal" in {
      Parsing ("abcd") using "abc" should produce ("abc")
    }

    "fail when the string literal does not match" in {
      Parsing ("abcd") using "bcd" should cause [Failure]
    }

  }

  "The eol parser" should {

    "succeed for \\n" in {
      Parsing ("\naaa") using eol should produce (())
    }

    "succeed for \\r\\n" in {
      Parsing ("\r\naaa") using eol should produce (())
    }

    "succeed at the end of the input" in {
      Parsing ("") using eol should produce (())
    }

    "fail when not at the end of a line" in {
      Parsing ("abc") using eol should cause [Failure]
    }

  }

  "The eof parser" should {

    "succeed at the end of the input" in {
      Parsing ("") using eof should produce ("")
    }

    "fail when not at the end of the input" in {
      Parsing ("\n") using eof should cause [Failure]
    }

  }

  "The atStart parser" should {

    "succeed at the start of the input" in {
      atStart.parse("abc") should produce (())
    }

    "fail when not at the start of the input" in {
      atStart.parse(ParserContext("abc").consume(1)) should cause [Failure]
    }

  }

  "The ws parser" should {

    "succeed with all whitespace characters" in {
      Parsing (" \t abcd") using ws should produce (" \t ")
    }

    "succeed with an empty string in case of non-whitespace characters" in {
      Parsing ("abcd") using ws should produce ("")
    }

  }

  "The wsEol parser" should {

    "succeed with whitespace characters followed by newline" in {
      Parsing (" \n abcd") using wsEol should produce (())
    }

    "succeed with just a newline character since whitespace is optional" in {
      Parsing ("\n abcd") using wsEol should produce (())
    }

    "fail with non-whitespace chacracters" in {
      Parsing ("abcd") using wsEol should cause [Failure]
    }

  }

  "The blankLine parser" should {

    "succeed with whitespace characters followed by newline" in {
      Parsing (" \n abcd") using blankLine should produce ("")
    }

    "succeed with just a newline character since whitespace is optional" in {
      Parsing ("\n abcd") using blankLine should produce ("")
    }

    "fail with non-whitespace chacracters" in {
      Parsing ("abcd") using blankLine should cause [Failure]
    }

  }

  "The blankLines parser" should {

    "succeed with one blank line" in {
      Parsing (" \n abcd") using blankLines should produce (List(""))
    }

    "succeed with three blank lines" in {
      Parsing (" \n\n \n abcd") using blankLines should produce (List("","",""))
    }

    "fail with non-whitespace chacracters" in {
      Parsing ("abcd") using blankLines should cause [Failure]
    }

  }

  "The restOfLine parser" should {

    "succeed with all characters of the current line" in {
      Parsing (" aa\nabcd") using restOfLine should produce (" aa")
    }

    "succeed with an empty result" in {
      Parsing ("\nabcd") using restOfLine should produce ("")
    }

  }

  "The textLine parser" should {

    "succeed with all characters of the current line" in {
      Parsing (" aa\nabcd") using textLine should produce (" aa")
    }

    "fail on an empty line" in {
      Parsing ("\nabcd") using textLine should cause [Failure]
    }

  }

  "The reference name parser" should {

    "parse a name consisting of characters" in {
      Parsing ("name") using refName should produce ("name")
    }

    "parse a name consisting of characters and digits" in {
      Parsing ("7name9") using refName should produce ("7name9")
    }

    "parse a name consisting of characters and any of the supported symbols" in {
      Parsing ("a-a_a.a:a+a") using refName should produce ("a-a_a.a:a+a")
    }

    "fail if the name starts with a symbol" in {
      Parsing ("-a_a.a:a+a") using refName should cause [Failure]
    }

    "ignore a trailing symbol" in {
      Parsing ("a-a_a.a:a+") using refName should produce ("a-a_a.a:a")
    }

    "stop parsing at two consecutive symbols" in {
      Parsing ("a-a.+a") using refName should produce ("a-a")
    }

    "stop parsing at unsupported symbols" in {
      Parsing ("a-a(a") using refName should produce ("a-a")
    }

    "parse a name containing non-ASCII characters" in {
      Parsing ("näme") using refName should produce ("näme")
    }

  }

  "The any parser" should {

    "always succeed consuming the entire input" in {
      Parsing ("abcde $&") using anyChars should produce ("abcde $&")
    }

    "only consume the specified maximum number of characters" in {
      Parsing ("abcde $&") using anyChars.max(3) should produce ("abc")
    }
  }

  "The anyOf parser" should {

    "succeed with an empty result when no characters match" in {
      Parsing ("ababccab") using anyOf('x') should produce ("")
    }

    "succeed for the matching character when 1 character is specified" in {
      Parsing ("xxabc") using anyOf('x') should produce ("xx")
    }

    "succeed for all matching characters when 3 characters are specified" in {
      Parsing ("xxyzxabc") using anyOf('x','y','z') should produce ("xxyzx")
    }

    "succeed in case the end of the input is reached" in {
      Parsing ("xxyzx") using anyOf('x','y','z') should produce ("xxyzx")
    }

    "fail when it does not consume the specified minimum number of characters" in {
      Parsing ("xxabc") using (anyOf('x') min 3) should cause [Failure]
    }

    "succeed when it does consume the specified minimum number of characters" in {
      Parsing ("xxxxabc") using (anyOf('x') min 3) should produce ("xxxx")
    }

    "stop, but still succeed, when it has consumed the specified maximum number of characters" in {
      Parsing ("xxxxxx") using (anyOf('x') max 3) should produce ("xxx")
    }

  }

  "The someOf parser" should {

    "succeed for the matching character when 1 character is specified" in {
      Parsing ("xxabc") using someOf('x') should produce ("xx")
    }

    "succeed for all matching characters when 3 characters are specified" in {
      Parsing ("xxyzxabc") using someOf('x','y','z') should produce ("xxyzx")
    }

    "succeed in case the end of the input is reached" in {
      Parsing ("xxyzx") using someOf('x','y','z') should produce ("xxyzx")
    }

    "fail when it does not consume the specified minimum number of characters" in {
      Parsing ("xxabc") using someOf('x').min(3) should cause [Failure]
    }

    "fail when it does not consume any characters as min(1) is implicit in someOf parsers" in {
      Parsing ("abcde") using someOf('x') should cause [Failure]
    }

    "succeed when it does consume the specified minimum number of characters" in {
      Parsing ("xxxxabc") using someOf('x').min(3) should produce ("xxxx")
    }

    "stop, but still succeed, when it has consumed the specified maximum number of characters" in {
      Parsing ("xxxxxx") using someOf('x').max(3) should produce ("xxx")
    }

  }

  "The oneOf parser" should {

    "succeed for the matching character when 1 character is specified" in {
      Parsing ("xxabc") using TextParsers.oneOf('x') should produce ("x")
    }

    "succeed for any of the specified 3 characters" in {
      Parsing ("yzxabc") using TextParsers.oneOf('x','y','z') should produce ("y")
    }

    "fail in case the end of the input is reached" in {
      Parsing ("") using TextParsers.oneOf('x','y','z') should cause [Failure]
    }

    "fail when the first character does not match" in {
      Parsing ("yxxabc") using TextParsers.oneOf('x') should cause [Failure]
    }

  }
  
  "The anyBut parser" should {

    "succeed for all non-matching characters when 1 character is specified" in {
      Parsing ("abcxxabc") using anyBut('x') should produce ("abc")
    }

    "succeed for all non-matching characters when 3 characters are specified" in {
      Parsing ("abczyxabc") using anyBut('x','y','z') should produce ("abc")
    }

    "succeed in case the end of the input is reached" in {
      Parsing ("abcabc") using anyBut('x','y','z') should produce ("abcabc")
    }

    "fail when it does not consume the specified minimum number of characters" in {
      Parsing ("abxx") using (anyBut('x') min 3) should cause [Failure]
    }

    "succeed when it does consume the specified minimum number of characters" in {
      Parsing ("abcdxxxx") using (anyBut('x') min 3) should produce ("abcd")
    }

    "stop, but still succeed, when it has consumed the specified maximum number of characters" in {
      Parsing ("abcdxxxx") using (anyBut('x') max 3) should produce ("abc")
    }

  }
  
  "The anyIn parser" should {

    "succeed for any character within the specified range when 1 range is specified" in {
      Parsing ("abcde $&") using anyIn('a' to 'd') should produce ("abcd")
    }

    "succeed for any character within the specified ranges when 2 ranges are specified" in {
      Parsing ("abcdxyzff") using anyIn('a' to 'd', 'x' to 'z') should produce ("abcdxyz")
    }

    "succeed for any matching character when 2 ranges and a single character are specified" in {
      Parsing ("abcd__xyzff") using anyIn('a' to 'd', 'x' to 'z', '_') should produce ("abcd__xyz")
    }

    "succeed in case the end of the input is reached" in {
      Parsing ("abcabd") using anyIn('a' to 'd') should produce ("abcabd")
    }

  }
  
  "The anyWhile parser" should {

    "succeed as long as the specified condition is met" in {
      Parsing ("abcde $&") using anyWhile(_ < 'd') should produce ("abc")
    }

    "succeed in case the end of the input is reached" in {
      Parsing ("abcabc") using anyWhile(_ < 'd') should produce ("abcabc")
    }

    "fail when it does not consume the specified minimum number of characters" in {
      Parsing ("abxx") using anyWhile(_ < 'd').min(3) should cause [Failure]
    }

    "succeed when it does consume the specified minimum number of characters" in {
      Parsing ("abcdxxxx") using anyWhile(_ < 'd').min(3) should produce ("abc")
    }

    "stop, but still succeed, when it has consumed the specified maximum number of characters" in {
      Parsing ("abcdxxxx") using anyWhile(_ < 'd').max(2) should produce ("ab")
    }

  }

  "The DelimitedBy parser for character delimiters" should {

    val az = delimitedBy(CharGroup.lowerAlpha)

    "stop as soon as one of the specified delimiter characters is seen" in {
      Parsing ("123abc") using az should produce ("123")
    }

    "succeed even when the result is empty" in {
      Parsing ("abc") using az should produce ("")
    }

    "fail when the result is empty but nonEmpty is specified" in {
      Parsing ("abc") using az.nonEmpty should cause [Failure]
    }

    "fail when a stop char is seen before the end delimiter" in {
      Parsing ("1234abcd") using az.failOn('3','4') should cause [Failure]
    }

    "fail in case the end of the input is reached before seeing a delimiter character" in {
      Parsing ("1234567") using az should cause [Failure]
    }

  }

  "The DelimitedBy parser for string literal delimiters" should {

    val lit = delimitedBy(">>>")

    "stop as soon as the specified string delimiter is seen" in {
      Parsing ("123>>>") using lit should produce ("123")
    }

    "succeed even when the result is empty" in {
      Parsing (">>>") using lit should produce ("")
    }

    "fail when the result is empty but nonEmpty is specified" in {
      Parsing (">>>") using lit.nonEmpty should cause [Failure]
    }

    "fail when a stop char is seen before the end delimiter" in {
      Parsing ("1234>>>") using lit.failOn('3','4') should cause [Failure]
    }

    "fail in case the end of the input is reached before seeing the delimiter string" in {
      Parsing ("1234567") using lit should cause [Failure]
    }

    "succeed when the specified post condition is met" in {
      Parsing ("123>>> ") using delimitedBy(">>>", ws.min(1)) should produce ("123")
    }

    "fail when the specified post condition is not met" in {
      Parsing ("123>>>A") using delimitedBy(">>>", ws.min(1)) should cause [Failure]
    }

  }

  
}
