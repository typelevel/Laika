/*
 * Copyright 2013 the original author or authors.
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

package laika.parse

import scala.util.parsing.input.CharSequenceReader

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

import laika.parse.helper.ParseResultHelpers
import laika.parse.helper.StringParserHelpers
   
class MarkupParsersSpec extends FlatSpec with ShouldMatchers with MarkupParsers with ParseResultHelpers with StringParserHelpers {
 
  
  "The eol parser" should "succeed for \\n" in {
    Parsing ("\naaa") using eol should produce ("")
  }
  
  it should "succeed for \\r\\n" in {
    Parsing ("\r\naaa") using eol should produce ("")
  }
  
  it should "succeed at the end of the input" in {
    Parsing ("") using eol should produce ("")
  }
  
  it should "fail when not at the end of a line" in {
    Parsing ("abc") using eol should cause [Failure]
  }
  
  "The any parser" should "always succeed consuming the entire input" in {
    Parsing ("abcde $&") using any should produce ("abcde $&")
  }
  
  "The anyOf parser" should "succeed with an empty result when no characters are specified" in {
    Parsing ("ababccab") using anyOf() should produce ("")
  }
  
  it should "succeed for the matching character when 1 character is specified" in {
    Parsing ("xxabc") using anyOf('x') should produce ("xx")
  }
  
  it should "succeed for all matching characters when 2 characters are specified" in {
    Parsing ("xxyxabc") using anyOf('x','y') should produce ("xxyx")
  }
  
  it should "succeed for all matching characters when 3 characters are specified" in {
    Parsing ("xxyzxabc") using anyOf('x','y','z') should produce ("xxyzx")
  }
  
  it should "succeed in case the end of the input is reached" in {
    Parsing ("xxyzx") using anyOf('x','y','z') should produce ("xxyzx")
  }
  
  "The anyBut parser" should "succeed consuming the entire input when no characters are specified" in {
    Parsing ("abcde $&") using anyBut() should produce ("abcde $&")
  }
  
  it should "succeed for all non-matching characters when 1 character is specified" in {
    Parsing ("abcxxabc") using anyBut('x') should produce ("abc")
  }
  
  it should "succeed for all non-matching characters when 2 characters are specified" in {
    Parsing ("abcyxabc") using anyBut('x','y') should produce ("abc")
  }
  
  it should "succeed for all non-matching characters when 3 characters are specified" in {
    Parsing ("abczyxabc") using anyBut('x','y','z') should produce ("abc")
  }
  
  it should "succeed in case the end of the input is reached" in {
    Parsing ("abcabc") using anyBut('x','y','z') should produce ("abcabc")
  }
  
  "The anyUntil parser" should "behave the same as the anyBut parser in case one of the specified characters is seen" in {
    Parsing ("abcxxabc") using anyUntil('x') should produce ("abc")
  }
  
  it should "fail if non of the specified characters appear in the input" in {
    Parsing ("abcabc") using anyUntil('x') should cause [Failure]
  }
  
  "The anyIn parser" should "succeed for any character within the specified range when 1 range is specified" in {
    Parsing ("abcde $&") using anyIn('a' to 'd') should produce ("abcd")
  }
  
  it should "succeed for any character within the specified ranges when 2 ranges are specified" in {
    Parsing ("abcdxyzff") using anyIn('a' to 'd', 'x' to 'z') should produce ("abcdxyz")
  }
  
  it should "succeed for any matching character when 2 ranges and a single character are specified" in {
    Parsing ("abcd__xyzff") using anyIn('a' to 'd', 'x' to 'z', '_') should produce ("abcd__xyz")
  }
  
  it should "succeed in case the end of the input is reached" in {
    Parsing ("abcabd") using anyIn('a' to 'd') should produce ("abcabd")
  }
  
  "The anyWhile parser" should "succeed as long as the specified condition is met" in {
    Parsing ("abcde $&") using anyWhile(_ < 'd') should produce ("abc")
  }
  
  "Any TextParser" should "fail when it does not consume the specified minimum number of characters" in {
    Parsing ("xxabc") using (anyOf('x') min 3) should cause [Failure]
  }
  
  it should "succeed when it does consume the specified minimum number of characters" in {
    Parsing ("xxxxabc") using (anyOf('x') min 3) should produce ("xxxx")
  }
  
  it should "stop, but still succeed, when it has consumed the specified maximum number of characters" in {
    Parsing ("xxxxxx") using (anyOf('x') max 3) should produce ("xxx")
  }
  
  it should "stop when a stop char is seen" in {
    Parsing ("abcdxxx") using (anyBut('x') stopChars('c','d')) should produce ("ab")
  }
  
  "The anyUntil parser" should "stop as soon as the specified parser succeeds" in {
    Parsing ("123abc") using anyUntil("[a-z]".r) should produce ("123")
  }
  
  it should "fail when it does not consume the specified minimum number of characters" in {
    Parsing ("12abc") using (anyUntil("[a-z]".r) min 3) should cause [Failure]
  }
  
  it should "succeed when it does consume the specified minimum number of characters" in {
    Parsing ("1234abc") using (anyUntil("[a-z]".r) min 3) should produce ("1234")
  }
  
  it should "stop, but still succeed, when it has consumed the specified maximum number of characters" in {
    Parsing ("1234xx") using (anyUntil("[a-z]".r) max 3) should produce ("123")
  }
  
  it should "stop when a stop char is seen" in {
    Parsing ("1234abcd") using (anyUntil("[a-z]".r) stopChars('3','4')) should produce ("12")
  }
  
  it should "fail in case the end of the input is reached" in {
    Parsing ("1234567") using anyUntil("[a-z]".r) should cause [Failure]
  }
  
  "The repMin parser" should "fail when the specified parser does not succeed the specified minimum number of times" in {
    val p = repMin(3, anyOf('a','b').min(1) ~ anyOf('c','d').min(1) ^^ {case s1~s2 => s1+s2})
    Parsing ("abcdabcdab") using p should cause [Failure]
  }
  
  it should "succeed when the specified parser does succeed the specified minimum number of times" in {
    val p = repMin(2, anyOf('a','b').min(1) ~ anyOf('c','d').min(1) ^^ {case s1~s2 => s1+s2})
    Parsing ("abcdabcdab") using p should produce (List("abcd","abcd"))
  }
  
  "The lookBehind parser" should "succeed when the specified parser succeeds at the given negative offset" in {
    val input = new CharSequenceReader("abcd").drop(2)
    (lookBehind(2,'a')(input)) should produce ('a')
  }
  
  it should "fail when the specified parser fails at the given negative offset" in {
    val input = new CharSequenceReader("abcd").drop(2)
    lookBehind(2,'b')(input) should cause [Failure]
  }
  
  it should "fail when the specified negative offset is too big" in {
    val input = new CharSequenceReader("abcd").drop(2)
    lookBehind(7,'a')(input) should cause [Failure]
  }
  
  
  
}