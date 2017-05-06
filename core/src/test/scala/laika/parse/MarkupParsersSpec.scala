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

package laika.parse

import laika.parse.core.Failure
import laika.parse.core.text.{DelimitedBy}
import laika.parse.core.text.TextParsers._
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import laika.parse.helper.ParseResultHelpers
import laika.parse.helper.StringParserHelpers
   
class MarkupParsersSpec extends FlatSpec with Matchers with ParseResultHelpers with StringParserHelpers {
 
  
  "The eol parser" should "succeed for \\n" in {
    Parsing ("\naaa") using eol should produce (())
  }
  
  it should "succeed for \\r\\n" in {
    Parsing ("\r\naaa") using eol should produce (())
  }
  
  it should "succeed at the end of the input" in {
    Parsing ("") using eol should produce (())
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
  
  "The DelimitedBy parser" should "behave the same as the anyBut parser in case one of the specified characters is seen" in {
    Parsing ("abcxxabc") using DelimitedBy('x') should produce ("abc")
  }
  
  it should "fail if non of the specified characters appear in the input" in {
    Parsing ("abcabc") using DelimitedBy('x') should cause [Failure]
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
  
  val az = DelimitedBy(('a' to 'z'): _*)
  
  "The DelimitedBy parser" should "stop as soon as the specified end delimiter is seen" in {
    Parsing ("123abc") using az should produce ("123")
  }

  it should "succeed even when the result is empty" in {
    Parsing ("abc") using az should produce ("")
  }
  
  it should "fail when the result is empty but nonEmpty is specified" in {
    Parsing ("abc") using az.nonEmpty should cause [Failure]
  }
  
  it should "fail when a stop char is seen before the end delimiter" in {
    Parsing ("1234abcd") using az.failOn('3','4') should cause [Failure]
  }
  
  it should "fail in case the end of the input is reached" in {
    Parsing ("1234567") using az should cause [Failure]
  }
  
  
  "The reference name parser" should "parse a name consisting of characters" in {
    Parsing ("name") using (refName) should produce ("name")
  }
  
  it should "parse a name consisting of characters and digits" in {
    Parsing ("7name9") using (refName) should produce ("7name9")
  }
  
  it should "parse a name consisting of characters and any of the supported symbols" in {
    Parsing ("a-a_a.a:a+a") using (refName) should produce ("a-a_a.a:a+a")
  }
  
  it should "fail if the name starts with a symbol" in {
    Parsing ("-a_a.a:a+a") using (refName) should cause [Failure]
  }
  
  it should "ignore a trailing symbol" in {
    Parsing ("a-a_a.a:a+") using (refName) should produce ("a-a_a.a:a")
  }
  
  it should "stop parsing at two consecutive symbols" in {
    Parsing ("a-a.+a") using (refName) should produce ("a-a")
  }
  
  it should "stop parsing at unsupported symbols" in {
    Parsing ("a-a(a") using (refName) should produce ("a-a")
  }
  
  it should "parse a name containing non-ASCII characters" in {
    Parsing ("näme") using (refName) should produce ("näme")
  }
  
  
}
