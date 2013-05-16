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
import org.scalatest.matchers.ShouldMatchers

import laika.parse.helper.ParseResultHelpers
import laika.parse.helper.StringParserHelpers

class BaseParsersSpec extends FlatSpec with ShouldMatchers with MarkupParsers with ParseResultHelpers with StringParserHelpers {

  
  "The repMax parser" should "not parse more than the specified maximum number of items" in {
    val p = repMax(2, anyOf('a','b').min(1) ~ anyOf('c','d').min(1) ^^ {case s1~s2 => s1+s2})
    Parsing ("abcdabcdabcdabcd") using p should produce (List("abcd","abcd"))
  }
  
  it should "succeed when the number of repetitions is lower than the specified maximum number of items" in {
    val p = repMax(2, anyOf('a','b').min(1) ~ anyOf('c','d').min(1) ^^ {case s1~s2 => s1+s2})
    Parsing ("abcdab") using p should produce (List("abcd"))
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