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

import laika.parse.core.{Failure, ParserContext}
import laika.parse.core.combinator.Parsers._
import laika.parse.helper.{ParseResultHelpers, StringParserHelpers}
import laika.util.~
import org.scalatest.{FlatSpec, Matchers}

class BaseParsersSpec extends FlatSpec with Matchers with MarkupParsers with ParseResultHelpers with StringParserHelpers {

  
  "The parser for dynamic repetitions" should "parse a sequence based on a dynamically changing parser" in {
    val p = rep("1", {res:String => (res.toInt + 1).toString})
    Parsing ("12345999") using p should produce (List("1","2","3","4","5"))
  }
  
  it should "succeed when only the first parsing step succeeds" in {
    val p = rep("1", {res:String => (res.toInt + 1).toString})
    Parsing ("1999") using p should produce (List("1"))
  }
  
  it should "succeed with an empty result when the first parsing step fails" in {
    val p = rep("1", {res:String => (res.toInt + 1).toString})
    Parsing ("999") using p should produce (List[String]())
  }
  
  
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
    val input = ParserContext("abcd").consume(2)
    (lookBehind(2,'a').parse(input)) should produce ('a')
  }
  
  it should "fail when the specified parser fails at the given negative offset" in {
    val input = ParserContext("abcd").consume(2)
    lookBehind(2,'b').parse(input) should cause [Failure]
  }
  
  it should "fail when the specified negative offset is too big" in {
    val input = ParserContext("abcd").consume(2)
    lookBehind(7,'a').parse(input) should cause [Failure]
  }
  
  
}
