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

import laika.parse.SourceCursor
import laika.parse.builders._
import laika.parse.helper.ParseResultHelpers
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
   
class TextParsersSpec extends AnyWordSpec with Matchers with ParseResultHelpers {


  "The prevNot validators" should {
    
    "succeed if the previous character is not one of those specified" in {
      (oneChar ~> prevNot('x')).parse("abc") should produce(())
    }

    "fail if the previous character is one of those specified" in {
      (oneChar ~> prevNot('a')).parse("abc").toEither.isLeft shouldBe true
    }
    
    "succeed at the start of the input" in {
      prevNot('x').parse("abc") should produce(())
    }

    "succeed if the predicate is not satisfied" in {
      (oneChar ~> prevNot(_ == 'b')).parse("abc") should produce(())
    }

    "fail if the the predicate is satisfied" in {
      (oneChar ~> prevNot(_ == 'a')).parse("abc").toEither.isLeft shouldBe true
    }

  }

  "The nextNot validators" should {

    "succeed if the next character is not one of those specified" in {
      (oneChar ~> nextNot('x')).parse("abc") should produce(())
    }

    "fail if the next character is one of those specified" in {
      (oneChar ~> nextNot('b')).parse("abc").toEither.isLeft shouldBe true
    }

    "succeed at the end of the input" in {
      (oneChar ~> nextNot('x')).parse("a") should produce(())
    }

    "succeed if the predicate is not satisfied" in {
      (oneChar ~> nextNot(_ == 'a')).parse("abc") should produce(())
    }

    "fail if the the predicate is satisfied" in {
      (oneChar ~> nextNot(_ == 'b')).parse("abc").toEither.isLeft shouldBe true
    }

  }

  "The prevIn and prevIs validators" should {

    "succeed if the previous character is one of those specified" in {
      (oneChar ~> prevIn('a','c')).parse("abc") should produce(())
    }

    "fail if the previous character is not one of those specified" in {
      (oneChar ~> prevIn('x','y')).parse("abc").toEither.isLeft shouldBe true
    }

    "fail at the start of the input" in {
      prevIn('x').parse("abc").toEither.isLeft shouldBe true
    }

    "succeed if the predicate is satisfied" in {
      (oneChar ~> prevIs(_ == 'a')).parse("abc") should produce(())
    }

    "fail if the the predicate is not satisfied" in {
      (oneChar ~> prevIs(_ == 'b')).parse("abc").toEither.isLeft shouldBe true
    }

  }

  "The nextIn and nextIs validators" should {

    "succeed if the next character is one of those specified" in {
      (oneChar ~> nextIn('b')).parse("abc") should produce(())
    }

    "fail if the next character is not one of those specified" in {
      (oneChar ~> nextIn('x')).parse("abc").toEither.isLeft shouldBe true
    }

    "fail at the end of the input" in {
      (oneChar ~> nextIn('x')).parse("a").toEither.isLeft shouldBe true
    }

    "succeed if the predicate is satisfied" in {
      (oneChar ~> nextIs(_ == 'b')).parse("abc") should produce(())
    }

    "fail if the the predicate is not satisfied" in {
      (oneChar ~> nextIs(_ == 'a')).parse("abc").toEither.isLeft shouldBe true
    }

  }

  "The literal parser" should {

    "succeed with a matching string literal" in {
      literal("abc").parse("abcd") should produce ("abc")
    }

    "fail when the string literal does not match" in {
      literal("bcd").parse("abcd").toEither.isLeft shouldBe true
    }

  }

  "The eol parser" should {

    "succeed for \\n" in {
      eol.parse("\naaa") should produce (())
    }

    "succeed for \\r\\n" in {
      eol.parse("\r\naaa") should produce (())
    }

    "succeed at the end of the input" in {
      eol.parse("") should produce (())
    }

    "fail when not at the end of a line" in {
      eol.parse("abc").toEither.isLeft shouldBe true
    }

  }

  "The eof parser" should {

    "succeed at the end of the input" in {
      eof.parse("") should produce ("")
    }

    "fail when not at the end of the input" in {
      eof.parse("\n").toEither.isLeft shouldBe true
    }

  }

  "The atStart parser" should {

    "succeed at the start of the input" in {
      atStart.parse("abc") should produce (())
    }

    "fail when not at the start of the input" in {
      atStart.parse(SourceCursor("abc").consume(1)).toEither.isLeft shouldBe true
    }

  }

  "The ws parser" should {

    "succeed with all whitespace characters" in {
      ws.parse(" \t abcd") should produce (" \t ")
    }

    "succeed with an empty string in case of non-whitespace characters" in {
      ws.parse("abcd") should produce ("")
    }

  }

  "The wsEol parser" should {

    "succeed with whitespace characters followed by newline" in {
      wsEol.parse(" \n abcd") should produce (())
    }

    "succeed with just a newline character since whitespace is optional" in {
      wsEol.parse("\n abcd") should produce (())
    }

    "fail with non-whitespace chacracters" in {
      wsEol.parse("abcd").toEither.isLeft shouldBe true
    }

  }

  "The blankLine parser" should {

    "succeed with whitespace characters followed by newline" in {
      blankLine.parse(" \n abcd") should produce ("")
    }

    "succeed with just a newline character since whitespace is optional" in {
      blankLine.parse("\n abcd") should produce ("")
    }

    "fail with non-whitespace chacracters" in {
      blankLine.parse("abcd").toEither.isLeft shouldBe true
    }

  }

  "The blankLines parser" should {

    "succeed with one blank line" in {
      blankLines.parse(" \n abcd") should produce (List(""))
    }

    "succeed with three blank lines" in {
      blankLines.parse(" \n\n \n abcd") should produce (List("","",""))
    }

    "fail with non-whitespace chacracters" in {
      blankLines.parse("abcd").toEither.isLeft shouldBe true
    }

  }

  "The restOfLine parser" should {

    "succeed with all characters of the current line" in {
      restOfLine.parse(" aa\nabcd") should produce (" aa")
    }

    "succeed with an empty result" in {
      restOfLine.parse("\nabcd") should produce ("")
    }

  }

  "The textLine parser" should {

    "succeed with all characters of the current line" in {
      textLine.parse(" aa\nabcd") should produce (" aa")
    }

    "fail on an empty line" in {
      textLine.parse("\nabcd").toEither.isLeft shouldBe true
    }

  }

  "The anyChars parser" should {

    "always succeed consuming the entire input" in {
      anyChars.parse("abcde $&") should produce ("abcde $&")
    }

    "only consume the specified maximum number of characters" in {
      anyChars.max(3).parse("abcde $&") should produce ("abc")
    }
  }

  "The anyOf parser" should {

    "succeed with an empty result when no characters match" in {
      anyOf('x').parse("ababccab") should produce ("")
    }

    "succeed for the matching character when 1 character is specified" in {
      anyOf('x').parse("xxabc") should produce ("xx")
    }

    "succeed for all matching characters when 3 characters are specified" in {
      anyOf('x','y','z').parse("xxyzxabc") should produce ("xxyzx")
    }

    "succeed in case the end of the input is reached" in {
      anyOf('x','y','z').parse("xxyzx") should produce ("xxyzx")
    }

    "fail when it does not consume the specified minimum number of characters" in {
      anyOf('x').min(3).parse("xxabc").toEither.isLeft shouldBe true
    }

    "succeed when it does consume the specified minimum number of characters" in {
      anyOf('x').min(3).parse("xxxxabc") should produce ("xxxx")
    }

    "stop, but still succeed, when it has consumed the specified maximum number of characters" in {
      anyOf('x').max(3).parse("xxxxxx") should produce ("xxx")
    }

  }

  "The someOf parser" should {

    "succeed for the matching character when 1 character is specified" in {
      someOf('x').parse("xxabc") should produce ("xx")
    }

    "succeed for all matching characters when 3 characters are specified" in {
      someOf('x','y','z').parse("xxyzxabc") should produce ("xxyzx")
    }

    "succeed in case the end of the input is reached" in {
      someOf('x','y','z').parse("xxyzx") should produce ("xxyzx")
    }

    "fail when it does not consume the specified minimum number of characters" in {
      someOf('x').min(3).parse("xxabc").toEither.isLeft shouldBe true
    }

    "fail when it does not consume any characters as min(1) is implicit in someOf parsers" in {
      someOf('x').parse("abcde").toEither.isLeft shouldBe true
    }

    "succeed when it does consume the specified minimum number of characters" in {
      someOf('x').min(3).parse("xxxxabc") should produce ("xxxx")
    }

    "stop, but still succeed, when it has consumed the specified maximum number of characters" in {
      someOf('x').max(3).parse("xxxxxx") should produce ("xxx")
    }

  }

  "The oneOf parser" should {

    "succeed for the matching character when 1 character is specified" in {
      TextParsers.oneOf('x').parse("xxabc") should produce ("x")
    }

    "succeed for any of the specified 3 characters" in {
      TextParsers.oneOf('x','y','z').parse("yzxabc") should produce ("y")
    }

    "fail in case the end of the input is reached" in {
      TextParsers.oneOf('x','y','z').parse("").toEither.isLeft shouldBe true
    }

    "fail when the first character does not match" in {
      TextParsers.oneOf('x').parse("yxxabc").toEither.isLeft shouldBe true
    }

  }
  
  "The anyNot parser" should {

    "succeed for all non-matching characters when 1 character is specified" in {
      anyNot('x').parse("abcxxabc") should produce ("abc")
    }

    "succeed for all non-matching characters when 3 characters are specified" in {
      anyNot('x','y','z').parse("abczyxabc") should produce ("abc")
    }

    "succeed in case the end of the input is reached" in {
      anyNot('x','y','z').parse("abcabc") should produce ("abcabc")
    }

    "fail when it does not consume the specified minimum number of characters" in {
      anyNot('x').min(3).parse("abxx").toEither.isLeft shouldBe true
    }
    
    "succeed when it does consume the specified minimum number of characters" in {
      anyNot('x').min(3).parse("abcdxxxx") should produce ("abcd")
    }

    "stop, but still succeed, when it has consumed the specified maximum number of characters" in {
      anyNot('x').max(3).parse("abcdxxxx") should produce ("abc")
    }

  }

  "The someNot parser" should {

    "succeed for all non-matching characters when 1 character is specified" in {
      someNot('x').parse("abcxxabc") should produce ("abc")
    }

    "succeed for all non-matching characters when 3 characters are specified" in {
      someNot('x','y','z').parse("abczyxabc") should produce ("abc")
    }

    "succeed in case the end of the input is reached" in {
      someNot('x','y','z').parse("abcabc") should produce ("abcabc")
    }

    "fail when it does not consume the specified minimum number of characters" in {
      someNot('x').min(3).parse("abxx").toEither.isLeft shouldBe true
    }

    "fail when it does not consume any characters as min(1) is implicit in someNot parsers" in {
      someNot('a','b').parse("abcde").toEither.isLeft shouldBe true
    }

    "succeed when it does consume the specified minimum number of characters" in {
      someNot('x').min(3).parse("abcdxxxx") should produce ("abcd")
    }

    "stop, but still succeed, when it has consumed the specified maximum number of characters" in {
      someNot('x').max(3).parse("abcdxxxx") should produce ("abc")
    }

  }

  "The oneNot parser" should {

    "succeed for a non-matching character when 1 character is specified" in {
      TextParsers.oneNot('a').parse("xxabc") should produce ("x")
    }

    "succeed for any of the specified 3 characters" in {
      TextParsers.oneNot('a','b','c').parse("yzxabc") should produce ("y")
    }

    "fail in case the end of the input is reached" in {
      TextParsers.oneNot('x','y','z').parse("").toEither.isLeft shouldBe true
    }

    "fail when the first character does not match" in {
      TextParsers.oneNot('x','y').parse("yxxabc").toEither.isLeft shouldBe true
    }

  }
  
  "The anyOf parser with the range builder" should {

    "succeed for any character within the specified range when 1 range is specified" in {
      anyOf(range('a', 'd')).parse("abcde $&") should produce ("abcd")
    }

    "succeed for any character within the specified ranges when 2 ranges are specified" in {
      anyOf(range('a', 'd') ++ range('x', 'z')).parse("abcdxyzff") should produce ("abcdxyz")
    }

    "succeed in case the end of the input is reached" in {
      anyOf(range('a', 'd')).parse("abcabd") should produce ("abcabd")
    }

  }
  
  "The anyWhile parser" should {

    "succeed with an empty result when no characters match" in {
      anyWhile(_ < 'd').parse("xyzzyw") should produce ("")
    }
    
    "succeed as long as the specified condition is met" in {
      anyWhile(_ < 'd').parse("abcde $&") should produce ("abc")
    }

    "succeed in case the end of the input is reached" in {
      anyWhile(_ < 'd').parse("abcabc") should produce ("abcabc")
    }

    "fail when it does not consume the specified minimum number of characters" in {
      anyWhile(_ < 'd').min(3).parse("abxx").toEither.isLeft shouldBe true
    }

    "succeed when it does consume the specified minimum number of characters" in {
      anyWhile(_ < 'd').min(3).parse("abcdxxxx") should produce ("abc")
    }

    "stop, but still succeed, when it has consumed the specified maximum number of characters" in {
      anyWhile(_ < 'd').max(2).parse("abcdxxxx") should produce ("ab")
    }

  }

  "The someWhile parser" should {

    "succeed as long as the specified condition is met" in {
      someWhile(_ < 'd').parse("abcde $&") should produce ("abc")
    }

    "succeed in case the end of the input is reached" in {
      someWhile(_ < 'd').parse("abcabc") should produce ("abcabc")
    }

    "fail when it does not consume the specified minimum number of characters" in {
      someWhile(_ < 'd').min(3).parse("abxx").toEither.isLeft shouldBe true
    }

    "fail when it does not consume any characters as min(1) is implicit in someNot parsers" in {
      someWhile(_ < 'd').parse("xxyyzz").toEither.isLeft shouldBe true
    }

    "succeed when it does consume the specified minimum number of characters" in {
      someWhile(_ < 'd').min(3).parse("abcdxxxx") should produce ("abc")
    }

    "stop, but still succeed, when it has consumed the specified maximum number of characters" in {
      someWhile(_ < 'd').max(2).parse("abcdxxxx") should produce ("ab")
    }

  }

  "The oneIf parser" should {

    "succeed as long as the specified condition is met" in {
      oneIf(_ < 'd').parse("abcde $&") should produce ("a")
    }

    "fail in case the end of the input is reached" in {
      oneIf(_ < 'd').parse("").toEither.isLeft shouldBe true
    }

    "fail in case the specified predicate is not met" in {
      oneIf(_ < 'd').parse("zxxxyyyz").toEither.isLeft shouldBe true
    }

  }

  "The DelimitedBy parser for character delimiters" should {

    val az = delimitedBy(CharGroup.lowerAlpha)

    "stop as soon as one of the specified delimiter characters is seen" in {
      az.parse("123abc") should produce ("123")
    }

    "succeed even when the result is empty" in {
      az.parse("abc") should produce ("")
    }

    "fail when the result is empty but nonEmpty is specified" in {
      az.nonEmpty.parse("abc").toEither.isLeft shouldBe true
    }

    "fail when a stop char is seen before the end delimiter" in {
      az.failOn('3','4').parse("1234abcd").toEither.isLeft shouldBe true
    }

    "fail in case the end of the input is reached before seeing a delimiter character" in {
      az.parse("1234567").toEither.isLeft shouldBe true
    }

  }

  "The DelimitedBy parser for string literal delimiters" should {
    
    import laika.parse.implicits._

    val lit = delimitedBy(">>>")

    "stop as soon as the specified string delimiter is seen" in {
      lit.parse("123>>>") should produce ("123")
    }

    "succeed even when the result is empty" in {
      lit.parse(">>>") should produce ("")
    }

    "fail when the result is empty but nonEmpty is specified" in {
      lit.nonEmpty.parse(">>>").toEither.isLeft shouldBe true
    }

    "fail when a stop char is seen before the end delimiter" in {
      lit.failOn('3','4').parse("1234>>>").toEither.isLeft shouldBe true
    }

    "fail in case the end of the input is reached before seeing the delimiter string" in {
      lit.parse("1234567").toEither.isLeft shouldBe true
    }

    "succeed when the specified post condition is met" in {
      delimitedBy(">>>" <~ ws.min(1)).parse("123>>> ") should produce ("123")
    }

    "fail when the specified post condition is not met" in {
      delimitedBy(">>>" <~ ws.min(1)).parse("123>>>A").toEither.isLeft shouldBe true
    }

  }

  
}
