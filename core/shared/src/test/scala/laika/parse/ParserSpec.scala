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

package laika.parse

import laika.ast.~
import laika.parse.combinator.Parsers
import laika.parse.combinator.Parsers._
import laika.parse.helper.ParseResultHelpers
import laika.parse.text.TextParsers
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * @author Jens Halm
  */
class ParserSpec extends AnyWordSpec with Matchers with ParseResultHelpers {


  private val parser1 = TextParsers.someOf('a','b')
  private val parser2 = TextParsers.someOf('b','c')


  "A Parser" should {

    "provide the result of the parsing operation" in {
      parser1.parse("abc") should produce ("ab")
    }

    "map the result" in {
      parser1.map(_.length).parse("abccbb") should produce (2)
    }

    "flatMap with a different parser" in {
      parser1.flatMap(res => parser2.max(res.length)).parse("abccbb") should produce ("cc")
    }

    "fail the flatMap when the first parser fails" in {
      parser1.min(3).flatMap(res => parser2.max(res.length)).parse("abccbb").toEither.isLeft shouldBe true
    }

    "fail the flatMap when the second parser fails" in {
      parser1.flatMap(_ => parser2.min(5)).parse("abccbb").toEither.isLeft shouldBe true
    }

    "provide a fixed result" in {
      parser1.as(7).parse("abccbb") should produce (7)
    }

    "apply a partial function to the result" in {
      parser1.collect { case "ab" => 9 }.parse("abc") should produce (9)
    }

    "fail if the specified partial function is not defined for the result" in {
      parser1.collect { case "xx" => 9 }.parse("abc").toEither.isLeft shouldBe true
    }

    "succeed if the specified Either function produces a Right" in {
      parser1.evalMap { res => Right(res.length) }.parse("abc") should produce (2)
    }

    "fail if the specified Either function produces a Left" in {
      parser1.evalMap { res => Left("wrong") }.parse("abc").toEither.isLeft shouldBe true
    }
    
    "handle errors from a failed parser" in {
      parser1.handleErrorWith(_ => parser2).parse("ccbb") should produce("ccbb")
    }

    "handle errors from a failed parser with information from the error" in {
      parser1.handleErrorWith(e => success(e.next.remaining.toString)).parse("ccbb") should produce("4")
    }

    "recover from a failed parser" in {
      parser1.recoverWith { case _ => parser2 }.parse("ccbb") should produce("ccbb")
    }

    "recover from a failed parser with information from the error" in {
      parser1.recoverWith { case e => success(e.next.remaining.toString) }.parse("ccbb") should produce("4")
    }
  }


  "The alternatives of two parsers" should {

    val alternatives = parser1 | parser2

    "provide the result of the first parser if it succeeds" in {
      alternatives.parse("bbcc") should produce ("bb")
    }

    "provide the result of the second parser if the first fails, but the second succeeds" in {
      alternatives.parse("ccbb") should produce ("ccbb")
    }

    "fail if both parsers fail" in {
      alternatives.parse("xxxx").toEither.isLeft shouldBe true
    }

  }

  "The failure of alternatives of four parsers" should {

    val p1 = TextParsers.anyOf('a','b').min(4).withFailureMessage("1")
    val p2 = TextParsers.anyOf('a','c').min(4).withFailureMessage("2")
    val p3 = TextParsers.anyOf('a','c','d').min(4).withFailureMessage("3")
    val p4 = TextParsers.anyOf('a','e').min(4).withFailureMessage("4")
    val p = p1 | p2 | p3 | p4

    def validate(input: String, expectedParser: Int, expectedMaxOffset: Int): Assertion = {
      val res = p.parse(input)
      res shouldBe a[Failure]
      val f: Failure = res.asInstanceOf[Failure]
      f.maxOffset shouldBe expectedMaxOffset
      f.message shouldBe expectedParser.toString
    }
    
    "should pick the first error when it has the largest max offset" in {
      validate("abaxxx", 1, 3)
    }

    "should pick the last error when it has the largest max offset" in {
      validate("aeaxxx", 4, 3)
    }

    "should pick the error of the parser with highest precedence if multiple failures have the same max offset" in {
      validate("acaxxx", 2, 3)
    }
  }

  "The concatenation of two parsers" should {

    "provide the combined result" in {
      (parser1 ~ parser2).parse("aabbcc") should produce (new ~("aabb", "cc"))
    }

    "fail if the first parser fails" in {
      (parser1 ~ parser2).parse("ccbbaa").toEither.isLeft shouldBe true
    }

    "fail if the second parser fails" in {
      (parser1 ~ parser2).parse("aaffgg").toEither.isLeft shouldBe true
    }

    "provide the first result" in {
      (parser1 <~ parser2).parse("aabbcc") should produce ("aabb")
    }

    "provide the second result" in {
      (parser1 ~> parser2).parse("aabbcc") should produce ("cc")
    }

  }

  "The optional parser" should {

    "produce a Some when the underlying parser succeeds" in {
      opt(parser1).parse("abc") should produce (Option("ab"))
    }

    "produce a None when the underlying parser fails" in {
      opt(parser1).parse("xxx") should produce (Option.empty[String])
    }

  }

  "The not parser" should {

    "fail when the underlying parser succeeds" in {
      Parsers.not(parser1).parse("abc").toEither.isLeft shouldBe true
    }

    "succeed when the underlying parser fails" in {
      Parsers.not(parser1).parse("xxx") should produce (())
    }

  }

  "The success parser" should {

    "always succeed" in {
      success(9).parse("foo") should produce (9)
    }

  }

  "The failure parser" should {

    "always fail" in {
      failure("expected failure").parse("foo").toEither.isLeft shouldBe true
    }

  }

  "The repetition parser for an arbitrary number of results" should {

    "produce an empty result when the first invocation fails" in {
      parser1.rep.parse("xxx") should produce (List.empty[String])
    }

    "provide all matching substrings" in {
      parser1.max(1).rep.parse("abacc") should produce (List("a","b","a"))
    }

  }

  "The repetition parser for a minimum number of results" should {

    "fail if the required minimum number of successful invocations is not reached" in {
      parser1.rep.min(4).parse("abaxx").toEither.isLeft shouldBe true
    }

    "succeed if at least one invocation succeeds" in {
      parser1.max(1).rep.min(1).parse("abc") should produce (List("a","b"))
    }

    "succeed if the specified number of successful invocations is reached" in {
      parser1.max(1).rep.min(2).parse("aba") should produce (List("a","b","a"))
    }

  }

  "The repetition parser for an maximum number of results" should {

    "produce an empty result when the first invocation fails" in {
      parser1.rep.max(2).parse("xxx") should produce (List.empty[String])
    }

    "provide only the maximum number of result allowed" in {
      parser1.max(1).rep.max(2).parse("abacc") should produce (List("a","b"))
    }

  }

  "The repetition parser with a separator" should {

    "produce an empty result when the first invocation fails" in {
      parser1.take(2).rep("-").parse("xxx") should produce (List.empty[String])
    }

    "provide all matching substrings" in {
      parser1.take(2).rep("-").parse("ab-ba-c") should produce (List("ab","ba"))
    }

    "succeed if the specified number of successful invocations is reached" in {
      parser1.take(2).rep("-").min(3).parse("ab-ba-bb-cc") should produce (List("ab","ba","bb"))
    }

    "fail if the required minimum number of successful invocations is not reached" in {
      parser1.take(2).rep("-").min(3).parse("ab-cc-bb-cc").toEither.isLeft shouldBe true
    }
  }

  "The parser for dynamic repetition" should {

    import TextParsers._

    val parser = literal("1").repWith { (res:String) => literal((res.toInt + 1).toString) }

    "parse a sequence based on a dynamically changing parser" in {
      parser.parse("12345999") should produce (List("1","2","3","4","5"))
    }

    "succeed when only the first parsing step succeeds" in {
      parser.parse("1999") should produce (List("1"))
    }

    "succeed with an empty result when the first parsing step fails" in {
      parser.parse("999") should produce (List[String]())
    }

  }

  "The lookAhead parser" should {

    import TextParsers._

    "succeed when the underlying parser succeeds at the current offset" in {
      lookAhead("a").parse("abcd") should produce("a")
    }

    "succeed when the underlying parser succeeds at the specified offset" in {
      lookAhead(2, "c").parse("abcd") should produce("c")
    }

    "fail when the underlying parser fails at the current offset" in {
      lookAhead("c").parse("abcd").toEither.isLeft shouldBe true
    }

    "fail when the underlying parser fails at the specified offset" in {
      lookAhead(2, "a").parse("abcd").toEither.isLeft shouldBe true
    }



  }

  "The lookBehind parser" should {

    val input = SourceCursor("abcd").consume(2)

    import TextParsers._

    "succeed when the specified parser succeeds at the given negative offset" in {
      lookBehind(2, literal("a")).parse(input) should produce("a")
    }

    "fail when the specified parser fails at the given negative offset" in {
      lookBehind(2, literal("b")).parse(input).toEither.isLeft shouldBe true
    }

    "fail when the specified negative offset is too big" in {
      lookBehind(7, literal("a")).parse(input).toEither.isLeft shouldBe true
    }

  }

  "The consumeAll parser" should {

    "succeed when all input has been consumed" in {
      consumeAll(parser1).parse("ababa") should produce("ababa")
    }

    "fail when not all input has been consumed" in {
      consumeAll(parser1).parse("ababaxx").toEither.isLeft shouldBe true
    }

  }
  
  "The source parser" should {
    
    import TextParsers._
    
    "produce the consumed string as a result" in {
      val p = anyOf('a') ~ opt(TextParsers.oneOf('d')) ~ TextParsers.oneOf('b').rep
      p.source.parse("aabbcc") should produce ("aabb")
    }
    
  }

  "The count parser" should {

    import TextParsers._

    "produce the length of the consumed string as a result" in {
      val p = anyOf('a') ~ opt(TextParsers.oneOf('d')) ~ TextParsers.oneOf('b').rep
      p.count.parse("aabbcc") should produce (4)
    }

  }
  
  "The maxOffset property" should {
    
    def validate(res: Parsed[_], expectedMaxOffset: Int): Assertion = {
      res shouldBe a[Failure]
      res.asInstanceOf[Failure].maxOffset shouldBe expectedMaxOffset
    }
    
    "be set properly on a failing character parser" in {
      val p1 = TextParsers.anyOf('a','b')
      val p2 = TextParsers.anyOf('c','d').min(3)
      validate((p1 ~ p2).parse("ababcdef"), 6)
    }

    "be set properly on a failing literal parser" in {
      val p = TextParsers.literal("Norway")
      validate(p.parse("Nowhere"), 2)
    }

    "be set properly on a failing delimited text parser" in {
      val p = TextParsers.delimitedBy("]")
      validate(p.parse("[wrong"), 6)
    }
    
  }


}
