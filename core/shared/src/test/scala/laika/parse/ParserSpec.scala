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
import laika.parse.helper.MigrationSpec
import laika.parse.text.TextParsers
import org.scalatest.Assertion

/**
  * @author Jens Halm
  */
class ParserSpec extends MigrationSpec {


  private val parser1 = TextParsers.someOf('a','b')
  private val parser2 = TextParsers.someOf('b','c')

  def run[A] (parser: Parser[A], input: String, expected: A): Assertion =
    assertEquals(parser.parse(input).toEither, Right(expected))

  def run[A] (parser: Parser[A], input: SourceCursor, expected: A): Assertion =
    assertEquals(parser.parse(input).toEither, Right(expected))

  def expectFailure[A] (parser: Parser[A], input: String): Assertion =
    assert(parser.parse(input).toEither.isLeft)

  def expectFailure[A] (parser: Parser[A], input: SourceCursor): Assertion =
    assert(parser.parse(input).toEither.isLeft)
  
  

  "A Parser" should {

    "provide the result of the parsing operation" in {
      run(parser1, "abc", "ab")
    }

    "map the result" in {
      run(parser1.map(_.length), "abccbb", 2)
    }

    "flatMap with a different parser" in {
      run(parser1.flatMap(res => parser2.max(res.length)), "abccbb", "cc")
    }

    "fail the flatMap when the first parser fails" in {
      expectFailure(parser1.min(3).flatMap(res => parser2.max(res.length)), "abccbb")
    }

    "fail the flatMap when the second parser fails" in {
      expectFailure(parser1.flatMap(_ => parser2.min(5)), "abccbb")
    }

    "provide a fixed result" in {
      run(parser1.as(7), "abccbb", 7)
    }

    "apply a partial function to the result" in {
      run(parser1.collect { case "ab" => 9 }, "abc", 9)
    }

    "fail if the specified partial function is not defined for the result" in {
      expectFailure(parser1.collect { case "xx" => 9 }, "abc")
    }

    "succeed if the specified Either function produces a Right" in {
      run(parser1.evalMap { res => Right(res.length) }, "abc", 2)
    }

    "fail if the specified Either function produces a Left" in {
      expectFailure(parser1.evalMap { res => Left("wrong") }, "abc")
    }
    
    "handle errors from a failed parser" in {
      run(parser1.handleErrorWith(_ => parser2), "ccbb", "ccbb")
    }

    "handle errors from a failed parser with information from the error" in {
      run(parser1.handleErrorWith(e => success(e.next.remaining.toString)), "ccbb", "4")
    }

    "recover from a failed parser" in {
      run(parser1.recoverWith { case _ => parser2 }, "ccbb", "ccbb")
    }

    "recover from a failed parser with information from the error" in {
      run(parser1.recoverWith { case e => success(e.next.remaining.toString) }, "ccbb", "4")
    }
  }


  "The alternatives of two parsers" should {

    val alternatives = parser1 | parser2

    "provide the result of the first parser if it succeeds" in {
      run(alternatives, "bbcc", "bb")
    }

    "provide the result of the second parser if the first fails, but the second succeeds" in {
      run(alternatives, "ccbb", "ccbb")
    }

    "fail if both parsers fail" in {
      expectFailure(alternatives, "xxxx")
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
      run((parser1 ~ parser2), "aabbcc", new ~("aabb", "cc"))
    }

    "fail if the first parser fails" in {
      expectFailure((parser1 ~ parser2), "ccbbaa")
    }

    "fail if the second parser fails" in {
      expectFailure((parser1 ~ parser2), "aaffgg")
    }

    "provide the first result" in {
      run((parser1 <~ parser2), "aabbcc", "aabb")
    }

    "provide the second result" in {
      run((parser1 ~> parser2), "aabbcc", "cc")
    }

  }

  "The optional parser" should {

    "produce a Some when the underlying parser succeeds" in {
      run(opt(parser1), "abc", Option("ab"))
    }

    "produce a None when the underlying parser fails" in {
      run(opt(parser1), "xxx", Option.empty[String])
    }

  }

  "The not parser" should {

    "fail when the underlying parser succeeds" in {
      expectFailure(Parsers.not(parser1), "abc")
    }

    "succeed when the underlying parser fails" in {
      run(Parsers.not(parser1), "xxx", ())
    }

  }

  "The success parser" should {

    "always succeed" in {
      run(success(9), "foo", 9)
    }

  }

  "The failure parser" should {

    "always fail" in {
      expectFailure(failure("expected failure"), "foo")
    }

  }

  "The repetition parser for an arbitrary number of results" should {

    "produce an empty result when the first invocation fails" in {
      run(parser1.rep, "xxx", List.empty[String])
    }

    "provide all matching substrings" in {
      run(parser1.max(1).rep, "abacc", List("a","b","a"))
    }

  }

  "The repetition parser for a minimum number of results" should {

    "fail if the required minimum number of successful invocations is not reached" in {
      expectFailure(parser1.rep.min(4), "abaxx")
    }

    "succeed if at least one invocation succeeds" in {
      run(parser1.max(1).rep.min(1), "abc", List("a","b"))
    }

    "succeed if the specified number of successful invocations is reached" in {
      run(parser1.max(1).rep.min(2), "aba", List("a","b","a"))
    }

  }

  "The repetition parser for an maximum number of results" should {

    "produce an empty result when the first invocation fails" in {
      run(parser1.rep.max(2), "xxx", List.empty[String])
    }

    "provide only the maximum number of result allowed" in {
      run(parser1.max(1).rep.max(2), "abacc", List("a","b"))
    }

  }

  "The repetition parser with a separator" should {

    "produce an empty result when the first invocation fails" in {
      run(parser1.take(2).rep("-"), "xxx", List.empty[String])
    }

    "provide all matching substrings" in {
      run(parser1.take(2).rep("-"), "ab-ba-c", List("ab","ba"))
    }

    "succeed if the specified number of successful invocations is reached" in {
      run(parser1.take(2).rep("-").min(3), "ab-ba-bb-cc", List("ab","ba","bb"))
    }

    "fail if the required minimum number of successful invocations is not reached" in {
      expectFailure(parser1.take(2).rep("-").min(3), "ab-cc-bb-cc")
    }
  }

  "The parser for dynamic repetition" should {

    import TextParsers._

    val parser = literal("1").repWith { (res:String) => literal((res.toInt + 1).toString) }

    "parse a sequence based on a dynamically changing parser" in {
      run(parser, "12345999", List("1","2","3","4","5"))
    }

    "succeed when only the first parsing step succeeds" in {
      run(parser, "1999", List("1"))
    }

    "succeed with an empty result when the first parsing step fails" in {
      run(parser, "999", List[String]())
    }

  }

  "The lookAhead parser" should {

    import TextParsers._

    "succeed when the underlying parser succeeds at the current offset" in {
      run(lookAhead("a"), "abcd", "a")
    }

    "succeed when the underlying parser succeeds at the specified offset" in {
      run(lookAhead(2, "c"), "abcd", "c")
    }

    "fail when the underlying parser fails at the current offset" in {
      expectFailure(lookAhead("c"), "abcd")
    }

    "fail when the underlying parser fails at the specified offset" in {
      expectFailure(lookAhead(2, "a"), "abcd")
    }



  }

  "The lookBehind parser" should {

    val input = SourceCursor("abcd").consume(2)

    import TextParsers._

    "succeed when the specified parser succeeds at the given negative offset" in {
      run(lookBehind(2, literal("a")), input, "a")
    }

    "fail when the specified parser fails at the given negative offset" in {
      expectFailure(lookBehind(2, literal("b")), input)
    }

    "fail when the specified negative offset is too big" in {
      expectFailure(lookBehind(7, literal("a")), input)
    }

  }

  "The consumeAll parser" should {

    "succeed when all input has been consumed" in {
      run(consumeAll(parser1), "ababa", "ababa")
    }

    "fail when not all input has been consumed" in {
      expectFailure(consumeAll(parser1), "ababaxx")
    }

  }
  
  "The source parser" should {
    
    import TextParsers._
    
    "produce the consumed string as a result" in {
      val p = anyOf('a') ~ opt(TextParsers.oneOf('d')) ~ TextParsers.oneOf('b').rep
      run(p.source, "aabbcc", "aabb")
    }
    
  }

  "The count parser" should {

    import TextParsers._

    "produce the length of the consumed string as a result" in {
      val p = anyOf('a') ~ opt(TextParsers.oneOf('d')) ~ TextParsers.oneOf('b').rep
      run(p.count, "aabbcc", 4)
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
