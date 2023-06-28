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
import laika.parse.text.TextParsers
import munit.FunSuite

/** @author Jens Halm
  */
class ParserSpec extends FunSuite {

  private val parser1 = TextParsers.someOf('a', 'b')
  private val parser2 = TextParsers.someOf('b', 'c')

  def run[A](parser: Parser[A], input: String, expected: A): Unit =
    assertEquals(parser.parse(input).toEither, Right(expected))

  def run[A](parser: Parser[A], input: SourceCursor, expected: A): Unit =
    assertEquals(parser.parse(input).toEither, Right(expected))

  def expectFailure[A](parser: Parser[A], input: String): Unit =
    assert(parser.parse(input).toEither.isLeft)

  def expectFailure[A](parser: Parser[A], input: SourceCursor): Unit =
    assert(parser.parse(input).toEither.isLeft)

  test("provide the result of the parsing operation") {
    run(parser1, "abc", "ab")
  }

  test("map the result") {
    run(parser1.map(_.length), "abccbb", 2)
  }

  test("flatMap with a different parser") {
    run(parser1.flatMap(res => parser2.max(res.length)), "abccbb", "cc")
  }

  test("fail the flatMap when the first parser fails") {
    expectFailure(parser1.min(3).flatMap(res => parser2.max(res.length)), "abccbb")
  }

  test("fail the flatMap when the second parser fails") {
    expectFailure(parser1.flatMap(_ => parser2.min(5)), "abccbb")
  }

  test("provide a fixed result") {
    run(parser1.as(7), "abccbb", 7)
  }

  test("apply a partial function to the result") {
    run(parser1.collect { case "ab" => 9 }, "abc", 9)
  }

  test("fail if the specified partial function is not defined for the result") {
    expectFailure(parser1.collect { case "xx" => 9 }, "abc")
  }

  test("succeed if the specified Either function produces a Right") {
    run(parser1.evalMap { res => Right(res.length) }, "abc", 2)
  }

  test("fail if the specified Either function produces a Left") {
    expectFailure(parser1.evalMap { _ => Left("wrong") }, "abc")
  }

  test("handle errors from a failed parser") {
    run(parser1.handleErrorWith(_ => parser2), "ccbb", "ccbb")
  }

  test("handle errors from a failed parser with information from the error") {
    run(parser1.handleErrorWith(e => success(e.next.remaining.toString)), "ccbb", "4")
  }

  test("recover from a failed parser") {
    run(parser1.recoverWith { case _ => parser2 }, "ccbb", "ccbb")
  }

  test("recover from a failed parser with information from the error") {
    run(parser1.recoverWith { case e => success(e.next.remaining.toString) }, "ccbb", "4")
  }

  object Alternatives {

    val parser: Parser[String] = parser1 | parser2

    private val p1 = TextParsers.anyOf('a', 'b').min(4).withFailureMessage("1")
    private val p2 = TextParsers.anyOf('a', 'c').min(4).withFailureMessage("2")
    private val p3 = TextParsers.anyOf('a', 'c', 'd').min(4).withFailureMessage("3")
    private val p4 = TextParsers.anyOf('a', 'e').min(4).withFailureMessage("4")
    private val p  = p1 | p2 | p3 | p4

    def validateFailure(input: String, expectedParser: Int, expectedMaxOffset: Int): Unit = {
      val res        = p.parse(input)
      assert(res.isFailure)
      val f: Failure = res.asInstanceOf[Failure]
      assertEquals(f.maxOffset, expectedMaxOffset)
      assertEquals(f.message, expectedParser.toString)
    }

  }

  test("alternatives - provide the result of the first parser if it succeeds") {
    run(Alternatives.parser, "bbcc", "bb")
  }

  test(
    "alternatives - provide the result of the second parser if the first fails, but the second succeeds"
  ) {
    run(Alternatives.parser, "ccbb", "ccbb")
  }

  test("alternatives - fail if both parsers fail") {
    expectFailure(Alternatives.parser, "xxxx")
  }

  test("alternatives - pick the first error when it has the largest max offset") {
    Alternatives.validateFailure("abaxxx", 1, 3)
  }

  test("alternatives - pick the last error when it has the largest max offset") {
    Alternatives.validateFailure("aeaxxx", 4, 3)
  }

  test(
    "alternatives - pick the error of the parser with highest precedence if multiple failures have the same max offset"
  ) {
    Alternatives.validateFailure("acaxxx", 2, 3)
  }

  test("concatenation - provide the combined result") {
    run(parser1 ~ parser2, "aabbcc", new ~("aabb", "cc"))
  }

  test("concatenation - fail if the first parser fails") {
    expectFailure(parser1 ~ parser2, "ccbbaa")
  }

  test("concatenation - fail if the second parser fails") {
    expectFailure(parser1 ~ parser2, "aaffgg")
  }

  test("concatenation - provide the first result") {
    run(parser1 <~ parser2, "aabbcc", "aabb")
  }

  test("concatenation - provide the second result") {
    run(parser1 ~> parser2, "aabbcc", "cc")
  }

  test("opt - produce a Some when the underlying parser succeeds") {
    run(opt(parser1), "abc", Option("ab"))
  }

  test("opt - produce a None when the underlying parser fails") {
    run(opt(parser1), "xxx", Option.empty[String])
  }

  test("not - fail when the underlying parser succeeds") {
    expectFailure(Parsers.not(parser1), "abc")
  }

  test("not - succeed when the underlying parser fails") {
    run(Parsers.not(parser1), "xxx", ())
  }

  test("success parser always succeeds") {
    run(success(9), "foo", 9)
  }

  test("failure parser always fails") {
    expectFailure(failure("expected failure"), "foo")
  }

  test("rep - produce an empty result when the first invocation fails") {
    run(parser1.rep, "xxx", List.empty[String])
  }

  test("rep - provide all matching substrings") {
    run(parser1.max(1).rep, "abacc", List("a", "b", "a"))
  }

  test("rep.min - fail if the required minimum number of successful invocations is not reached") {
    expectFailure(parser1.rep.min(4), "abaxx")
  }

  test("rep.min - succeed if at least one invocation succeeds") {
    run(parser1.max(1).rep.min(1), "abc", List("a", "b"))
  }

  test("rep.min - succeed if the specified number of successful invocations is reached") {
    run(parser1.max(1).rep.min(2), "aba", List("a", "b", "a"))
  }

  test("rep.max - produce an empty result when the first invocation fails") {
    run(parser1.rep.max(2), "xxx", List.empty[String])
  }

  test("rep.max - provide only the maximum number of result allowed") {
    run(parser1.max(1).rep.max(2), "abacc", List("a", "b"))
  }

  test("rep with separator - produce an empty result when the first invocation fails") {
    run(parser1.take(2).rep("-"), "xxx", List.empty[String])
  }

  test("rep with separator - provide all matching substrings") {
    run(parser1.take(2).rep("-"), "ab-ba-c", List("ab", "ba"))
  }

  test(
    "rep with separator - succeed if the specified number of successful invocations is reached"
  ) {
    run(parser1.take(2).rep("-").min(3), "ab-ba-bb-cc", List("ab", "ba", "bb"))
  }

  test(
    "rep with separator - fail if the required minimum number of successful invocations is not reached"
  ) {
    expectFailure(parser1.take(2).rep("-").min(3), "ab-cc-bb-cc")
  }

  object RepWith {

    import TextParsers._

    val parser: Parser[List[String]] = literal("1").repWith { (res: String) =>
      literal((res.toInt + 1).toString)
    }

  }

  test("repWith - parse a sequence based on a dynamically changing parser") {
    run(RepWith.parser, "12345999", List("1", "2", "3", "4", "5"))
  }

  test("repWith - succeed when only the first parsing step succeeds") {
    run(RepWith.parser, "1999", List("1"))
  }

  test("repWith - succeed with an empty result when the first parsing step fails") {
    run(RepWith.parser, "999", List[String]())
  }

  test("lookAhead - succeeds when the underlying parser succeeds at the current offset") {
    run(TextParsers.lookAhead("a"), "abcd", "a")
  }

  test("lookAhead - succeeds when the underlying parser succeeds at the specified offset") {
    run(TextParsers.lookAhead(2, "c"), "abcd", "c")
  }

  test("lookAhead - fails when the underlying parser fails at the current offset") {
    expectFailure(TextParsers.lookAhead("c"), "abcd")
  }

  test("lookAhead - fails when the underlying parser fails at the specified offset") {
    expectFailure(TextParsers.lookAhead(2, "a"), "abcd")
  }

  object LookBehind {
    val input: SourceCursor = SourceCursor("abcd").consume(2)
  }

  test("lookBehind - succeeds when the specified parser succeeds at the given negative offset") {
    run(TextParsers.lookBehind(2, TextParsers.literal("a")), LookBehind.input, "a")
  }

  test("lookBehind - fails when the specified parser fails at the given negative offset") {
    expectFailure(TextParsers.lookBehind(2, TextParsers.literal("b")), LookBehind.input)
  }

  test("lookBehind - fails when the specified negative offset is too big") {
    expectFailure(TextParsers.lookBehind(7, TextParsers.literal("a")), LookBehind.input)
  }

  test("consumeAll - succeeds when all input has been consumed") {
    run(consumeAll(parser1), "ababa", "ababa")
  }

  test("consumeAll - fails when not all input has been consumed") {
    expectFailure(consumeAll(parser1), "ababaxx")
  }

  test("source parser produces the consumed string as a result") {
    import TextParsers._
    val p = anyOf('a') ~ opt(oneOf('d')) ~ oneOf('b').rep
    run(p.source, "aabbcc", "aabb")
  }

  test("count parser produces the length of the consumed string as a result") {
    import TextParsers._
    val p = anyOf('a') ~ opt(TextParsers.oneOf('d')) ~ TextParsers.oneOf('b').rep
    run(p.count, "aabbcc", 4)
  }

  object MaxOffset {

    def validate(res: Parsed[_], expectedMaxOffset: Int): Unit = {
      assert(res.isFailure)
      assertEquals(res.asInstanceOf[Failure].maxOffset, expectedMaxOffset)
    }

  }

  test("be set properly on a failing character parser") {
    val p1 = TextParsers.anyOf('a', 'b')
    val p2 = TextParsers.anyOf('c', 'd').min(3)
    MaxOffset.validate((p1 ~ p2).parse("ababcdef"), 6)
  }

  test("be set properly on a failing literal parser") {
    val p = TextParsers.literal("Norway")
    MaxOffset.validate(p.parse("Nowhere"), 2)
  }

  test("be set properly on a failing delimited text parser") {
    val p = TextParsers.delimitedBy("]")
    MaxOffset.validate(p.parse("[wrong"), 6)
  }

}
