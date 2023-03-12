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

import cats.data.NonEmptySet
import laika.parse.Parser
import laika.parse.builders._
import munit.FunSuite

/** @author Jens Halm
  */
class DelimiterParserSpec extends FunSuite {

  val skip: Parser[Unit] = oneChar.void

  val abc: NonEmptySet[Char] = NonEmptySet.of('a', 'b', 'c')

  test("parse a character delimiter") {
    assertEquals(delimiter('*').parse("*").toEither, Right("*"))
  }

  test("parse a string literal delimiter") {
    assertEquals(delimiter("**").parse("**").toEither, Right("**"))
  }

  test("fail if a string literal delimiter is not matched") {
    assert(delimiter("**").parse("*").toEither.isLeft)
  }

  test("parse a character group delimiter") {
    assertEquals(delimiter(someOf(abc).take(2)).parse("cb").toEither, Right("cb"))
  }

  test("fail if a character group delimiter is not matched") {
    assert(delimiter(someOf(abc).take(2)).parse("cd").toEither.isLeft)
  }

  test("parse a delimiter with a character post-condition") {
    assertEquals(delimiter("**").nextNot('.').parse("**:").toEither, Right("**"))
  }

  test("fail when a character post-condition is not satisfied") {
    assert(delimiter("**").nextNot('.').parse("**.").toEither.isLeft)
  }

  test("parse a delimiter with a character set post-condition") {
    assertEquals(delimiter("**").nextNot(abc).parse("**:").toEither, Right("**"))
  }

  test("fail when a character set post-condition is not satisfied") {
    assert(delimiter("**").nextNot(abc).parse("**a").toEither.isLeft)
  }

  test("parse a delimiter with a post-condition function") {
    assertEquals(delimiter("**").nextNot(_ > 'a').parse("**:").toEither, Right("**"))
  }

  test("fail when a post-condition function is not satisfied") {
    assert(delimiter("**").nextNot(_ > 'a').parse("**z").toEither.isLeft)
  }

  test("parse a delimiter with a character pre-condition") {
    assertEquals((skip ~> delimiter("**").prevNot('.')).parse(":**").toEither, Right("**"))
  }

  test("fail when a character pre-condition is not satisfied") {
    assert((skip ~> delimiter("**").prevNot('.')).parse(".**").toEither.isLeft)
  }

  test("parse a delimiter with a character set pre-condition") {
    assertEquals((skip ~> delimiter("**").prevNot(abc)).parse(":**").toEither, Right("**"))
  }

  test("fail when a character set pre-condition is not satisfied") {
    assert((skip ~> delimiter("**").prevNot(abc)).parse("a**").toEither.isLeft)
  }

  test("parse a delimiter with a pre-condition function") {
    assertEquals((skip ~> delimiter("**").prevNot(_ > 'a')).parse(":**").toEither, Right("**"))
  }

  test("fail when a pre-condition function is not satisfied") {
    assert((skip ~> delimiter("**").prevNot(_ > 'a')).parse("z**").toEither.isLeft)
  }

  test("compose two delimiters") {
    val inner = delimiter("*").nextNot('a')
    val outer = delimiter(inner).nextNot('b')

    assert(outer.parse("*a").toEither.isLeft)
    assert(outer.parse("*b").toEither.isLeft)
    assertEquals(outer.parse("*c").toEither, Right("*"))
  }

}
