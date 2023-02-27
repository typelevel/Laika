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

package laika.rst

import laika.rst.BaseParsers.simpleRefName
import munit.FunSuite

/** @author Jens Halm
  */
class BaseParsersSpec extends FunSuite {

  test("parse a name consisting of characters") {
    assertEquals(simpleRefName.parse("name").toEither, Right("name"))
  }

  test("parse a name consisting of characters and digits") {
    assertEquals(simpleRefName.parse("7name9").toEither, Right("7name9"))
  }

  test("parse a name consisting of characters and any of the supported symbols") {
    assertEquals(simpleRefName.parse("a-a_a.a:a+a").toEither, Right("a-a_a.a:a+a"))
  }

  test("fail if the name starts with a symbol") {
    assertEquals(simpleRefName.parse("-a_a.a:a+a").toEither.isLeft, true)
  }

  test("ignore a trailing symbol") {
    assertEquals(simpleRefName.parse("a-a_a.a:a+").toEither, Right("a-a_a.a:a"))
  }

  test("stop parsing at two consecutive symbols") {
    assertEquals(simpleRefName.parse("a-a.+a").toEither, Right("a-a"))
  }

  test("stop parsing at unsupported symbols") {
    assertEquals(simpleRefName.parse("a-a(a").toEither, Right("a-a"))
  }

  test("parse a name containing non-ASCII characters") {
    assertEquals(simpleRefName.parse("näme").toEither, Right("näme"))
  }

}
