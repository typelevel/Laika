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

package laika.time

import munit.FunSuite

import scala.scalajs.js

class PlatformDateTimeSpec extends FunSuite {

  private def getDate(dateString: String): String = new js.Date(dateString).toISOString()

  test("parse a date without time") {
    assertEquals(
      PlatformDateTime.parse("2011-10-10").map(_.toISOString),
      Right(getDate("2011-10-10T00:00:00Z"))
    )
  }

  test("parse a local date time") {
    assertEquals(
      PlatformDateTime.parse("2011-10-10T14:48:00").map(_.toISOString),
      Right(getDate("2011-10-10T14:48:00"))
    )
  }

  test("parse a UTC date time") {
    assertEquals(
      PlatformDateTime.parse("2011-10-10T14:48:00Z").map(_.toISOString),
      Right(getDate("2011-10-10T14:48:00Z"))
    )
  }

  test("parse a date time with an offset") {
    assertEquals(
      PlatformDateTime.parse("2011-10-10T14:48:00+0100").map(_.toISOString),
      Right(getDate("2011-10-10T13:48:00Z"))
    )
  }

  test("fail in case of invalid date format") {
    assertEquals(PlatformDateTime.parse("2011-10-10XX14:48:00+0100").isLeft, true)
  }

}
