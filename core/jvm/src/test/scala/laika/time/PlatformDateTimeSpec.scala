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

import laika.config.PlatformDateTime
import munit.FunSuite

import java.time.{ LocalDateTime, OffsetDateTime, ZoneId }

class PlatformDateTimeSpec extends FunSuite {

  private val localDate     = "2011-10-10"
  private val localDateTime = localDate + "T14:48:00"

  private val expectedOffset =
    ZoneId.systemDefault().getRules
      .getValidOffsets(LocalDateTime.parse(localDateTime))
      .get(0)
      .toString

  private def getDate(dateString: String): PlatformDateTime.Type = OffsetDateTime.parse(dateString)

  test("parse a date without time") {
    assertEquals(
      PlatformDateTime.parse(localDate),
      Right(getDate(s"${localDate}T00:00:00$expectedOffset"))
    )
  }

  test("parse a local date time") {
    assertEquals(
      PlatformDateTime.parse(localDateTime),
      Right(getDate(s"$localDateTime$expectedOffset"))
    )
  }

  test("parse a UTC date time") {
    assertEquals(
      PlatformDateTime.parse(localDateTime + "Z"),
      Right(getDate(localDateTime + "Z"))
    )
  }

  test("parse a date time with an offset without colon") {
    assertEquals(
      PlatformDateTime.parse(localDateTime + "+0300"),
      Right(getDate(localDateTime + "+03:00"))
    )
  }

  test("parse a date time with an offset with colon") {
    assertEquals(
      PlatformDateTime.parse(localDateTime + "+03:00"),
      Right(getDate(localDateTime + "+03:00"))
    )
  }

  test("fail in case of invalid date format") {
    assertEquals(PlatformDateTime.parse("2011-10-10XX14:48:00+0100").isLeft, true)
  }

}
