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

import java.text.SimpleDateFormat
import java.time.{Instant, LocalDateTime, ZoneId}
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.util.Date

import scala.util.Try

/**
  * @author Jens Halm
  */
object PlatformDateFormatImpl extends PlatformDateFormat {

  private val offsetDateTime: DateTimeFormatter = new DateTimeFormatterBuilder()
    .append(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
    .appendPattern("[XXX][X]")
    .toFormatter

  def parse (dateString: String): Either[String, Date] = {

    def parseOffsetDateTime(input: String): Either[String, Date] = Try {
      Date.from(Instant.from(offsetDateTime.parse(input)))
    }.toEither.left.map(_.getMessage)

    def parseLocalDateTime(input: String): Either[String, Date] = Try {
      Date.from(LocalDateTime.parse(input).atZone(ZoneId.systemDefault()).toInstant )
    }.toEither.left.map(_.getMessage)

    if (dateString.matches(".*(Z|[+-]\\d\\d[:]?\\d\\d)")) parseOffsetDateTime(dateString)
    else if (dateString.contains("T")) parseLocalDateTime(dateString)
    else parseOffsetDateTime(dateString + "T00:00:00Z")
  }

  private[laika] def format (date: Date, pattern: String): Either[String, String] =
    Try(new SimpleDateFormat(pattern).format(date)).toEither.left.map(_.getMessage)
  
}
