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

import cats.syntax.all._
import java.time.format.{ DateTimeFormatter, DateTimeFormatterBuilder, FormatStyle }
import java.time.{ LocalDateTime, OffsetDateTime, ZoneId }
import java.util.Locale
import scala.util.Try

/** @author Jens Halm
  */
object PlatformDateTimeImpl extends PlatformDateTime {

  type Type = OffsetDateTime

  private[laika] def now: Type = OffsetDateTime.now()

  private val offsetDateTime: DateTimeFormatter = new DateTimeFormatterBuilder()
    .append(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
    .appendPattern("[XXX][X]")
    .toFormatter

  def parse(dateString: String): Either[String, Type] = {

    def parseOffsetDateTime(input: String): Either[String, Type] = Try {
      OffsetDateTime.from(offsetDateTime.parse(input))
    }.toEither.left.map(_.getMessage)

    def parseLocalDateTime(input: String): Either[String, Type] = Try {
      OffsetDateTime.from(LocalDateTime.parse(input).atZone(ZoneId.systemDefault()))
    }.toEither.left.map(_.getMessage)

    if (dateString.matches(".*(Z|[+-]\\d\\d[:]?\\d\\d)")) parseOffsetDateTime(dateString)
    else if (dateString.contains("T")) parseLocalDateTime(dateString)
    else parseLocalDateTime(dateString + "T00:00:00")
  }

  private[laika] def format(
      date: Type,
      pattern: String,
      locale: Option[String] = None
  ): Either[String, String] =
    getLocale(locale).flatMap { loc =>
      Try(
        new DateTimeFormatterBuilder()
          .appendPattern(pattern)
          .toFormatter
          .withLocale(loc)
          .format(date)
      ).toEither.left.map(_.getMessage)
    }

  private lazy val formatterConstants = Map(
    "BASIC_ISO_DATE"       -> DateTimeFormatter.BASIC_ISO_DATE,
    "ISO_LOCAL_DATE"       -> DateTimeFormatter.ISO_LOCAL_DATE,
    "ISO_OFFSET_DATE"      -> DateTimeFormatter.ISO_OFFSET_DATE,
    "ISO_DATE"             -> DateTimeFormatter.ISO_DATE,
    "ISO_LOCAL_TIME"       -> DateTimeFormatter.ISO_LOCAL_TIME,
    "ISO_OFFSET_TIME"      -> DateTimeFormatter.ISO_OFFSET_TIME,
    "ISO_TIME"             -> DateTimeFormatter.ISO_TIME,
    "ISO_LOCAL_DATE_TIME"  -> DateTimeFormatter.ISO_LOCAL_DATE_TIME,
    "ISO_OFFSET_DATE_TIME" -> DateTimeFormatter.ISO_OFFSET_DATE_TIME,
    "ISO_ZONED_DATE_TIME"  -> DateTimeFormatter.ISO_ZONED_DATE_TIME,
    "ISO_DATE_TIME"        -> DateTimeFormatter.ISO_DATE_TIME,
    "ISO_ORDINAL_DATE"     -> DateTimeFormatter.ISO_ORDINAL_DATE,
    "ISO_WEEK_DATE"        -> DateTimeFormatter.ISO_WEEK_DATE,
    "ISO_INSTANT"          -> DateTimeFormatter.ISO_INSTANT,
    "RFC_1123_DATE_TIME"   -> DateTimeFormatter.RFC_1123_DATE_TIME,
    "MEDIUM"               -> DateTimeFormatter.ofLocalizedDateTime(FormatStyle.MEDIUM),
    "SHORT"                -> DateTimeFormatter.ofLocalizedDateTime(FormatStyle.SHORT)
  )

  private def getLocale(languageTag: Option[String]): Either[String, Locale] =
    languageTag.fold[Either[String, Locale]](Right(Locale.getDefault)) { lang =>
      Try(new Locale.Builder().setLanguageTag(lang).build()).toEither.leftMap(_.getMessage)
    }

  private[laika] def formatConstant(
      date: Type,
      constant: String,
      locale: Option[String] = None
  ): Option[Either[String, String]] =
    formatterConstants.get(constant.trim.toUpperCase).map { formatter =>
      getLocale(locale).flatMap { loc =>
        Try(formatter.withLocale(loc).format(date)).toEither.leftMap(_.getMessage)
      }
    }

}
