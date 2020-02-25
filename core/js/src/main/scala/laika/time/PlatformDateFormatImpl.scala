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

import java.util.Date

import scala.scalajs.js
import scala.util.Try

/**
  * @author Jens Halm
  */
object PlatformDateFormatImpl extends PlatformDateFormat {

  def parse (dateString: String): Either[String, Date] = {
    val ms = js.Date.parse(dateString)
    if (ms.isNaN) Left(s"Invalid date format: $dateString")
    else Right(new Date(ms.toLong))
  }
  
  // For now the js impl ignores the pattern, this might be enhanced in the future
  private[laika] def format (date: Date, pattern: String): Either[String, String] =
    Try(new js.Date(date.getTime.toDouble).toLocaleString).toEither.left.map(_.getMessage)
  
}
