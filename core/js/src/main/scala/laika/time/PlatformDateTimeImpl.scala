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
import scala.scalajs.js
import scala.util.Try

/**
  * @author Jens Halm
  */
object PlatformDateTimeImpl extends PlatformDateTime {

  type Type = js.Date

  private[laika] def now: Type = new js.Date()

  def parse (dateString: String): Either[String, Type] = {
    val result = new js.Date(dateString)
    if (result.getTime().isNaN) Left(s"Invalid date format: $dateString")
    else Right(result)
  }
  
  private[laika] def format (date: Type, pattern: String): Either[String, String] = {
    /*
    Formatting based on an explicit pattern is not supported for JavaScript Dates.
    The specified pattern is therefore mostly ignored, apart from looking for a colon as a hint whether
    a time component should be included.
    
    A proper way to handle this would be to parse the pattern and translate it to a JavaScript options object,
    but this is currently considered beyond the scope of Laika.
    For this reason this is currently not public API.
     */
    val attempt = {
      if (pattern.contains(":")) Try(date.toLocaleString())
      else Try(date.toLocaleDateString())
    }
    attempt.toEither.left.map(_.getMessage)
  }

  private lazy val formatterConstants = Set("full", "long", "medium", "short")

  private[laika] def formatConstant (date: Type, constant: String): Option[Either[String, String]] =
    if (formatterConstants.contains(constant.toLowerCase)) {
      val opts = js.Dynamic.literal(
        "dateStyle" -> constant.toLowerCase, 
        "timeStyle" -> constant.toLowerCase
      )
      val locale = "en" // TODO - query from metadata
      Try(date
        .asInstanceOf[js.Dynamic]
        .toLocaleString(locale, opts) // arguments not supported by core Scala.js, hence the dynamic approach
        .asInstanceOf[String]
      )
        .toEither
        .leftMap(_.getMessage)
        .some
    }
    else None

}
