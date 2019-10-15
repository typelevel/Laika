/*
 * Copyright 2012-2019 the original author or authors.
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

package laika.parse.directive

import laika.api.config.{Config, ConfigBuilder}
import laika.ast.{InvalidElement, Path}
import laika.bundle.ConfigProvider
import laika.parse.Parser
import laika.parse.combinator.Parsers
import laika.parse.hocon.HoconParsers.ConfigValue
import laika.parse.text.TextParsers._

/** Provides parser implementation for configuration header sections
  * in text markup files, which are expected to be in HOCON format.
  *
  * @author Jens Halm
  */
object ConfigHeaderParser {

  type ConfigHeaderParser = Parser[Either[InvalidElement, Config]]

  /** Parser for default configuration headers which are enclosed
    * between lines containing `{%` and `%}` respectively.
    */
  def withDefaultLineDelimiters(path: Path): ConfigHeaderParser = betweenLines("{%","%}")(path)

  /** Parser for configuration headers which are enclosed
    * between the specified start and end delimiters.
    * These delimiters are expected to be both on a separate line.
    */
  def betweenLines(startDelim: String, endDelim: String)(path: Path): ConfigHeaderParser = {
    val parser = startDelim ~> delimitedBy(endDelim) <~ wsEol
    forTextParser(parser)(path)
  }

  /** Generic base parser for configuration headers based on the specified string parser.
    *
    * The parser is expected to detect and consume any start and end delimiters without
    * adding them to the result which is supposed to be a string in HOCON format.
    *
    * The contract for such a parser is that it fails if it cannot successfully read
    * the expected start or end delimiters, so that other parsers (if defined) can be
    * tried instead.
    */
  def forTextParser (parser: Parser[String])(path: Path): ConfigHeaderParser = parser ^^ { str =>
    try {
      Right(ConfigProvider.fromInput(str, path))
    }
    catch {
      case ex: Exception => Left(InvalidElement("Error parsing config header: "+ex.getMessage, s"{%$str%}"))
    }
  }

  /** Merges the specified parsers so that they will be tried consecutively until
    * one of them succeeds. If all of them fail, the merged parser will fail, too.
    */
  def merged (parsers: Seq[Path => ConfigHeaderParser])(path: Path): ConfigHeaderParser =
    parsers.map(_(path)).reduce(_ | _)

  val fallback: Path => Parser[Either[InvalidElement, Config]] = { _ => Parsers.success(Right(Config.empty)) }

  def merge (config: Config, values: Seq[(String, ConfigValue)]): Config =
    values.foldLeft(ConfigBuilder.empty.withFallback(config)) { case (builder, (key, value)) =>
      builder.withValue(key, value)
    }.build


}
