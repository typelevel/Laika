/*
 * Copyright 2013-2018 the original author or authors.
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

package laika.directive

import com.typesafe.config.{Config, ConfigFactory, ConfigParseOptions, ConfigValueFactory}
import laika.parse.core.Parser
import laika.parse.core.combinator.Parsers
import laika.parse.core.markup.DocumentParser.InvalidElement
import laika.parse.core.text.TextParsers
import laika.parse.core.text.TextParsers._
import laika.tree.Paths.Path

/**
  * @author Jens Halm
  */
object ConfigHeaderParser {

  type ConfigHeaderParser = Parser[Either[InvalidElement, Config]]

  def withDefaultLineDelimiters(path: Path): ConfigHeaderParser = betweenLines("{%","%}")(path)

  def betweenLines(startDelim: String, endDelim: String)(path: Path): ConfigHeaderParser = {
    val parser = startDelim ~> TextParsers.delimitedBy(endDelim) <~ wsEol
    forTextParser(parser)(path)
  }

  def beforeLine(endDelim: String)(path: Path): ConfigHeaderParser = {
    val parser = TextParsers.delimitedBy(endDelim) <~ wsEol
    forTextParser(parser)(path)
  }

  def forTextParser (parser: Parser[String])(path: Path): ConfigHeaderParser = parser ^^ { str =>
    try {
      Right(ConfigFactory.parseString(str, ConfigParseOptions.defaults().setOriginDescription(s"path:$path")))
    }
    catch {
      case ex: Exception => Left(InvalidElement("Error parsing config header: "+ex.getMessage, s"{%$str%}"))
    }
  }

  def merged (parsers: Seq[Path => ConfigHeaderParser])(path: Path): ConfigHeaderParser =
    parsers.map(_(path)).reduce(_ | _)

  val fallback: Path => Parser[Either[InvalidElement, Config]] = { _ => Parsers.success(Right(ConfigFactory.empty)) }

  def merge (config: Config, values: Map[String, AnyRef]): Config = {
    import scala.collection.JavaConverters._
    val javaValues = values.mapValues {
      case m: Map[_,_]      => m.asJava
      case it: Iterable[_]  => it.asJava
      case other            => other
    }
    (config /: javaValues) { case (config, (name, value)) =>
      config.withValue(name, ConfigValueFactory.fromAnyRef(value)) // TODO - simply use fromMap?
    }
  }


}
