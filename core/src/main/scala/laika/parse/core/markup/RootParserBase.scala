/*
 * Copyright 2013-2017 the original author or authors.
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

package laika.parse.core.markup

import com.typesafe.config.{Config, ConfigFactory, ConfigValueFactory}
import laika.parse.core._
import laika.parse.core.text.TextParsers._
import laika.rewrite.TreeUtil
import laika.tree.Documents.Document
import laika.tree.Elements._
import laika.tree.Paths.Path
import laika.util.~

/**
  * @author Jens Halm
  */
trait RootParserBase extends DefaultRecursiveParsers {


  /** Parses a full document, delegating most of the work to the `topLevelBlock` parser.
    */
  lazy val rootElement: Parser[RootElement] = opt(blankLines) ~> blockList(topLevelBlock) ^^ RootElement

  /** Fully parses the input from the specified reader and returns the document tree.
    *  This function is expected to always succeed, errors would be considered a bug
    *  of this library, as the parsers treat all unknown or malformed markup as regular
    *  text.
    */
  def parseDocument (reader: ParserContext, path: Path): Document = {
    val (config, root) = parseConfigAndRoot(reader, path)
    Document(path, root, TreeUtil.extractFragments(root.content), config)
  }

  def config (path: Path): Parser[Either[InvalidBlock,Config]] = failure("configuration sections not enabled")

  /** Fully parses the input from the specified reader and returns the configuration and root element.
    */
  protected def parseConfigAndRoot (ctx: ParserContext, path: Path): (Config,RootElement) = {
    // TODO - extract into ConfigHeaderParser
    def assembleConfig (config: Config, root: RootElement) = {
      import scala.collection.JavaConverters._
      val values = root.content collect { case ConfigValue(name, value, _) => (name, value match {
        case m: Map[_,_]      => m.asJava
        case it: Iterable[_]  => it.asJava
        case other            => other
      })}
      ((config /: values) { case (config, (name, value)) =>
        config.withValue(name, ConfigValueFactory.fromAnyRef(value))
      }, root)
    }
    val parser = opt(config(path)) ~ rootElement ^^ {
      case Some(Right(config)) ~ root => assembleConfig(config, root)
      case Some(Left(block)) ~ root   => assembleConfig(ConfigFactory.empty(), root.copy(content = block +: root.content))
      case None ~ root                => assembleConfig(ConfigFactory.empty(), root)
    }
    unsafeParserFunction(parser)(ctx)
  }


  protected def mergeSpanParsers (base: Map[Char, Parser[Span]], additional: Map[Char, Parser[Span]]) = {
    additional.foldLeft(base) {
      case (acc, (char, parser)) =>
        val oldParser = base.get(char)
        acc + (char -> oldParser.map(parser | _).getOrElse(parser))
    }
  }

  protected def mergeBlockParsers (parsers: Seq[Parser[Block]]): Parser[Block] =
    if (parsers.isEmpty) failure("No block parsers specified")
    else parsers.reduceLeft(_ | _)


}
