/*
 * Copyright 2013-2016 the original author or authors.
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

package laika.template

import laika.parse.core.{Parser, ParserContext}
import laika.parse.core.text.TextParsers._
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigParseOptions
import laika.directive.DirectiveParsers
import laika.directive.Directives.Templates
import laika.parse.core.markup.{DefaultEscapedTextParsers, DefaultRecursiveSpanParsers, EscapedTextParsers, RecursiveSpanParsers}
import laika.parse.core.text.{DelimitedBy, MarkupParser}
import laika.rewrite.DocumentCursor
import laika.tree.Paths.Path
import laika.tree.Documents.TemplateDocument
import laika.tree.Elements._
import laika.tree.Templates._
import laika.util.~


object ConfigParser {

  val configBlock = "{%" ~> DelimitedBy("%}") <~ wsEol

  def forPath[T] (path: Path, errorHandler: (Exception, String) => T): Parser[Either[T, Config]] = configBlock ^^ { str =>
    try {
      Right(ConfigFactory.parseString(str, ConfigParseOptions.defaults().setOriginDescription(s"path:$path")))
    }
    catch {
      case ex: Exception => Left(errorHandler(ex, str))
    }
  }

}


/** Provides the parsers for directives in templates.
  */
class TemplateParsers (directives: Map[String, Templates.Directive]) extends DefaultRecursiveSpanParsers
                                                                     with DefaultEscapedTextParsers {

  val directiveParsers = new DirectiveParsers(this)

  import directiveParsers._

  case class DirectiveSpan(f: DocumentCursor => Span, options: Options = NoOpt) extends SpanResolver with TemplateSpan {
    def resolve(cursor: DocumentCursor) = f(cursor) match {
      case s: TemplateSpan => s
      case s: Span => TemplateElement(s)
    }
  }

  lazy val spanParsers: Map[Char, Parser[Span]] = Map(
    '{' -> reference(TemplateContextReference(_)),
    '@' -> templateDirective,
    '\\'-> ((any take 1) ^^ { Text(_) })
  )

  lazy val templateDirective: Parser[TemplateSpan] = {
    val contextRefOrNestedBraces = Map('{' -> (reference(TemplateContextReference(_)) | nestedBraces))
    val bodyContent = wsOrNl ~ '{' ~> (withSource(delimitedRecursiveSpans(DelimitedBy('}'), contextRefOrNestedBraces)) ^^ (_._2.dropRight(1)))
    withSource(directiveParser(bodyContent, includeStartChar = false)) ^^ { case (result, source) =>

      def createContext(parts: PartMap, docCursor: Option[DocumentCursor]): Templates.DirectiveContext = {
        new DirectiveContextBase(parts, docCursor) with Templates.DirectiveContext {
          val parser = new Templates.Parser {
            def apply(source: String) = nestedTemplateSpans.parseMarkup(source)
          }
        }
      }
      def invalid(msg: String) = TemplateElement(InvalidSpan(SystemMessage(laika.tree.Elements.Error, msg), Literal("@" + source)))

      applyDirective(Templates)(result, directives.get, createContext, s => DirectiveSpan(s), invalid, "template")
    }
  }

  def configParser (path: Path): Parser[Either[InvalidSpan,Config]] =
    ConfigParser.forPath(path, {
      (ex: Exception, str: String) => InvalidSpan(SystemMessage(laika.tree.Elements.Error,
        "Error parsing config header: "+ex.getMessage), TemplateString(s"{%$str%}"))
    })

  lazy val templateSpans: Parser[List[TemplateSpan]] = recursiveSpans ^^ {
    _.collect {
      case s: TemplateSpan => s
      case Text(s, opt) => TemplateString(s, opt)
    }
  }

  lazy val nestedTemplateSpans = new MarkupParser(templateSpans)

  def templateWithConfig (path: Path): Parser[(Config, List[TemplateSpan])] = opt(configParser(path)) ~ templateSpans ^^ {
    case Some(Right(config)) ~ root => (config, root)
    case Some(Left(span)) ~ root    => (ConfigFactory.empty(), TemplateElement(span) :: root)
    case None ~ root                => (ConfigFactory.empty(), root)
  }

  def parseTemplate (ctx: ParserContext, path: Path): TemplateDocument = {
    val (config, root) = new MarkupParser(templateWithConfig(path)).parseMarkup(ctx)
    TemplateDocument(path, TemplateRoot(root), config)
  }


}
