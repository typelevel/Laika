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

package laika.directive

import laika.ast._
import laika.directive.Directives.Templates
import laika.io.Input
import laika.parse.core.markup.DefaultRecursiveSpanParsers
import laika.parse.core.text.TextParsers._
import laika.parse.core.{Parser, ParserContext}
import laika.rewrite.DocumentCursor


/** Provides the parsers for directives and context references in templates.
  *
  * @author Jens Halm
  */
class TemplateParsers (directives: Map[String, Templates.Directive]) extends DefaultRecursiveSpanParsers {

  import DirectiveParsers._

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
    val nestedTemplateSpans: ParserContext => List[TemplateSpan] = unsafeParserFunction(templateSpans)

    val contextRefOrNestedBraces = Map('{' -> (reference(TemplateContextReference(_)) | nestedBraces))
    val bodyContent = wsOrNl ~ '{' ~> (withSource(delimitedRecursiveSpans(delimitedBy('}'), contextRefOrNestedBraces)) ^^ (_._2.dropRight(1)))
    withSource(directiveParser(bodyContent, this)) ^^ { case (result, source) =>

      def createContext(parts: PartMap, docCursor: Option[DocumentCursor]): Templates.DirectiveContext = {
        new DirectiveContextBase(parts, docCursor) with Templates.DirectiveContext {
          val parser = new Templates.Parser {
            def apply(source: String) = nestedTemplateSpans(ParserContext(source))
          }
        }
      }
      def invalid (msg: String) = InvalidElement(msg, "@" + source).asTemplateSpan

      applyDirective(Templates)(result, directives.get, createContext, s => DirectiveSpan(s), invalid, "template")
    }
  }

  lazy val templateSpans: Parser[List[TemplateSpan]] = recursiveSpans ^^ {
    _.collect {
      case s: TemplateSpan => s
      case Text(s, opt) => TemplateString(s, opt)
    }
  }

  lazy val templateRoot: Parser[TemplateRoot] = templateSpans ^^ (TemplateRoot(_))

}

/** Default template parser without any template directives,
  * usually used for parsing fallback templates from resource folders.
  */
object DefaultTemplateParser extends TemplateParsers(Map.empty) {
  def parse (input: Input): TemplateDocument = {
    val root = unsafeParserFunction(templateRoot)(input.asParserInput)
    TemplateDocument(input.path, root)
  }
}
