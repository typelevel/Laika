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

package laika.parse.directive

import laika.ast._
import laika.directive.Templates
import laika.parse.Parser
import laika.parse.markup.DefaultRecursiveSpanParsers
import laika.parse.text.TextParsers._

/** Provides the parsers for directives and context references in templates.
  *
  * @author Jens Halm
  */
class TemplateParsers (directives: Map[String, Templates.Directive]) extends DefaultRecursiveSpanParsers {

  import DirectiveParsers._

  lazy val spanParsers: Map[Char, Parser[Span]] = Map(
    '{' -> reference(TemplateContextReference(_)),
    '@' -> templateDirective,
    '\\'-> ((any take 1) ^^ { Text(_) })
  )

  lazy val templateDirective: Parser[TemplateSpan] = {

    val separators = directives.values.flatMap(_.separators).toSet
    val contextRefOrNestedBraces = Map('{' -> (reference(TemplateContextReference(_)) | nestedBraces))
    val legacyBody = wsOrNl ~ '{' ~> (withSource(delimitedRecursiveSpans(delimitedBy('}'), contextRefOrNestedBraces)) ^^ (_._2.dropRight(1)))
    val newBody: BodyParserBuilder = spec =>
      if (directives.get(spec.name).exists(_.hasBody)) withSource(delimitedRecursiveSpans(delimitedBy(spec.fence), contextRefOrNestedBraces)) ^^ { src =>
        Some(src._2.dropRight(spec.fence.length))
      } | success(None)
      else success(None)

    withSource(directiveParser(newBody, legacyBody, this)) ^^ { case (result, source) =>
      if (separators.contains(result.name)) Templates.SeparatorInstance(result)
      else Templates.DirectiveInstance(directives.get(result.name), result, templateSpans, source)
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
