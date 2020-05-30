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

package laika.parse.directive

import laika.ast._
import laika.directive.Templates
import laika.parse.Parser
import laika.parse.markup.DefaultRecursiveSpanParsers
import laika.parse.text.PrefixedParser
import laika.parse.builders._
import laika.parse.implicits._

/** Provides the parsers for directives and context references in templates.
  *
  * @author Jens Halm
  */
class TemplateParsers (directives: Map[String, Templates.Directive]) extends DefaultRecursiveSpanParsers {

  import DirectiveParsers._

  lazy val spanParsers: Seq[PrefixedParser[Span]] = Seq(
    hoconReference(TemplateContextReference(_,_), _.asTemplateSpan),
    templateDirective,
    "\\" ~> oneChar.map(Text(_))
  )

  lazy val templateDirective: PrefixedParser[TemplateSpan] = {

    val body: BodyParserBuilder = spec =>
      if (directives.get(spec.name).exists(_.hasBody)) recursiveSpans(delimitedBy(spec.fence)).source.map { src =>
        Some(src.dropRight(spec.fence.length))
      } | success(None)
      else success(None)

    val separators = directives.values.flatMap(_.separators).toSet
    
    PrefixedParser('@') {
      directiveParser(body, this).withSource.map { case (result, source) =>
        if (separators.contains(result.name)) Templates.SeparatorInstance(result, source)
        else Templates.DirectiveInstance(directives.get(result.name), result, templateSpans, source)
      }
    }
  }

  lazy val templateSpans: Parser[List[TemplateSpan]] = recursiveSpans.map {
    _.collect {
      case s: TemplateSpan => s
      case Text(s, opt) => TemplateString(s, opt)
    }
  }

  lazy val templateRoot: Parser[TemplateRoot] = templateSpans.map(TemplateRoot(_))

  def getSyntaxHighlighter (language: String): Option[Parser[Seq[Span]]] = None
}
