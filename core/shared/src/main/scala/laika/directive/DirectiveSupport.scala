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

package laika.directive

import laika.ast.{DocumentCursor, InvalidElement, LinkDefinitionReference, Replace, RewriteAction, RewriteRules, Span}
import laika.bundle.{BundleOrigin, ConfigProvider, ExtensionBundle, ParserBundle}
import laika.config.ConfigParser
import laika.parse.Parser
import laika.parse.builders.{delimitedBy, text, ws}
import laika.parse.implicits._
import laika.parse.directive.{BlockDirectiveParsers, ConfigHeaderParser, DirectiveParsers, SpanDirectiveParsers, TemplateParsers}
import laika.parse.text.TextParsers

/** Internal API that processes all directives defined
  * by one or more DirectiveRegistries. This extension
  * is installed by default, unless the transformation
  * is run in strict mode.
  *
  * @author Jens Halm
  */
class DirectiveSupport (blockDirectives: Seq[Blocks.Directive],
                        spanDirectives: Seq[Spans.Directive],
                        templateDirectives: Seq[Templates.Directive],
                        linkDirectives: Seq[Links.Directive]) extends ExtensionBundle {

  val description: String = "Laika's directive support"

  override val origin: BundleOrigin = BundleOrigin.Library
  
  private val configProvider: ConfigProvider = new ConfigProvider {
    def configHeader: Parser[ConfigParser] = ConfigHeaderParser.withDefaultLineDelimiters
    def configDocument (input: String): ConfigParser = ConfigParser.parse(input)
  }
  
  override lazy val parsers: ParserBundle = ParserBundle(
    blockParsers = Seq(BlockDirectiveParsers.blockDirective(Blocks.toMap(blockDirectives))),
    spanParsers = Seq(
      SpanDirectiveParsers.spanDirective(Spans.toMap(spanDirectives ++ linkDirectives.map(_.asSpanDirective))), 
      SpanDirectiveParsers.contextRef, 
      SpanDirectiveParsers.legacyContextRef
    ),
    configProvider = Some(configProvider),
    templateParser = Some(new TemplateParsers(Templates.toMap(templateDirectives)).templateRoot)
  )
  
  private val linkDirectiveMap = linkDirectives.map(d => (d.name, d)).toMap
  private val linkParser = (DirectiveParsers.nameDecl <~ ws) ~ ("(" ~> text(delimitedBy(')')).embed("\\" ~> TextParsers.oneChar))
  
  override lazy val rewriteRules: Seq[DocumentCursor => RewriteRules] = Seq(
    cursor => RewriteRules.forSpans {
      case LinkDefinitionReference(content, id, src, opt) if id.startsWith("@:") => 
        println(linkDirectiveMap)
        linkParser.parse(id.drop(2)).toEither.fold(
          err => Replace(InvalidElement(s"Invalid link directive: $err", src).asSpan),
          res => linkDirectiveMap.get(res._1)
            .fold[RewriteAction[Span]](Replace(InvalidElement(s"Unknown link directive: ${res._1}", src).asSpan)) { dir =>
              dir(res._2, cursor).fold(
                err => Replace(InvalidElement(s"Invalid link directive: $err", src).asSpan),
                res => Replace(res.copy(content = content, options = res.options + opt))
              )
            }
        )
    }
  )

  /** Hook for extension registries for adding block, span and template directives.
    */
  def withDirectives (newBlockDirectives: Seq[Blocks.Directive],
                      newSpanDirectives: Seq[Spans.Directive],
                      newTemplateDirectives: Seq[Templates.Directive],
                      newLinkDirectives: Seq[Links.Directive]) : DirectiveSupport =
    new DirectiveSupport(blockDirectives ++ newBlockDirectives,
      spanDirectives ++ newSpanDirectives,
      templateDirectives ++ newTemplateDirectives,
      linkDirectives ++ newLinkDirectives)

}

/** Empty default instance without any directives installed.
  */
object DirectiveSupport extends DirectiveSupport(Nil, Nil, Nil, Nil)
