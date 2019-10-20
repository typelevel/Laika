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

import laika.bundle.{ConfigProvider, ExtensionBundle, ParserBundle, UnresolvedConfig}
import laika.parse.Parser
import laika.parse.directive.{BlockDirectiveParsers, ConfigHeaderParser, SpanDirectiveParsers, TemplateParsers}

/** Internal API that processes all directives defined
  * by one or more DirectiveRegistries. This extension
  * is installed by default, unless the transformation
  * is run in strict mode.
  *
  * @author Jens Halm
  */
class DirectiveSupport (blockDirectives: Seq[Blocks.Directive],
                        spanDirectives: Seq[Spans.Directive],
                        templateDirectives: Seq[Templates.Directive]) extends ExtensionBundle {

  private val configProvider: ConfigProvider = new ConfigProvider {
    def configHeader: Parser[UnresolvedConfig] = ConfigHeaderParser.withDefaultLineDelimiters
    def configDocument (input: String): UnresolvedConfig = UnresolvedConfig.default(input)
  }
  
  override lazy val parsers: ParserBundle = ParserBundle(
    blockParsers = Seq(BlockDirectiveParsers.blockDirective(Blocks.toMap(blockDirectives))),
    spanParsers = Seq(
      SpanDirectiveParsers.spanDirective(Spans.toMap(spanDirectives)), 
      SpanDirectiveParsers.contextRef, 
      SpanDirectiveParsers.legacyContextRef
    ),
    configProvider = Some(configProvider),
    templateParser = Some(new TemplateParsers(Templates.toMap(templateDirectives)).templateRoot)
  )

  /** Hook for extension registries for adding block, span and template directives.
    */
  def withDirectives (newBlockDirectives: Seq[Blocks.Directive],
                      newSpanDirectives: Seq[Spans.Directive],
                      newTemplateDirectives: Seq[Templates.Directive]) : DirectiveSupport =
    new DirectiveSupport(blockDirectives ++ newBlockDirectives,
      spanDirectives ++ newSpanDirectives,
      templateDirectives ++ newTemplateDirectives)

}

/** Empty default instance without any directives installed.
  */
object DirectiveSupport extends DirectiveSupport(Nil, Nil, Nil)
