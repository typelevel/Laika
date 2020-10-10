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

import laika.ast.{DocumentCursor, InvalidSpan, LinkIdReference, NoOpt, Options, Replace, RewriteRules, Span, SpanResolver}
import laika.bundle.{BundleOrigin, ConfigProvider, ExtensionBundle, ParserBundle}
import laika.config.ConfigParser
import laika.parse.{Parser, SourceFragment}
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
      SpanDirectiveParsers.contextRef
    ),
    configProvider = Some(configProvider),
    templateParser = Some(new TemplateParsers(Templates.toMap(templateDirectives)).templateRoot)
  )
  
  private val linkDirectiveMap = linkDirectives.map(d => (d.name, d)).toMap
  private val linkParser = (DirectiveParsers.nameDecl <~ ws) ~ ("(" ~> text(delimitedBy(')')).embed("\\" ~> TextParsers.oneChar))
  
  case class LinkDirectiveResolver(ref: LinkIdReference,
                                   directiveName: String,
                                   typeName: String,
                                   source: SourceFragment,
                                   options: Options = NoOpt) extends SpanResolver {
    type Self = LinkDirectiveResolver

    def resolve (cursor: DocumentCursor): Span = linkDirectiveMap.get(directiveName)
      .fold[Span](InvalidSpan(s"Unknown link directive: $directiveName", source)) { dir =>
        dir(typeName, cursor).fold(
          err => InvalidSpan(s"Invalid link directive: $err", source),
          res => res.copy(content = ref.content, options = res.options + ref.options)
        )
      }

    def withOptions(options: Options): LinkDirectiveResolver = copy(options = options)

    def unresolvedMessage: String = s"unresolved api directive for type $typeName"
  }
  
  override lazy val rewriteRules: Seq[DocumentCursor => RewriteRules] = Seq(
    _ => RewriteRules.forSpans {
      case ref: LinkIdReference if ref.ref.startsWith("@:") => 
        linkParser.parse(ref.ref.drop(2)).toEither.fold(
          err => Replace(InvalidSpan(s"Invalid link directive: $err", ref.source)),
          res => Replace(LinkDirectiveResolver(ref, res._1, res._2, ref.source, ref.options))
        )
        // In the current design for rewrite rules which does not allow to specify a phase to be executed in,
        // we need to insert a span resolver to get into a later phase where target ids have all been resolved.
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
