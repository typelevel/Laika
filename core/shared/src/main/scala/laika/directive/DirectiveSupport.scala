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

import laika.ast.RewriteRules.RewritePhaseBuilder
import laika.ast.{
  DocumentCursor,
  InvalidSpan,
  LinkIdReference,
  LinkPathReference,
  NoOpt,
  Options,
  Replace,
  RewritePhase,
  RewriteRules,
  Span,
  SpanResolver
}
import laika.bundle.{ BundleOrigin, ConfigProvider, ExtensionBundle, ParserBundle }
import laika.config.ConfigParser
import laika.parse.{ Parser, SourceFragment }
import laika.parse.builders.{ delimitedBy, text, ws }
import laika.parse.combinator.Parsers
import laika.parse.implicits._
import laika.parse.directive.{
  BlockDirectiveParsers,
  ConfigHeaderParser,
  DirectiveParsers,
  SpanDirectiveParsers,
  TemplateParsers
}
import laika.parse.text.TextParsers

/** Internal API that processes all directives defined
  * by one or more DirectiveRegistries. This extension
  * is installed by default, unless the transformation
  * is run in strict mode.
  *
  * @author Jens Halm
  */
private[laika] class DirectiveSupport(
    blockDirectives: Seq[Blocks.Directive],
    spanDirectives: Seq[Spans.Directive],
    templateDirectives: Seq[Templates.Directive],
    linkDirectives: Seq[Links.Directive],
    strictMode: Boolean
) extends ExtensionBundle { self =>

  val description: String = "Laika's directive support"

  override val origin: BundleOrigin = BundleOrigin.Library

  private val configProvider: ConfigProvider = new ConfigProvider {

    def markupConfigHeader: Parser[ConfigParser] =
      if (strictMode) Parsers.success(ConfigParser.empty)
      else ConfigHeaderParser.withDefaultLineDelimiters

    def templateConfigHeader: Parser[ConfigParser]  = ConfigHeaderParser.withDefaultLineDelimiters
    def configDocument(input: String): ConfigParser = ConfigParser.parse(input)
  }

  override lazy val parsers: ParserBundle = new ParserBundle(
    blockParsers =
      if (strictMode) Nil
      else Seq(BlockDirectiveParsers.blockDirective(Blocks.toMap(blockDirectives))),
    spanParsers =
      if (strictMode) Nil
      else
        Seq(
          SpanDirectiveParsers.spanDirective(
            Spans.toMap(spanDirectives ++ linkDirectives.map(_.asSpanDirective))
          ),
          SpanDirectiveParsers.contextRef
        ),
    configProvider = Some(configProvider),
    templateParser = Some(new TemplateParsers(Templates.toMap(templateDirectives)).templateRoot)
  )

  private val linkDirectiveMap = linkDirectives.map(d => (d.name, d)).toMap

  private val linkParser = (DirectiveParsers.nameDecl <~ ws) ~ ("(" ~> text(delimitedBy(')')).embed(
    "\\" ~> TextParsers.oneChar
  ))

  case class LinkDirectiveResolver(
      ref: LinkIdReference,
      directiveName: String,
      typeName: String,
      source: SourceFragment,
      options: Options = NoOpt
  ) extends SpanResolver {
    type Self = LinkDirectiveResolver

    def resolve(cursor: DocumentCursor): Span = linkDirectiveMap.get(directiveName)
      .fold[Span](InvalidSpan(s"Unknown link directive: $directiveName", source)) { dir =>
        dir(typeName, cursor).fold(
          err => InvalidSpan(s"Invalid link directive: $err", source),
          res => res.copy(content = ref.content, options = res.options + ref.options)
        )
      }

    def withOptions(options: Options): LinkDirectiveResolver = copy(options = options)

    def runsIn(phase: RewritePhase): Boolean = phase.isInstanceOf[RewritePhase.Render]

    def unresolvedMessage: String = s"unresolved api directive for type $typeName"
  }

  private case class LinkDirectiveResolver2(
      ref: LinkPathReference,
      directiveName: String,
      typeName: String,
      source: SourceFragment,
      options: Options = NoOpt
  ) extends SpanResolver {
    type Self = LinkDirectiveResolver2

    def resolve(cursor: DocumentCursor): Span = linkDirectiveMap.get(directiveName)
      .fold[Span](InvalidSpan(s"Unknown link directive: $directiveName", source)) { dir =>
        dir(typeName, cursor).fold(
          err => InvalidSpan(s"Invalid link directive: $err", source),
          res => res.copy(content = ref.content, options = res.options + ref.options)
        )
      }

    def withOptions(options: Options): LinkDirectiveResolver2 = copy(options = options)

    def runsIn(phase: RewritePhase): Boolean = phase.isInstanceOf[RewritePhase.Render]

    def unresolvedMessage: String = s"unresolved api directive for type $typeName"
  }

  override lazy val rewriteRules: RewritePhaseBuilder = { case RewritePhase.Resolve =>
    if (strictMode) Nil
    else
      Seq(RewriteRules.forSpans {
        case ref: LinkPathReference if ref.path.toString.startsWith("@:") =>
          linkParser.parse(ref.path.toString.drop(2)).toEither.fold(
            err => Replace(InvalidSpan(s"Invalid link directive: $err", ref.source)),
            res => Replace(LinkDirectiveResolver2(ref, res._1, res._2, ref.source, ref.options))
          )
      }.asBuilder)
  }

  /** Hook for extension registries for adding block, span and template directives.
    */
  def withDirectives(
      newBlockDirectives: Seq[Blocks.Directive],
      newSpanDirectives: Seq[Spans.Directive],
      newTemplateDirectives: Seq[Templates.Directive],
      newLinkDirectives: Seq[Links.Directive]
  ): DirectiveSupport =
    new DirectiveSupport(
      blockDirectives ++ newBlockDirectives,
      spanDirectives ++ newSpanDirectives,
      templateDirectives ++ newTemplateDirectives,
      linkDirectives ++ newLinkDirectives,
      strictMode
    )

  override def forStrictMode: Option[ExtensionBundle] =
    Some(new DirectiveSupport(Nil, Nil, templateDirectives, Nil, strictMode = true))

}

/** Empty default instance without any directives installed.
  */
private[laika] object DirectiveSupport
    extends DirectiveSupport(Nil, Nil, Nil, Nil, strictMode = false)
