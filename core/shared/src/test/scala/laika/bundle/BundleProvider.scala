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

package laika.bundle

import laika.config.{Config, ConfigParser}
import laika.ast._
import laika.directive.{DirectiveRegistry, Templates}
import laika.parse.Parser
import laika.parse.markup.DocumentParser.ParserInput

/**
  * @author Jens Halm
  */
object BundleProvider {
  
  class TestExtensionBundle (override val origin: BundleOrigin = BundleOrigin.User) extends ExtensionBundle {
    val description: String = "Extensions under test"
  }

  def forMarkupParser (blockParsers: Seq[BlockParserBuilder] = Nil,
                       spanParsers: Seq[SpanParserBuilder] = Nil,
                       origin: BundleOrigin = BundleOrigin.User): ExtensionBundle = new TestExtensionBundle(origin) {

    override def parsers: ParserBundle = ParserBundle(
      blockParsers = blockParsers,
      spanParsers = spanParsers
    )

  }

  def forParserHooks (postProcessBlocks: Seq[Block] => Seq[Block] = identity,
                      postProcessDocument: UnresolvedDocument => UnresolvedDocument = identity,
                      preProcessInput: ParserInput => ParserInput = identity,
                      origin: BundleOrigin = BundleOrigin.User): ExtensionBundle = new TestExtensionBundle(origin) {

    override def parsers: ParserBundle = ParserBundle(
      markupParserHooks = Some(ParserHooks(
        postProcessBlocks = postProcessBlocks,
        postProcessDocument = postProcessDocument,
        preProcessInput = preProcessInput
      ))
    )

  }

  def forConfigProvider (provider: ConfigProvider, origin: BundleOrigin = BundleOrigin.User): TestExtensionBundle = new TestExtensionBundle(origin) {

    override def parsers: ParserBundle = ParserBundle(
      configProvider = Some(provider)
    )

  }

  def forConfigString (input: String, origin: BundleOrigin = BundleOrigin.User): TestExtensionBundle = new TestExtensionBundle(origin) {

    override def baseConfig: Config = ConfigParser.parse(input).resolve().toOption.get

  }

  def forDocTypeMatcher (matcher: PartialFunction[Path, DocumentType]): ExtensionBundle = new TestExtensionBundle() {

    override def docTypeMatcher: PartialFunction[Path, DocumentType] = matcher

  }

  def forDocTypeMatcher (origin: BundleOrigin)(matcher: PartialFunction[Path, DocumentType]): ExtensionBundle = new TestExtensionBundle(origin) {

    override def docTypeMatcher: PartialFunction[Path, DocumentType] = matcher

  }

  def forSlugBuilder (f: String => String, origin: BundleOrigin = BundleOrigin.User): ExtensionBundle = new TestExtensionBundle(origin) {

    override def slugBuilder: Option[String => String] = Some(f)

  }

  def forSpanRewriteRule (rule: RewriteRule[Span]): ExtensionBundle = new TestExtensionBundle() {

    override def rewriteRules: Seq[DocumentCursor => RewriteRules] = Seq(_ => laika.ast.RewriteRules.forSpans(rule))

  }

  def forSpanRewriteRule (origin: BundleOrigin)(rule: RewriteRule[Span]): ExtensionBundle = new TestExtensionBundle(origin) {

    override def rewriteRules: Seq[DocumentCursor => RewriteRules] = Seq(_ => laika.ast.RewriteRules.forSpans(rule))

  }

  def forTemplateParser(parser: Parser[TemplateRoot], origin: BundleOrigin = BundleOrigin.User): ExtensionBundle = new TestExtensionBundle(origin) {

    override def parsers: ParserBundle = ParserBundle(
      templateParser = Some(parser)
    )

  }

  def forTemplateDirective(directive: Templates.Directive, bundleOrigin: BundleOrigin = BundleOrigin.User): ExtensionBundle = new DirectiveRegistry {

    override def origin: BundleOrigin = bundleOrigin
    
    val templateDirectives = Seq(directive)
    val spanDirectives = Seq()
    val blockDirectives = Seq()
    val linkDirectives = Seq()

  }

  def forStyleSheetParser (parser: Parser[Set[StyleDeclaration]], origin: BundleOrigin = BundleOrigin.User): ExtensionBundle = new TestExtensionBundle(origin) {

    override def parsers: ParserBundle = ParserBundle(
      styleSheetParser = Some(parser)
    )

  }

  def forOverrides (overrides: RenderOverrides, origin: BundleOrigin = BundleOrigin.User): ExtensionBundle = new TestExtensionBundle(origin) {

    override def renderOverrides: Seq[RenderOverrides] = Seq(overrides)

  }

}
