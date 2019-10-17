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

package laika.bundle

import laika.api.config.Config
import laika.ast.Path.Root
import laika.ast._
import laika.directive.{DirectiveRegistry, Templates}
import laika.parse.Parser
import laika.parse.markup.DocumentParser.ParserInput

/**
  * @author Jens Halm
  */
object BundleProvider {

  def forMarkupParser (blockParsers: Seq[BlockParserBuilder] = Nil,
                       spanParsers: Seq[SpanParserBuilder] = Nil): ExtensionBundle = new ExtensionBundle {

    override def parsers: ParserBundle = ParserBundle(
      blockParsers = blockParsers,
      spanParsers = spanParsers
    )

  }

  def forParserHooks (postProcessBlocks: Seq[Block] => Seq[Block] = identity,
                      postProcessDocument: UnresolvedDocument => UnresolvedDocument = identity,
                      preProcessInput: ParserInput => ParserInput = identity): ExtensionBundle = new ExtensionBundle {

    override def parsers: ParserBundle = ParserBundle(
      markupParserHooks = Some(ParserHooks(
        postProcessBlocks = postProcessBlocks,
        postProcessDocument = postProcessDocument,
        preProcessInput = preProcessInput
      ))
    )

  }

  def forConfigProvider (provider: ConfigProvider): ExtensionBundle = new ExtensionBundle {

    override def parsers: ParserBundle = ParserBundle(
      configProvider = Some(provider)
    )

  }

  def forConfigString (input: String): ExtensionBundle = new ExtensionBundle {

    override def baseConfig: Config = ConfigProvider.fromInput(input, Root)

  }

  def forDocTypeMatcher (matcher: PartialFunction[Path, DocumentType]): ExtensionBundle = new ExtensionBundle {

    override def docTypeMatcher: PartialFunction[Path, DocumentType] = matcher

  }

  def forSpanRewriteRule (rule: RewriteRule[Span]): ExtensionBundle = new ExtensionBundle {

    override def rewriteRules: Seq[DocumentCursor => RewriteRules] = Seq(_ => laika.ast.RewriteRules.forSpans(rule))

  }

  def forTemplateParser(parser: Parser[TemplateRoot]): ExtensionBundle = new ExtensionBundle {

    override def parsers: ParserBundle = ParserBundle(
      templateParser = Some(parser)
    )

  }

  def forTemplateDirective(directive: Templates.Directive): ExtensionBundle = new DirectiveRegistry {

    val templateDirectives = Seq(directive)
    val spanDirectives = Seq()
    val blockDirectives = Seq()

  }

  def forStyleSheetParser (parser: Parser[Set[StyleDeclaration]]): ExtensionBundle = new ExtensionBundle {

    override def parsers: ParserBundle = ParserBundle(
      styleSheetParser = Some(parser)
    )

  }

  def forTheme (theme: RenderTheme): ExtensionBundle = new ExtensionBundle {

    override def themes: Seq[RenderTheme] = Seq(theme)

  }

}
