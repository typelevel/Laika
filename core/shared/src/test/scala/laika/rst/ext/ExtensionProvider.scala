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

package laika.rst.ext

import laika.ast.{Block, Span}
import laika.bundle.{ExtensionBundle, MarkupExtensions, ParserHooks}
import laika.format.ReStructuredText
import laika.parse.markup.RootParser
import laika.rst.bundle.{LinkTargetProcessor, RstExtensionRegistry, RstExtensionSupport}
import laika.rst.ext.Directives.Directive
import laika.rst.ext.TextRoles.TextRole

/**
  * @author Jens Halm
  */
object ExtensionProvider {

  def forDefaultTextRole (name: String): ExtensionBundle = new RstExtensionRegistry {
    val blockDirectives = Nil
    val spanDirectives = Nil
    val textRoles = Nil
    override val defaultTextRole = Some(name)
  }

  def forExtensions (blocks: Seq[Directive[Block]] = Nil,
                     spans: Seq[Directive[Span]] = Nil,
                     roles: Seq[TextRole] = Nil): ExtensionBundle = new RstExtensionRegistry {
    val blockDirectives = blocks
    val spanDirectives = spans
    val textRoles = roles
  }

}

object RootParserProvider {

  def forBundle (bundle: ExtensionBundle): RootParser = {
    val finalBundle = bundle.processExtension(RstExtensionSupport)
    val markupExtensions = MarkupExtensions(
      blockParsers = finalBundle.parsers.blockParsers,
      spanParsers = finalBundle.parsers.spanParsers,
      syntaxHighlighters = Nil,
      parserHooks = ParserHooks(postProcessBlocks = LinkTargetProcessor)
    )
    new RootParser(ReStructuredText, markupExtensions)
  }

}
