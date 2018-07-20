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

package laika.parse.rst.ext

import laika.api.ext.{ExtensionBundle, MarkupExtensions, ParserDefinitionBuilders, RootParserHooks}
import laika.parse.core.markup.RootParser
import laika.parse.rst.{LinkTargetProcessor, ReStructuredText}
import laika.parse.rst.ext.Directives.Directive
import laika.parse.rst.ext.TextRoles.TextRole
import laika.tree.Elements.{Block, Span}

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
      blockParsers = finalBundle.parserDefinitions.blockParsers,
      spanParsers = finalBundle.parserDefinitions.spanParsers,
      rootParserHooks = RootParserHooks(postProcessBlocks = LinkTargetProcessor)
    )
    new RootParser(ReStructuredText, markupExtensions)
  }

}
