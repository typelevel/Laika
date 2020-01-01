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

package laika.rst.bundle

import laika.ast.{Block, DocumentCursor, RewriteRule, Span}
import laika.bundle.{BundleOrigin, ExtensionBundle, ParserBundle}
import laika.parse.markup.RecursiveParsers
import laika.rst.InlineParsers
import laika.rst.ext.Directives.Directive
import laika.rst.ext.ExtensionParsers
import laika.rst.ext.TextRoles.TextRole

/** Internal API that processes all extensions defined
  * by one or more RstExtensionRegistries. This extension
  * is installed by default when using the reStructuredText
  * parser.
  *
  * @author Jens Halm
  */
class RstExtensionSupport (blockDirectives: Seq[Directive[Block]],
                           spanDirectives: Seq[Directive[Span]],
                           textRoles: Seq[TextRole],
                           defaultTextRole: String) extends ExtensionBundle {

  override val origin: BundleOrigin = BundleOrigin.Parser
  
  override val useInStrictMode: Boolean = true

  override def rewriteRules: Seq[DocumentCursor => laika.ast.RewriteRules] = Seq(new RewriteRules(textRoles))

  override lazy val parsers: ParserBundle = ParserBundle(
    blockParsers = Seq(
      ExtensionParsers.allBlocks(blockDirectives, spanDirectives, textRoles, defaultTextRole)
    ),
    spanParsers = Seq(
      InlineParsers.interpretedTextWithRoleSuffix(defaultTextRole)
    )
  )

  def withDirectives (newBlockDirectives: Seq[Directive[Block]],
                      newSpanDirectives: Seq[Directive[Span]],
                      newTextRoles: Seq[TextRole],
                      newDefaultTextRole: Option[String] = None) : RstExtensionSupport =
    new RstExtensionSupport(
      blockDirectives ++ newBlockDirectives,
      spanDirectives ++ newSpanDirectives,
      textRoles ++ newTextRoles,
      newDefaultTextRole.getOrElse(defaultTextRole))

}

/** Empty base instance as a basis for registering reStructuredText extensions.
  */
object RstExtensionSupport extends RstExtensionSupport(Nil, Nil, Nil, "title-reference")

/** Common base trait for reStructuredText extensions (directives and text roles).
  */
trait RstExtension[P] {

  /** The name the extension is identified by in text markup.
    */
  def name: String

  /** The factory creating an instance of the extension based
    * on the recursive parsers of the host language.
    */
  def part: RecursiveParsers => P

}

/** Companion with utilities for initializing extensions.
  */
object RstExtension {

  /** Initializes the specified extensions with the given recursive parsers
    * and returns them as a map keyed by the name of the extension.
    */
  def createAsMap[P] (ext: Seq[RstExtension[P]], recParsers: RecursiveParsers): Map[String, P] =
    ext map { e => (e.name.toLowerCase, e.part(recParsers)) } toMap

}
