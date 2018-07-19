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

import laika.api.ext.{ExtensionBundle, ParserDefinitionBuilders}
import laika.parse.core.markup.RecursiveParsers
import laika.parse.rst.InlineParsers
import laika.parse.rst.ext.Directives.Directive
import laika.parse.rst.ext.TextRoles.TextRole
import laika.rewrite.DocumentCursor
import laika.tree.Elements.{Block, RewriteRule, Span}

/**
  * @author Jens Halm
  */
class RstExtensionSupport (blockDirectives: Seq[Directive[Block]],
                           spanDirectives: Seq[Directive[Span]],
                           textRoles: Seq[TextRole],
                           defaultTextRole: String) extends ExtensionBundle {

  override val useInStrictMode: Boolean = true

  override def rewriteRules: Seq[DocumentCursor => RewriteRule] = Seq(new RewriteRules(textRoles))

  override lazy val parserDefinitions: ParserDefinitionBuilders = {
    ParserDefinitionBuilders(
      blockParsers = Seq(
        RstExtensionParsers.allBlocks(blockDirectives, spanDirectives, textRoles, defaultTextRole)
      ),
      spanParsers = Seq(
        InlineParsers.interpretedTextWithRoleSuffix(defaultTextRole)
      )
    )
  }

  def withDirectives (newBlockDirectives: Seq[Directive[Block]],
                      newSpanDirectives: Seq[Directive[Span]],
                      newTextRoles: Seq[TextRole],
                      newDefaultTextRole: Option[String] = None) : RstExtensionSupport =
    new RstExtensionSupport(
      blockDirectives ++ newBlockDirectives,
      spanDirectives ++ newSpanDirectives,
      textRoles ++ newTextRoles,
      newDefaultTextRole.getOrElse(defaultTextRole))

  // val textRoleElements = textRoles map { role => CustomizedTextRole(role.name, role.default) }
}

object RstExtensionSupport extends RstExtensionSupport(Nil, Nil, Nil, "title-reference")

trait RstExtension[P] {

  def name: String

  def part: RecursiveParsers => P

}

object RstExtension {

  def createAsMap[P] (ext: Seq[RstExtension[P]], recParsers: RecursiveParsers): Map[String, P] =
    ext map { e => (e.name.toLowerCase, e.part(recParsers)) } toMap

}

trait RstExtensionRegistry extends ExtensionBundle {

  override val useInStrictMode: Boolean = true

  /**  Registers the specified span directives.
    *  These span directives can then be referred to by substitution references.
    *
    *  Example:
    *
    *  {{{
    *  val rst = ReStructuredText withSpanDirectives (
    *    SpanDirective("replace") {
    *      spanContent map SpanSequence
    *    }
    *  )
    *
    *  Transform from rst to HTML fromFile "hello.rst" toFile "hello.html"
    *  }}}
    *
    *  For more details on implementing directives see [[Directives]].
    */
  def spanDirectives: Seq[Directive[Span]]

  /**  Registers the specified block directives.
    *
    *  Example:
    *
    *  {{{
    *  case class Note (title: String, content: Seq[Block]) extends Block with BlockContainer[Note]
    *
    *  val rst = ReStructuredText withBlockDirectives (
    *    BlockDirective("note") {
    *      (argument() ~ blockContent)(Note)
    *    }
    *  )
    *
    *  Transform from rst to HTML fromFile "hello.rst" toFile "hello.html"
    *  }}}
    *
    *  For more details on implementing directives see [[Directives]].
    */
  def blockDirectives: Seq[Directive[Block]]

  /**  Registers the specified text roles.
    *  These text roles may then be used in interpreted text spans.
    *
    *  Example:
    *
    *  {{{
    *  val rst = ReStructuredText withTextRoles (
    *    TextRole("link", "http://www.our-server.com/tickets/")(field("base-url")) {
    *      (base, text) => Link(List(Text(text)), base + text)
    *    }
    *  )
    *
    *  Transform from rst to HTML fromFile "hello.rst" toFile "hello.html"
    *  }}}
    *
    *  For more details on implementing directives see [[TextRoles]].
    */
  def textRoles: Seq[TextRole]

  /**  Overrides the name of the default text role to apply when interpreted text
    *  is used in markup without an explicit role name.
    */
  def defaultTextRole: Option[String] = None


  override def processExtension: PartialFunction[ExtensionBundle, ExtensionBundle] = {
    case ds: RstExtensionSupport => ds.withDirectives(blockDirectives, spanDirectives, textRoles, defaultTextRole)
  }

}

object StandardExtensions extends RstExtensionRegistry {

  lazy val blockDirectives = (new StandardBlockDirectives).blockDirectives
  lazy val spanDirectives = (new StandardSpanDirectives).spanDirectives
  lazy val textRoles = (new StandardTextRoles).allRoles

}

object RawContentExtensions extends RstExtensionRegistry {

  lazy val blockDirectives = Seq((new StandardBlockDirectives).rawDirective)
  lazy val spanDirectives = Seq()
  lazy val textRoles = Seq((new StandardTextRoles).rawTextRole)

}
