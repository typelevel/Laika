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

package laika.rst.bundle

import laika.ast.{Block, Span}
import laika.bundle.ExtensionBundle
import laika.rst.ext.Directives.Directive
import laika.rst.ext.{Directives, TextRoles}
import laika.rst.ext.TextRoles.TextRole
import laika.rst.std.{StandardBlockDirectives, StandardSpanDirectives, StandardTextRoles}

/** Registry for custom reStructuredText extensions. Application code can define
  * any number of instances mixing in this trait and then pass
  * them to Parse, Render or Transform operations:
  *
  * {{{
  * object MyExtensions extends RstExtensionRegistry {
  *   val spanDirectives = Seq(...)
  *   val blockDirectives = Seq(...)
  *   val textRoles = Seq(...)
  * }
  * object OtherExtensions extends RstExtensionRegistry {
  *   [...]
  * }
  *
  * Transform
  *   .from(ReStructuredText)
  *   .to(HTML)
  *   .using(MyDirectives, OtherDirectives)
  *   .fromFile("hello.rst")
  *   .toFile("hello.html")
  * }}}
  *
  * In contrast to the original Python implementation, this API has been redesigned to be a more
  *  idiomatic, concise and type-safe Scala DSL. See the documentation for the methods of this trait
  *  for concrete examples on how to implement an extension.
  *
  *  The following extension types are available:
  *
  *  - Block Directives - an extension hook for adding new block level elements to
  *    reStructuredText markup. Use the `blockDirectives` method of this class to
  *    add directive implementations to the parser. Specification entry:
  *    [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#directives]]
  *
  *  - Substitution Definitions - an extension hook for adding new span level elements to
  *    reStructuredText markup that can be used by substitution references (like `|subst|`).
  *    Use the `spanDirectives` method of this class to
  *    add directive implementations to the parser that can be used as substitution definitions.
  *    Specification entry:
  *    [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#substitution-definitions]]
  *
  *  - Interpreted Text Roles - an extension hook for adding new dynamic span level elements to
  *    reStructuredText markup. In contrast to substitution definitions the implementation of a text
  *    role uses the text from the occurrences in the markup referring to the role as input.
  *    Use the `textRoles` method of this class to
  *    add custom text role implementations to the parser that can be referred to by interpreted text.
  *    Specification entry:
  *    [[http://docutils.sourceforge.net/docs/ref/rst/directives.html#custom-interpreted-text-roles]]
  */
trait RstExtensionRegistry extends ExtensionBundle {

  override val useInStrictMode: Boolean = true

  /**  Registers the specified span directives.
    *  These span directives can then be referred to by substitution references.
    *
    *  Example:
    *
    *  {{{
    *  object MyDirectives extends RstExtensionRegistry {
    *    val spanDirectives = Seq(
    *      SpanDirective("replace") {
    *        spanContent map SpanSequence
    *      }
    *    )
    *    val blockDirectives = Seq()
    *    val textRoles = Seq()
    *  }
    *
    *  Transform.from(ReStructuredText).to(HTML)
    *    .using(MyDirectives)
    *    .fromFile("hello.rst").toFile("hello.html")
    *  }}}
    *
    *  For more details on implementing directives see [[laika.rst.ext.Directives]].
    */
  def spanDirectives: Seq[Directive[Span]]

  /**  Registers the specified block directives.
    *
    *  Example:
    *
    *  {{{
    *  case class Note (title: String, content: Seq[Block]) extends Block with BlockContainer[Note]
    *
    *  object MyDirectives extends RstExtensionRegistry {
    *    val blockDirectives = Seq(
    *      BlockDirective("note") {
    *        (argument() ~ blockContent)(Note)
    *      }
    *    )
    *    val spanDirectives = Seq()
    *    val textRoles = Seq()
    *  }
    *
    *  Transform.from(ReStructuredText).to(HTML)
    *    .using(MyDirectives)
    *    .fromFile("hello.rst").toFile("hello.html")
    *  }}}
    *
    *  For more details on implementing directives see [[laika.rst.ext.Directives]].
    */
  def blockDirectives: Seq[Directive[Block]]

  /**  Registers the specified text roles.
    *  These text roles may then be used in interpreted text spans.
    *
    *  Example:
    *
    *  {{{
    *  val textRole = TextRole("link", "http://www.company.com/main/")(field("base-url")) {
    *    (base, text) => Link(List(Text(text)), base + text)
    *  }
    *
    *  object MyDirectives extends RstExtensionRegistry {
    *    val textRoles = Seq(textRole)
    *    val spanDirectives = Seq()
    *    val blockDirectives = Seq()
    *  }
    *
    *  Transform from ReStructuredText to HTML using
    *    MyDirectives fromFile "hello.rst" toFile "hello.html"
    *  }}}
    *
    *  For more details on implementing directives see [[laika.rst.ext.TextRoles]].
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


/** Registry for all standard extensions of ReStructuredText as defined by the
  * specification, except for those which allow for raw content pass-through,
  * which are kept separately in `RawContentExtensions`.
  *
  * See [[http://docutils.sourceforge.net/docs/ref/rst/directives.html]] for details.
  *
  * This extension is installed by default when using the reStructuredText parser.
  */
object StandardExtensions extends RstExtensionRegistry {

  lazy val blockDirectives = (new StandardBlockDirectives).blockDirectives
  lazy val spanDirectives = (new StandardSpanDirectives).spanDirectives
  lazy val textRoles = (new StandardTextRoles).allRoles

}

/** Registry for the standard extensions of ReStructuredText which allow for
  * raw content pass-through. These have to be enabled to call `withRawContent`
  * on the `Parse` or `Transform` APIs:
  *
  * {{{
  * Transform
  *   .from(ReStructuredText)
  *   .to(HTML)
  *   .withRawContent
  *   .fromFile("hello.rst")
  *   .toFile("hello.html")
  * }}}
  */
object RawContentExtensions extends RstExtensionRegistry {

  override val acceptRawContent = true
  lazy val blockDirectives = Seq((new StandardBlockDirectives).rawDirective)
  lazy val spanDirectives = Seq()
  lazy val textRoles = Seq((new StandardTextRoles).rawTextRole)

}
