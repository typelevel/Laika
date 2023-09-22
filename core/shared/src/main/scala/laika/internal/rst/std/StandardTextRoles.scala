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

package laika.internal.rst.std

import cats.data.NonEmptySet
import laika.ast.*
import laika.internal.rst.ast.RstStyle
import laika.internal.rst.ext.TextRoles.TextRole
import laika.parse.GeneratedSource
import laika.parse.builders.~
import laika.internal.rst.ext.TextRoles.Parts.*

import scala.collection.immutable.TreeSet

/** Defines all supported standard text roles of the reStructuredText reference parser.
  *
  *  The following text roles are fully supported:
  *
  *  - `emphasis`
  *  - `strong`
  *  - `literal`
  *  - `subscript` (and `sub` alias)
  *  - `superscript` (and `sup` alias)
  *  - `title-reference` (and `title` alias) - the default role
  *  - `raw` (+ format option) (needs to be enabled explicitly through `ReStructuredText.withRawElements`)
  *
  *  The following text role is supported with some limitation:
  *
  *  - `code` does currently not support syntax highlighting
  *    (it allows to set the language so client-side highlighters can be integrated if required)
  *
  *  The following text roles are not supported:
  *
  *  - `math`
  *  - `pep-reference`
  *  - `rfc-reference`
  *
  *  In contrast to the reference parser the `default-role` directive cannot be supported
  *  in Laika as the parser is not stateful. Therefore it is not possible to change the
  *  default role half-way through a parsing process. However the default role can be specified
  *  through the API when creating a parser instance:
  *
  *  {{{
  *  object RstExtensions extends RstExtensionRegistry {
  *    val blockDirectives = Nil
  *    val spanDirectives = Nil
  *    val textRoles = Nil
  *    override val defaultTextRole = "my-role-name"
  *  }
  *
  *  val transformer = Transformer.from(ReStructuredText).to(HTML).using(RstExtensions)
  *  }}}
  *
  *  See [[http://docutils.sourceforge.net/docs/ref/rst/roles.html]] for details.
  *
  *  @author Jens Halm
  */
private[rst] class StandardTextRoles {

  private val classOption = optField(
    "class",
    opt => Right(Options(None, opt.input.split(" ").toSet))
  ) map (_.getOrElse(Options.empty))

  /** The standard emphasis text role.
    */
  lazy val emphasis: TextRole =
    TextRole("emphasis", Options.empty)(classOption)((opt, text) =>
      Emphasized(List(Text(text)), opt)
    )

  /** The standard strong text role.
    */
  lazy val strong: TextRole =
    TextRole("strong", Options.empty)(classOption)((opt, text) => Strong(List(Text(text)), opt))

  /** The standard literal text role.
    */
  lazy val literal: TextRole =
    TextRole("literal", Options.empty)(classOption)((opt, text) => Literal(text, opt))

  /** The standard subscript text role.
    */
  lazy val subscript: TextRole =
    TextRole("subscript", Options.empty)(classOption)((opt, text) =>
      Text(text, opt + RstStyle.subscript)
    )

  /** The standard superscript text role.
    */
  lazy val superscript: TextRole =
    TextRole("superscript", Options.empty)(classOption)((opt, text) =>
      Text(text, opt + RstStyle.superscript)
    )

  /** The sub text role, an alias for the subscript role.
    */
  lazy val sub: TextRole =
    TextRole("sub", Options.empty)(classOption)((opt, text) => Text(text, opt + RstStyle.subscript))

  /** The sup text role, an alias for the superscript role.
    */
  lazy val sup: TextRole =
    TextRole("sup", Options.empty)(classOption)((opt, text) =>
      Text(text, opt + RstStyle.superscript)
    )

  /** The standard title-reference text role, the default text role in reStructuredText unless overridden
    *  with `RstExtensionRegistry.defaultTextRole`.
    */
  lazy val titleRef: TextRole =
    TextRole("title-reference", Options.empty)(classOption)((opt, text) =>
      Emphasized(List(Text(text)), opt + RstStyle.titleReference)
    )

  /** The title text role, an alias for the title-reference role.
    */
  lazy val title: TextRole =
    TextRole("title", Options.empty)(classOption)((opt, text) =>
      Emphasized(List(Text(text)), opt + RstStyle.titleReference)
    )

  /** The standard code text role. The current implementation does not support syntax highlighting.
    */
  lazy val codeSpan: TextRole =
    TextRole("code", ("", Options.empty)) {
      (optField("language") ~ classOption).map { case lang ~ opt => (lang.getOrElse(""), opt) }
    } { case ((lang, opt), text) =>
      InlineCode(lang, List(Text(text)), opt)
    }

  /** The raw text role, which is not enabled by default,
    *  see [[http://docutils.sourceforge.net/docs/ref/rst/roles.html#raw]] for details.
    *  It can be enabled with `Transformer.from(ReStructuredText).to(HTML).withRawContent`.
    */
  lazy val rawTextRole: TextRole =
    TextRole("raw", (Nil: List[String], Options.empty)) {
      (field("format") ~ classOption).map { case format ~ opt => (format.split(" ").toList, opt) }
    } { case ((formats, opt), content) =>
      NonEmptySet.fromSet(TreeSet(formats: _*)) match {
        case Some(set) => RawContent(set, content, opt)
        case None      => InvalidSpan("no format specified", GeneratedSource)
      }
    }

  /** All standard text roles currently supported by Laika, except for
    *  the `raw` text role which needs to be enabled explicitly.
    */
  lazy val allRoles: List[TextRole] =
    List(emphasis, strong, literal, subscript, superscript, sub, sup, titleRef, title, codeSpan)

}
