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

package laika.rst.ast

import laika.ast._
import laika.parse.SourceFragment

/** A two-column table-like structure used for bibliographic fields or directive options.
  */
case class FieldList(content: Seq[Field], options: Options = NoOpt) extends Block with ListContainer
    with RewritableContainer {
  type Self = FieldList

  override def rewriteChildren(rules: RewriteRules): FieldList = {
    val zippedContent = content.map(_.rewriteChildren(rules)).zip(content)
    if (zippedContent.forall { case (rewritten, old) => rewritten.eq(old) }) this
    else copy(content = zippedContent.map(_._1))
  }

  def withOptions(options: Options): FieldList = copy(options = options)
}

/** A single entry in a field list consisting of name and body.
  */
case class Field(name: Seq[Span], content: Seq[Block], options: Options = NoOpt) extends ListItem
    with BlockContainer {

  type Self = Field

  override def rewriteChildren(rules: RewriteRules): Field =
    copy(content = rules.rewriteBlocks(content), name = rules.rewriteSpans(name))

  def withContent(newContent: Seq[Block]): Field = copy(content = newContent)
  def withOptions(options: Options): Field       = copy(options = options)
}

/** A classifier for a term in a definition list.
  */
case class Classifier(content: Seq[Span], options: Options = NoOpt) extends Span
    with SpanContainer {
  type Self = Classifier
  def withContent(newContent: Seq[Span]): Classifier = copy(content = newContent)
  def withOptions(options: Options): Classifier      = copy(options = options)
}

/** A list of command line options and descriptions.
  */
case class OptionList(content: Seq[OptionListItem], options: Options = NoOpt) extends Block
    with ListContainer
    with RewritableContainer {
  type Self = OptionList

  override def rewriteChildren(rules: RewriteRules): OptionList =
    copy(content = content.map(_.rewriteChildren(rules)))

  def withOptions(options: Options): OptionList = copy(options = options)
}

/** A single item in an option list. The content property serves as the description of the option.
  */
case class OptionListItem(
    programOptions: Seq[ProgramOption],
    content: Seq[Block],
    options: Options = NoOpt
) extends ListItem
    with BlockContainer {
  type Self = OptionListItem
  def withContent(newContent: Seq[Block]): OptionListItem = copy(content = newContent)
  def withOptions(options: Options): OptionListItem       = copy(options = options)
}

/** A single option, including its name and all arguments, but not the description.
  */
case class ProgramOption(name: String, argument: Option[OptionArgument], options: Options = NoOpt)
    extends Element {
  type Self = ProgramOption
  def withOptions(options: Options): ProgramOption = copy(options = options)
}

/** A single option argument.
  */
case class OptionArgument(value: String, delimiter: String, options: Options = NoOpt)
    extends Element {
  type Self = OptionArgument
  def withOptions(options: Options): OptionArgument = copy(options = options)
}

/** A substitution definition with its span content that will be inserted
  *  wherever this substitution is referenced in flow content.
  */
case class SubstitutionDefinition(name: String, content: Span, options: Options = NoOpt)
    extends Definition with Hidden {
  type Self = SubstitutionDefinition
  def withOptions(options: Options): SubstitutionDefinition = copy(options = options)
}

/** Refers to a substitution definition with the same name.
  *  This type of element will only temporarily be part of the document tree and replaced
  *  by the content of the substitution definition in a rewrite step.
  */
case class SubstitutionReference(name: String, source: SourceFragment, options: Options = NoOpt)
    extends Reference {
  type Self = SubstitutionReference
  def withOptions(options: Options): SubstitutionReference = copy(options = options)
  lazy val unresolvedMessage: String = s"Unresolved substitution reference with name '$name'"
}

/** Represents an interactive Python session. Somewhat unlikely to be used in
  *  the context of this library, but included for the sake of completeness.
  */
case class DoctestBlock(content: String, options: Options = NoOpt) extends Block
    with TextContainer {
  type Self = DoctestBlock
  def withOptions(options: Options): DoctestBlock = copy(options = options)
}

/** Header decoration consisting of both an overline and an underline.
  */
case class OverlineAndUnderline(char: Char) extends HeaderDecoration

/** Header decoration consisting of an underline only.
  */
case class Underline(char: Char) extends HeaderDecoration

/** Temporary element to represent interpreted text with its associated role name.
  *  In a post-processing step this text will be replaced by the result of calling
  *  the corresponding role function.
  */
case class InterpretedText(
    role: String,
    content: String,
    source: SourceFragment,
    options: Options = NoOpt
) extends Reference with TextContainer {
  type Self = InterpretedText
  def withOptions(options: Options): InterpretedText = copy(options = options)
  lazy val unresolvedMessage: String = s"Unresolved interpreted text with role '$role'"
}

/** Temporary element to represent a customized text role that can be applied
  *  to spans of interpreted text. The `apply` function can then be applied
  *  to spans of interpreted text referring to the name of this role and passing
  *  the text as the argument to the function.
  */
case class CustomizedTextRole(name: String, apply: String => Span, options: Options = NoOpt)
    extends Definition with Hidden {
  type Self = CustomizedTextRole
  def withOptions(options: Options): CustomizedTextRole = copy(options = options)
}

/** Temporary element representing a file inclusion.
  * The path is interpreted as relative to the path of the processed document if it is not an absolute path.
  */
case class Include(path: String, source: SourceFragment, options: Options = NoOpt) extends Block
    with BlockResolver {

  type Self = Include
  def withOptions(options: Options): Include = copy(options = options)

  def resolve(cursor: DocumentCursor): Block =
    cursor.parent.target.selectDocument(path) match {
      case Some(target) => BlockSequence(target.content.content)
      case None         => InvalidBlock(s"Unresolvable path reference: $path", source)
    }

  def runsIn(phase: RewritePhase): Boolean = phase.isInstanceOf[RewritePhase.Render]
  lazy val unresolvedMessage: String       = s"Unresolved file inclusion with path '$path'"
}

/** Generates a table of contents element inside a topic.
  */
case class Contents(
    title: String,
    source: SourceFragment,
    depth: Int = Int.MaxValue,
    local: Boolean = false,
    options: Options = NoOpt
) extends Block with BlockResolver {

  type Self = Contents
  def withOptions(options: Options): Contents = copy(options = options)

  def resolve(cursor: DocumentCursor): Block = {
    val nav = cursor.target.asNavigationItem(
      NavigationBuilderContext(
        refPath = cursor.target.path,
        maxLevels = depth,
        currentLevel = 0
      )
    ).content
    TitledBlock(List(Text(title)), Seq(NavigationList(nav)), options + Style.nav)
  }

  def runsIn(phase: RewritePhase): Boolean = phase.isInstanceOf[RewritePhase.Render]
  lazy val unresolvedMessage: String = s"Unresolved table of contents generator with title '$title'"
}

/** A single item inside a line block.
  */
abstract class LineBlockItem extends Block with RewritableContainer {
  type Self <: LineBlockItem
}

/** A single line inside a line block.
  */
case class Line(content: Seq[Span], options: Options = NoOpt) extends LineBlockItem
    with SpanContainer {
  type Self = Line
  def withContent(newContent: Seq[Span]): Line = copy(content = newContent)
  def withOptions(options: Options): Line      = copy(options = options)
}

object Line extends SpanContainerCompanion {
  type ContainerType = Line
  protected def createSpanContainer(spans: Seq[Span]): Line = Line(spans)
}

/** A block containing lines which preserve line breaks and optionally nested line blocks.
  */
case class LineBlock(content: Seq[LineBlockItem], options: Options = NoOpt) extends LineBlockItem
    with ElementTraversal with RewritableContainer {
  type Self = LineBlock

  def rewriteChildren(rules: RewriteRules): LineBlock =
    copy(content = content.map(_.rewriteChildren(rules)))

  def withOptions(options: Options): LineBlock = copy(options = options)
}

object LineBlock {
  def apply(item: LineBlockItem, items: LineBlockItem*): LineBlock = LineBlock(item +: items.toList)
}

/** Represent a reference name.
  *  When resolving references whitespace needs to be normalized
  *  and the name converted to lower case.
  */
case class ReferenceName(original: String) {
  lazy val normalized: String = original.replaceAll("[\n ]+", " ").toLowerCase
}
