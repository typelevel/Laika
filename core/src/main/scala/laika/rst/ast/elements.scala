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

package laika.rst.ast

import laika.ast._
import laika.rewrite.nav.TocGenerator


/** A two-column table-like structure used for bibliographic fields or directive options.
  */
case class FieldList (content: Seq[Field], options: Options = NoOpt) extends Block with ListContainer[FieldList]
                                                                                   with RewritableContainer[FieldList] {

  override def rewriteChildren (rules: RewriteRules): FieldList = {
    val zippedContent = content.map(_.rewriteChildren(rules)).zip(content)
    if (zippedContent.forall { case (rewritten, old) => rewritten.eq(old) }) this
    else copy(content = zippedContent.map(_._1))
  }
}

/** A single entry in a field list consisting of name and body.
  */
case class Field (name: Seq[Span], content: Seq[Block], options: Options = NoOpt) extends ListItem with BlockContainer[Field] {
  override def rewriteChildren (rules: RewriteRules): Field = (rules.rewriteBlocks(content), rules.rewriteSpans(name)) match {
    case (None, None) => this
    case (newContent, newName) => copy(content = newContent.getOrElse(content), name = newName.getOrElse(name))
  }
  def withContent (newContent: Seq[Block]): Field = copy(content = content)
}

/** A classifier for a term in a definition list.
  */
case class Classifier (content: Seq[Span], options: Options = NoOpt) extends Span with SpanContainer[Classifier] {
  def withContent (newContent: Seq[Span]): Classifier = copy(content = newContent)
}

/** A list of command line options and descriptions.
  */
case class OptionList (content: Seq[OptionListItem], options: Options = NoOpt) extends Block with ListContainer[OptionList]
                                                                                             with RewritableContainer[OptionList] {

  override def rewriteChildren (rules: RewriteRules): OptionList = {
    val rewrittenItems = content.map(i => rules.rewriteBlocks(i.content))
    if (rewrittenItems.forall(_.isEmpty)) this
    else copy(content = rewrittenItems.zip(content) map {
      case (rewrittenItem, oldItem) => rewrittenItem.fold(oldItem)(c => oldItem.copy(content = c))
    })
  }
}

/** A single item in an option list. The content property serves as the description of the option.
  */
case class OptionListItem (programOptions: Seq[ProgramOption], content: Seq[Block], options: Options = NoOpt) extends ListItem
                                                                                                              with BlockContainer[OptionListItem] {
  def withContent (newContent: Seq[Block]): OptionListItem = copy(content = content)
}

/** A single option, including its name and all arguments, but not the description.
  */
case class ProgramOption (name: String, argument: scala.Option[OptionArgument]) extends Element

/** A single option argument.
  */
case class OptionArgument (value: String, delimiter: String) extends Element

/** A substitution definition with its span content that will be inserted
  *  wherever this substitution is referenced in flow content.
  */
case class SubstitutionDefinition (name: String, content: Span, options: Options = NoOpt) extends Definition

/** Refers to a substitution definition with the same name.
  *  This type of element will only temporarily be part of the document tree and replaced
  *  by the content of the substitution definition in a rewrite step.
  */
case class SubstitutionReference (name: String, options: Options = NoOpt) extends Reference {
  val source = s"|$name|"
}

/** Represents an interactive Python session. Somewhat unlikely to be used in
  *  the context of this library, but included for the sake of completeness.
  */
case class DoctestBlock (content: String, options: Options = NoOpt) extends Block with TextContainer

/** Header decoration consisting of both an overline and an underline.
  */
case class OverlineAndUnderline (char: Char) extends HeaderDecoration

/** Header decoration consisting of an underline only.
  */
case class Underline (char: Char) extends HeaderDecoration


/** Temporary element to represent interpreted text with its associated role name.
  *  In a post-processing step this text will be replaced by the result of calling
  *  the corresponding role function.
  */
case class InterpretedText (role: String, content: String, source: String, options: Options = NoOpt) extends Reference with TextContainer

/** Temporary element to represent a customized text role that can be applied
  *  to spans of interpreted text. The `apply` function can then be applied
  *  to spans of interpreted text referring to the name of this role and passing
  *  the text as the argument to the function.
  */
case class CustomizedTextRole (name: String, apply: String => Span, options: Options = NoOpt) extends Definition

/** Temporary element representing a file inclusion.
  *  The path is interpreted as relative to the path of the processed
  *  document if it is not an absolute path.
  */
case class Include (path: String, options: Options = NoOpt) extends Block with BlockResolver {
  def resolve (cursor: DocumentCursor): Block =
    cursor.parent.target.selectDocument(path) match {
      case Some(target) => BlockSequence(target.content.content)
      case None         => InvalidElement(s"Unresolvable path reference: $path", s".. include:: $path").asBlock
    }
}

/** Generates a table of contents element inside a topic.
  */
case class Contents (title: String, depth: Int = Int.MaxValue, local: Boolean = false, options: Options = NoOpt) extends Block with BlockResolver {
  def resolve (cursor: DocumentCursor): Block = {
    val toc = TocGenerator.fromDocument(cursor.target, depth, cursor.target.path) // TODO - find parent for local toc
    TitledBlock(List(Text(title)), toc, options + Styles("toc"))
  }
}


/** Represent a reference name.
  *  When resolving references whitespace needs to be normalized
  *  and the name converted to lower case.
  */
case class ReferenceName (original: String) {
  lazy val normalized: String = original.replaceAll("[\n ]+", " ").toLowerCase
}
