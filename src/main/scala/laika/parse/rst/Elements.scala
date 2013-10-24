/*
 * Copyright 2013 the original author or authors.
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

package laika.parse.rst

import laika.tree.Elements._
import laika.tree.Templates.PlaceholderBlock
import laika.tree.Documents.DocumentContext

/** Provides the elements of the document tree that are too specific to reStructuredText
 *  to be added to the generic tree model. 
 *  
 *  @author Jens Halm
 */
object Elements {

  
  /** A two-column table-like structure used for bibliographic fields or directive options.
   */
  case class FieldList (content: Seq[Field], options: Options = NoOpt) extends Block with ListContainer[FieldList]

  /** A single entry in a field list consisting of name and body.
   */
  case class Field (name: Seq[Span], content: Seq[Block], options: Options = NoOpt) extends ListItem with BlockContainer[Field]
  
  /** A classifier for a term in a definition list.
   */
  case class Classifier (content: Seq[Span], options: Options = NoOpt) extends Span with SpanContainer[Classifier]
  
  /** A list of command line options and descriptions.
   */
  case class OptionList (content: Seq[OptionListItem], options: Options = NoOpt) extends Block with ListContainer[OptionList]
  
  /** A single item in an option list. The content property serves as the description of the option.
   */
  case class OptionListItem (programOptions: Seq[ProgramOption], content: Seq[Block], options: Options = NoOpt) extends ListItem 
                                                                                                         with BlockContainer[OptionListItem]
  
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
    val source = "|"+name+"|"
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
  case class Include (path: String, options: Options = NoOpt) extends Block with PlaceholderBlock {
    def resolve (context: DocumentContext): Block = 
      context.parent.selectDocument(path) match {
        case Some(target) => BlockSequence(target.content.content)
        case None => InvalidBlock(SystemMessage(Error, "Unresolvable path reference: " + path), 
            LiteralBlock(".. include:: " + path))
      }
  }
  
  
}