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

package laika.rewrite.link

import laika.ast.Path

/** Represents a selector used for matching reference
  *  nodes to target nodes. The selectors often differ
  *  from both, the ids rendered in the final document
  *  and the ids used for display.
  */
sealed trait Selector {

  /** Indicates whether this selector is applicable
    * beyond the boundaries of a single document.
    */
  def global: Boolean

  /** Indicates whether this selector has to be unique
    * within its scope.
    *
    * When the global flag is set it must be globally
    * unique, otherwise it must only be unique within
    * the document it occurs in.
    */
  def unique: Boolean

}

/** A selector that can be used for a sequence of targets.
  */
sealed trait SequenceSelector extends Selector {
  val global = false
  val unique = false
}

/** A selector that can is a globally unique identifier.
  */
sealed trait UniqueSelector extends Selector {
  val global = true
  val unique = true
  def description: String
}

/** A selector for a rendered target in a document.
  */
case class TargetIdSelector (id: String) extends UniqueSelector {
  val description = s"link target with id '$id'"
}

/** A selector for a definition for an internal or external link.
  */
case class LinkDefinitionSelector (id: String) extends UniqueSelector {
  val description = s"link definition with id '$id'"
}

/** A selector based on a path, optionally including a fragment component.
  */
case class PathSelector (path: Path) extends UniqueSelector {
  val description = s"link target with path '$path'"
}

/** An anonymous selector (usually matched by position).
  */
case object AnonymousSelector extends SequenceSelector

/** An auto-number selector (usually matched by position).
  */
case object AutonumberSelector extends SequenceSelector

/** An auto-symbol selector (usually matched by position).
  */
case object AutosymbolSelector extends SequenceSelector
