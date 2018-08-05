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

package laika.ast


/** Represents a single predicate which is
  * part of the selector for a style declaration.
  */
sealed trait StylePredicate {

  /** The specificity of this predicate.
    *  Used to calculate the precedence if
    *  multiple selectors apply to the same
    *  target element.
    */
  def specificity: Specificity

  /** Indicates whether this predicate holds
    *  for the specified element.
    */
  def evaluate (element: Element): Boolean
}

/** Contains the available predicate types.
  *
  * The available selector types represent a subset of the full
  * CSS standard, picking those features which are reasonably suited
  * in the context of the Laika document model. It includes type,
  * class and id selectors, but no attribute selectors as most
  * Laika tree elements primarily contain a sequence of children
  * and not many other properties to match against.
  *
  * It also supports the CSS syntax to declare selectors for
  * child elements (e.g. `Paragraph .intro`) or immediate
  * children (e.g. `Paragraph > .intro`).
  */
object StylePredicate {

  /** A predicate that holds if the given type name matches the type
    *  of the target element. In contrast to web CSS, in Laika
    *  this is not a tag name, but the (unqualified) name
    *  of the class of the target element instance (e.g. `Paragraph`).
    */
  case class ElementType (name: String) extends StylePredicate {
    val specificity = Specificity(0,0,1,0)
    def evaluate (element: Element): Boolean = element.getClass.getSimpleName == name
  }

  /** A predicate that holds if the given id matches
    *  the id of the target element.
    */
  case class Id (id: String) extends StylePredicate {
    val specificity = Specificity(1,0,0,0)
    def evaluate (element: Element): Boolean = element match {
      case c: Customizable if c.options.id == Some(id) => true
      case _ => false
    }
  }

  /** A predicate that holds if the given style name matches
    *  one of the styles of the target element.
    */
  case class StyleName (name: String) extends StylePredicate {
    val specificity = Specificity(0,1,0,0)
    def evaluate (element: Element): Boolean = element match {
      case c: Customizable if c.options.styles.contains(name) => true
      case _ => false
    }
  }
}

/** Represents a parent selector and indicates whether it is
  *  an immediate parent or an ancestor anywhere in the hierarchy.
  */
case class ParentSelector (selector: StyleSelector, immediate: Boolean)

/** Represents the specificity of a style selector or predicate.
  * This is modeled after the precedence rules of the CSS standard.
  */
case class Specificity (ids: Int, classes: Int, types: Int, order: Int) extends Ordered[Specificity] {

  /** Adds this and the given specificity by summing
    *  all its individual parameters.
    */
  def + (other: Specificity): Specificity =
    Specificity(ids + other.ids, classes + other.classes, types + other.types, order)

  /** Compares this instance with the given specificity.
    *  Applies the CSS standard weighting of first comparing the
    *  number of specified ids, followed by classes, then types
    *  and finally the order.
    */
  def compare (other: Specificity): Int =
    if (ids != other.ids) ids compare other.ids
    else if (classes != other.classes) classes compare other.classes
    else if (types != other.types) types compare other.types
    else order compare other.order
}


/** Responsible for determining whether a style declaration
  *  should be applied to a target element, basing its decision
  *  on a set of predicates.
  *
  *  @param predicates the set of predicates that need to hold
  *  for this selector to be applicable to a target element
  *  @param parent the optional parent selector
  *  @param order the index of the style declaration this selector
  *  belongs to
  */
case class StyleSelector(predicates: Set[StylePredicate] = Set(),
                         parent: Option[ParentSelector] = None,
                         order: Int = 0) {

  /** The specificity of this selector, calculated
    *  from the specificity of its predicates and the order.
    *
    *  Used to calculate the precedence if multiple selectors
    *  apply to the same target element.
    */
  def specificity: Specificity = {
    val thisSpec = predicates.map(_.specificity).reduceLeftOption(_+_).getOrElse(Specificity(0,0,0,0)).copy(order = order)
    parent.fold(thisSpec)(thisSpec + _.selector.specificity)
  }

  /** Indicates whether this selector applies to the specified target
    *  element.
    *
    *  @param target the target element to apply this selector to
    *  @param parents the parents of the specified target element, which
    *  also need to match in case this selector has parent selectors.
    *  @return true if this selector matches the target element
    */
  def matches (target: Element, parents: Seq[Element]): Boolean = {
    def matchesParent (parents: Seq[Element], selector: StyleSelector): Boolean = parents match {
      case Seq() => false
      case seq => selector.matches(seq.head, seq.tail) || matchesParent(seq.tail, selector)
    }

    predicates.forall(_.evaluate(target)) &&
      parent.fold(true) { parent =>
        if (parent.immediate) parents.headOption.fold(false)(parent.selector.matches(_, parents.tail))
        else matchesParent(parents, parent.selector)
      }
  }

}


/** Represents a single style declaration.
  *
  *  @param selector the selector to determine which elements this declaration applies to
  *  @param styles the styles themselves in a map representing the names and values of each style
  */
case class StyleDeclaration (selector: StyleSelector, styles: Map[String, String]) {

  /** Indicates whether this style declaration applies to the specified target
    *  element.
    *
    *  @param element the target element to apply the selector of this style declaration to
    *  @param parents the parents of the specified target element, which
    *  also need to match in case the selector of this declaration has parent selectors.
    *  @return true if this style declaration applies to the target element
    */
  def appliesTo (element: Element, parents: Seq[Element]): Boolean = selector.matches(element, parents)

  /** Returns a new style declaration with the order parameter
    *  in its Specificity property increased by the specified amount.
    */
  def increaseOrderBy (amount: Int): StyleDeclaration =
    if (amount == 0) this
    else StyleDeclaration(selector.copy(order = selector.order + amount), styles)

}

/** Companion providing factory methods for the StyleDeclaration class.
  */
object StyleDeclaration extends ((StyleSelector, Map[String, String]) => StyleDeclaration) {

  /** Creates a new StyleDeclaration with only one predicate.
    */
  def apply(predicate: StylePredicate, styles: (String, String)*): StyleDeclaration = apply(StyleSelector(Set(predicate)), styles.toMap)
}

/** Represents a set of one or more style declarations.
  *
  *  @param paths the paths the style declarations have been obtained from
  *  @param styles the style declarations that belong to this set
  */
case class StyleDeclarationSet (paths: Set[Path], styles: Set[StyleDeclaration]) {

  /** Collects all the styles that apply to the specified target element.
    *
    *  @param target the target element to collect the matching styles for
    *  @param parents the parents of the specified target element
    *  @return a map representing the keys and values of all styles that apply to the target element
    */
  def collectStyles (target: Element, parents: Seq[Element]): Map[String, String] = {
    val decls = styles.filter(_.appliesTo(target, parents)).toSeq.sortBy(_.selector.specificity)
    (Map[String,String]() /: decls) { case (acc, decl) => acc ++ decl.styles }
  }

  /** Merges the style declaration of this instance with the specified set
    *  and returns the merged set in a new instance.
    */
  def ++ (set: StyleDeclarationSet): StyleDeclarationSet = {
    val maxOrder = if (styles.isEmpty) 0 else styles.maxBy(_.selector.order).selector.order + 1
    new StyleDeclarationSet(paths ++ set.paths, styles ++ (set.styles map (_.increaseOrderBy(maxOrder))))
  }

}

/** Companion providing factory methods for the StyleDeclaration class.
  */
object StyleDeclarationSet extends ((Set[Path], Set[StyleDeclaration]) => StyleDeclarationSet) {

  /** Creates an empty StyleDeclarationSet with `Root` as its only path element.
    */
  val empty: StyleDeclarationSet = new StyleDeclarationSet(Set(Path.Root), Set.empty)

  /** Creates a new StyleDeclarationSet with a single path element and
    *  the specified declarations.
    */
  def apply(path: Path, styles: StyleDeclaration*): StyleDeclarationSet = apply(Set(path), styles.toSet)

  /** Creates a new StyleDeclarationSet with a single path element and
    *  the specified declarations.
    */
  def forPath(path: Path, styles: Set[StyleDeclaration]) = apply(Set(path), styles)
}