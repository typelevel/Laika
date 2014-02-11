/*
 * Copyright 2014 the original author or authors.
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

package laika.parse.css

import laika.tree.Elements.Element
import laika.tree.Elements.Customizable
import laika.tree.Elements.Id

/**
 * @author Jens Halm
 */
object Styles {

  
  case class ParentSelector (selector: Selector, immediate: Boolean)
  
  case class Specificity (ids: Int, classes: Int, types: Int, order: Int) extends Ordered[Specificity] {
    def + (other: Specificity) = Specificity(ids + other.ids, classes + other.classes, types + other.types, order)
    def compare (other: Specificity) = 
      if (ids != other.ids) ids compare other.ids
      else if (classes != other.classes) classes compare other.classes
      else if (types != other.types) types compare other.types
      else order compare other.order
  }
  
  
  case class Selector (
      elementType: Option[Class[_ <: Element]] = None, 
      id: Option[String] = None, 
      classes: Set[String] = Set(), 
      parent: Option[ParentSelector] = None,
      order: Int) {
    
    private def satisfies [T](opt: Option[T])(pred: T => Boolean) = opt.fold(true)(pred)
    
    def specificity: Specificity = {
      val thisSpec = Specificity(id.fold(0)(_=>1), classes.size, elementType.fold(0)(_=>1), order)
      parent.fold(thisSpec)(thisSpec + _.selector.specificity)
    }
    
    def matches (target: Element, parents: Seq[Element]): Boolean = {
      def matchesParent (parents: Seq[Element], selector: Selector): Boolean = parents match {
        case Seq() => false
        case seq => selector.matches(seq.head, seq.tail) || matchesParent(seq.tail, selector)
      }
      
      satisfies(elementType)(_ == target.getClass) &&
      satisfies(id){ id => target match {
        case c: Customizable if c.options.id == Some(id) => true
        case _ => false
      }} &&
      (target match {
        case c: Customizable if classes.subsetOf(c.options.styles) => true
        case _ => false
      }) &&
      satisfies(parent){ parent =>
        if (parent.immediate) parents.headOption.fold(false)(parent.selector.matches(_, parents.tail))
        else matchesParent(parents, parent.selector)
      }
    }
    
  }
  
  
  case class Style (name: String, value: String, inherited: Boolean)
  
  case class StyleDeclaration (selector: Selector, styles: Map[String, Style]) {
    
    def appliesTo (element: Element, parents: Seq[Element]) = selector.matches(element, parents)
    
  }
  
  class StyleDeclarationSet (decl: Set[StyleDeclaration]) {
    
    def collectStyles (element: Element, parents: Seq[Element], parentStyles: Map[String, Style]) = {
      val styles = decl.filter(_.appliesTo(element, parents)).toSeq.sortBy(_.selector.specificity)
      (parentStyles /: styles) { 
        (acc, decl) => acc ++ decl.styles
      } 
    }
      
    
  }
  
 
}