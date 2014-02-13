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
import scala.language.existentials

/**
 * @author Jens Halm
 */
object Styles {

  
  sealed abstract class Predicate {
    def specificity: Specificity
    def evaluate (element: Element): Boolean
  }
  
  case class ElementType (name: String) extends Predicate {
    val specificity = Specificity(0,0,1,0)
    def evaluate (element: Element) = element.getClass.getSimpleName == name
  }
  
  case class Id (id: String) extends Predicate {
    val specificity = Specificity(1,0,0,0)
    def evaluate (element: Element) = element match {
      case c: Customizable if c.options.id == Some(id) => true
      case _ => false
    }
  }  
  
  case class StyleName (name: String) extends Predicate {
    val specificity = Specificity(0,1,0,0)
    def evaluate (element: Element) = element match {
      case c: Customizable if c.options.styles.contains(name) => true
      case _ => false
    }
  }  
  
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
      predicates: Set[Predicate] = Set(), 
      parent: Option[ParentSelector] = None,
      order: Int = Int.MinValue) {
    
    def specificity: Specificity = {
      val thisSpec = predicates.map(_.specificity).reduceLeftOption(_+_).getOrElse(Specificity(0,0,0,0)).copy(order = order)
      parent.fold(thisSpec)(thisSpec + _.selector.specificity)
    }
    
    def matches (target: Element, parents: Seq[Element]): Boolean = {
      def matchesParent (parents: Seq[Element], selector: Selector): Boolean = parents match {
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
  
  
  case class StyleDeclaration (selector: Selector, styles: Map[String, String]) {
    
    def appliesTo (element: Element, parents: Seq[Element]) = selector.matches(element, parents)
    
  }
  
  class StyleDeclarationSet (decl: Set[StyleDeclaration]) {
    
    def collectStyles (element: Element, parents: Seq[Element]) = {
      val decls = decl.filter(_.appliesTo(element, parents)).toSeq.sortBy(_.selector.specificity)
      (Map[String,String]() /: decls) { case (acc, decl) => acc ++ decl.styles }
    }
    
  }
  
 
}