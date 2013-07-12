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

package laika.tree

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import laika.tree.Elements._
 
/** Provides means to traverse, select and rewrite elements of a document tree. 
 *  This trait can get mixed in by any element node, not just the root Document class,
 *  therefore providing traversal for any kind of sub-tree, too.
 * 
 *  @author Jens Halm
 */
trait ElementTraversal [Self <: Element with ElementTraversal[Self]] { self: Element => 
  
   
  private case object Unmodified extends Element
  private val Retain = Some(Unmodified)
  private val fallbackRule: PartialFunction[Element,Option[Element]] = { case e => Some(e) }
  
  /** Returns a new, rewritten tree model based on the specified rule.
   *  The rule is a partial function that takes an `Element` and returns an `Option[Element]`.
   *  
   *  If the function is not defined for a specific element the old element remains
   *  in the tree unchanged. If it returns `None` then the node gets removed from the tree, 
   *  if it returns an element it will replace the old one. Of course the function may
   *  also return the old element.
   *  
   *  The rewriting is performed in a way that only branches of the tree that contain
   *  new or removed elements will be replaced. It is processed bottom-up, therefore
   *  any element container passed to the rule only contains children which have already
   *  been processed. 
   *  
   *  In case multiple rewrite rules need to be applied it may be more efficient to
   *  first combine them with `orElse`.
   */
  def rewrite (rule: PartialFunction[Element,Option[Element]]): Self = {
    val result = rewriteProperties(rule)
    
    (rule orElse fallbackRule)(result) match {
      case Some(e) => e.asInstanceOf[Self]
      case _ => result.asInstanceOf[Self]
    }
  }
  
  private def rewriteProperties (rule: PartialFunction[Element,Option[Element]]): Self = {

    lazy val rewriteOldElement: Element => Option[Element] = rule orElse { case _ => Retain }
    lazy val rewriteNewElement: Element => Option[Element] = rule orElse fallbackRule
    
    def rewriteElement (e: Any): Option[AnyRef] = {
      
      def optimize (rewritten: Option[AnyRef], original: AnyRef) = rewritten match {
        case Some(result) if result eq original => Retain
        case e => e
      }
      
      e match {
        case et: ElementTraversal[_] => {
          val newET = et.rewriteProperties(rule)
          val f = if (newET eq et) rewriteOldElement else rewriteNewElement
          val finalET = f(newET.asInstanceOf[Element])
          optimize(finalET, et)
        }
        case e: Element         => optimize(rewriteOldElement(e), e)
        case t: Traversable[_]  => optimize(rewriteChildren(t.asInstanceOf[Traversable[AnyRef]]), t)
        case x                  => Retain
      }
    }
    
    def rewriteChildren (children: Traversable[AnyRef]) = {
      var i = 0
      var changed = false
      lazy val newChildren = ListBuffer[AnyRef]() 
  
      children.foreach { oldElem =>
        rewriteElement(oldElem) match {
          case Retain                   => if (changed) newChildren += oldElem
          case Some(result) if changed  => newChildren += result
          case Some(result)             => changed = true; newChildren ++= children.take(i) += result 
          case None         if !changed => changed = true; newChildren ++= children.take(i)
          case _                        => ()
        }
        
        i += 1
      }
      
      if (!changed) Retain
      else Some(newChildren.toList)
    }
    
    var changed = false
    var i = 0
    lazy val newElements = new Array[Any](productArity)

    productIterator.foreach { elem =>
      val oldElem = elem.asInstanceOf[AnyRef]
      
      rewriteElement(oldElem) match {
        case Retain                   => if (changed) newElements(i) = oldElem
        case Some(result) if changed  => newElements(i) = result
        case None         if changed  => newElements(i) = oldElem // removal only possible for elements of Traversables
        case Some(result)             => changed = true
                                         if (i>0) for (x <- 0 to (i-1)) newElements(x) = productElement(x) 
                                         newElements(i) = result 
        case None                     => ()
      }
      
      i += 1
    }
    
    if (!changed) this.asInstanceOf[Self] else copyWith(newElements)
  }
  
  private def copyWith (newElements: Array[Any]): Self = {
    val ctor = getClass.getConstructors()(0)
    try {
      ctor.newInstance(newElements.asInstanceOf[Array[AnyRef]]:_*).asInstanceOf[Self]
    } catch {
      case e : IllegalArgumentException => throw RewriteException(self, e)
    }
  }
  
  
  /** Invokes the specified function for each child of this element
   *  container, including children of children, and this element itself,
   *  in depth-first traversal.
   */
  def foreach (f: Element => Unit): Unit = { 
    
    def foreachInElement (element: Element, f: Element => Unit): Unit = {
      foreachInTraversable(element.productIterator, f) 
      f(element)
    }
    
    def foreachInTraversable (t: TraversableOnce[Any], f: Element => Unit): Unit = {
      t.foreach { 
        case e:  Element          => foreachInElement(e, f)
        case t:  Traversable[_]   => foreachInTraversable(t, f) 
        case _                    => ()
      }
    }
    
    foreachInElement(this, f)
  }
  
  /** Selects all elements satisfying the specified predicate, collecting
   *  in depth-first traversal, including this element itself. 
   */
  def select (p: Element => Boolean): List[Element] = {
    val buffer = new ListBuffer[Element]
    foreach { e =>
      if (p(e)) buffer += e
    }
    buffer.toList
  }
  
  /** Selects all elements satisfying the specified predicate, collecting
   *  in depth-first traversal, including this element itself. 
   */
  def collect [B](pf: PartialFunction[Element, B]): List[B] = {
    val buffer = new ListBuffer[B]
    foreach { e =>
      if (pf.isDefinedAt(e)) buffer += pf(e)
    } 
    buffer.result
  }
  
  
  /** Exception thrown when a rewrite rule replaced an element with a new element
   *  that does not comply with the contract of its parent container.
   *  
   *  The `element` property holds the offending element.
   */
  class RewriteException (val element: Element, message: String, cause: Throwable) extends RuntimeException(message, cause)
  
  object RewriteException {
    
    def apply (element: Element, cause: Throwable) = {
      val message = "Unable to rewrite element '" + ElementTraversal.this +
          "': one of its new children does not conform to the contract of the element"
      new RewriteException(element, message, cause)
    }
    
  }
  
}

