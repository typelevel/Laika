/*
 * Copyright 2013-2016 the original author or authors.
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

package laika.rewrite

import laika.ast.Path

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/** Generators for various different types of automatically generated identifiers.
 * 
 *  @author Jens Halm
 */
object IdGenerators {

  /** Represents an identifier for an element in the document tree.
   */
  sealed abstract class Id
  
  /** Represents a positional anonymous id.
   *  References get matched to the targets in the order of
   *  appearance in the document tree. Therefore the number 
   *  of references with this id type must match the number
   *  of targets. 
   */
  case class Anonymous (pos: Int) extends Id
  
  /** Represents an id with a unique string identifier.
   *  If multiple elements use the same identifier
   *  they get marked as invalid and their ids get removed.
   */
  case class Named (name: String) extends Id
  
  /** Represents an id that captures a relationship
   *  between two paths. Useful for representing cross-references
   *  between two documents.
   */
  case class Relative (path: Path, name: String) extends Id
  
  /** Represents a generated id based on the provided
   *  generator function. The Set passed to the function
   *  contains the ids that are already in use (independent
   *  of whether they were generated or not).
   */
  case class Generated (generator: Set[String] => String) extends Id

  /** Represents a hybrid id, with one identifier used as a selector
   *  for finding matching reference elements and the other merely used
   *  for rendering.  
   */
  case class Hybrid (id: String, display: Id) extends Id
  
  /** Represents an id that is not getting used in the final
   *  rendering step.
   */
  case object Hidden extends Id

  /** Converts the specified string to an Id instance
   *  that represents a unique identifier.
   */
  implicit def stringToId (name: String): Id = Named(name)
  
  
  /** Generates symbol identifiers. 
   *  Contains a predefined list of ten symbols to generate.
   *  If more than ten symbols are required, the same sequence 
   *  will be reused, doubled and then tripled, and so on ("**" etc.).
   */
  class SymbolGenerator extends (Set[String] => String) {
    private val symbols = List('*','\u2020','\u2021','\u00a7','\u00b6','#','\u2660','\u2665','\u2666','\u2663')
    private val stream = Stream.iterate((symbols,1)){ case (sym,num) => if (sym.isEmpty) (symbols,num+1) else (sym.tail,num) }.iterator
    @tailrec final def apply (used: Set[String]): String = {
      val (sym,num) = stream.next
      val candidate = sym.head.toString * num
      if (!used.contains(candidate)) candidate
      else apply(used)
    }
  } 
    
  /** Generates ascending sequences of numeric identifiers,
   *  starting with 1. It skips all numbers which are contained
   *  in the Set of used identifiers passed to the function.
   */
  class NumberGenerator extends (Set[String] => String) {
    private val numbers = Stream.from(1).iterator
    @tailrec final def apply (used: Set[String]): String = {
      val candidate = numbers.next.toString
      if (!used.contains(candidate)) candidate 
      else apply(used)
    }
  }
  
  /** Generates an identifier based on a concrete suggestion.
   *  If the suggested id is not already contained in the 
   *  Set of used identifiers passed to the function it will
   *  be used as is. Otherwise it will try to append numbers
   *  (-1, -2, and so on) until it finds an unused id.
   */
  def suggestedId (suggested: String, map: IdMap) = Generated( used => {
    def result (str: String): String = {
      map += (suggested, str)
      str
    }
    @tailrec def next (num: Int): String = {
      val candidate = s"$suggested-$num"
      if (!used.contains(candidate)) result(candidate)
      else next(num + 1)
    }
    if (!used.contains(suggested)) result(suggested) else next(1)
  })
  
  /** Maps original ids to their final generated ids.
   *  The same original id may trigger the generation of multiple
   *  final ids if used in more than one place. The lookup function
   *  returns them one after the other.
   */
  class IdMap {
    private val map = scala.collection.mutable.Map[String,ListBuffer[String]]()
    def += (origId: String, docId: String): Unit = {
      val list = map.getOrElseUpdate(origId, ListBuffer())
      list += docId
    }
    def lookupFunction: String => Option[String] = {
      val lookup = map.mapValues(_.iterator).view.force
      s => lookup.get(s).flatMap(i => if (i.hasNext) Some(i.next) else None)
    }
  }
  
  
}
