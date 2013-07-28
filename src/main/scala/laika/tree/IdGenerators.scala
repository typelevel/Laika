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

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
 *  @author Jens Halm
 */
object IdGenerators {

  
  sealed abstract class Id
  
  case class Anonymous (pos: Int) extends Id
  case class Named (name: String) extends Id
  case class Generated (generator: Set[String] => String) extends Id
  case class Hybrid (id: String, display: Id) extends Id
  case object Hidden extends Id
  
  implicit def stringToId (name: String) = Named(name)
  
  
  class SymbolGenerator extends (Set[String] => String) {
    private val symbols = List('*','\u2020','\u2021','\u00a7','\u00b6','#','\u2660','\u2665','\u2666','\u2663')
    private val stream = Stream.iterate((symbols,1)){ case (sym,num) => if (sym.isEmpty) (symbols,num+1) else (sym.tail,num) }.iterator
    @tailrec final def apply (used: Set[String]) = {
      val (sym,num) = stream.next
      val candidate = sym.head.toString * num
      if (!used.contains(candidate)) candidate
      else apply(used)
    }
  } 
    
  class NumberGenerator extends (Set[String] => String) {
    private val numbers = Stream.from(1).iterator
    @tailrec final def apply (used: Set[String]) = {
      val candidate = numbers.next.toString
      if (!used.contains(candidate)) candidate 
      else apply(used)
    }
  }
  
  def suggestedId (suggested: String, map: IdMap) = Generated( used => {
    def result (str: String) = {
      map += (suggested, str)
      str
    }
    @tailrec def next (num: Int): String = {
      val candidate = suggested+"-"+num
      if (!used.contains(candidate)) result(candidate)
      else next(num + 1)
    }
    if (!used.contains(suggested)) result(suggested) else next(1)
  })
  
  class IdMap {
    private val map = scala.collection.mutable.Map[String,ListBuffer[String]]()
    def += (origId: String, docId: String)= {
      val list = map.getOrElseUpdate(origId, ListBuffer())
      list += docId
    }
    def lookupFunction: String => Option[String] = {
      val lookup = map.mapValues(_.iterator).view.force
      s => lookup.get(s).flatMap(i => if (i.hasNext) Some(i.next) else None)
    }
  }
  
  
}