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
import scala.collection.mutable.Stack
import laika.tree.Elements._
import laika.tree.Documents.Document

/** Rewrite rules responsible for building the section structure
 *  of a document based on the header elements it contains and
 *  their level.
 * 
 * @author Jens Halm
 */
object SectionBuilder extends (() => PartialFunction[Element,Option[Element]]) {

  
  class DefaultRule { 
    
    class Builder (header:Header) {
    
      private val buffer = new ListBuffer[Block]
      
      def += (block: Block) = buffer += block
      
      def >= (level: Int) = header.level >= level
      
      def toSection = Section(header, buffer.toList)
      
    }
    
    def buildSections (document: RootElement) = {
      val stack = new Stack[Builder]
      stack push new Builder(Header(0,Nil)) 
      
      def closeSections (toLevel: Int) = {
        while (!stack.isEmpty && stack.top >= toLevel) {
          val section = stack.pop.toSection
          stack.top += section
        }
      }
      
      document.content.foreach { 
        case h @ Header(level, _, _) => closeSections(level); stack push new Builder(h)
        case block                   => stack.top += block
      }
  
      closeSections(1)
      RootElement(stack.pop.toSection.content)
    }

    val rewrite: PartialFunction[Element, Option[Element]] = { 
      case root: RootElement => Some(buildSections(root)) 
    }
  }
  
  /** Provides the default rewrite rules for building the section structure
   *  for the specified document (without applying them).
   */
  def apply () = (new DefaultRule).rewrite
  
}