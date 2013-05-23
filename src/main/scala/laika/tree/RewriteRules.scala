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

/** The default rewrite rules that get applied to the raw document tree after parsing
 *  unless explicitly disabled. The rules are responsible for resolving link and image
 *  references and building a structure of sections based on the headers in the document
 *  and their level.
 *  
 *  To disable these rules the `Parse` API needs to be used (the `Transform` API always
 *  applies them):
 *  
 *  {{{
 *  val doc = Parse as Markdown asRawDocument fromFile "hello.md"
 *  }}}
 * 
 *  @author Jens Halm
 */
object RewriteRules {

  
  /** Function providing the default rewrite rules when passed a document instance.
   */
  val defaults: Document => PartialFunction[Element, Option[Element]] = { document =>
    val linkTargetResolver = (new LinkTargetResolver).rewriteRule
    linkTargetResolver(document) orElse sectionBuilder(document)
  }
  
  val sectionBuilder: Document => PartialFunction[Element, Option[Element]] = { document =>
    val pf: PartialFunction[Element, Option[Element]] = { 
      case doc: Document        => Some(buildSections(doc)) 
    }
    pf
  }

  /** Applies the default rewrite rules to the specified document tree,
   *  returning a new rewritten tree instance.
   */
  def applyDefaults (document: Document) = {
    document rewrite defaults(document) 
  }
  
   
  private def buildSections (doc: Document) = {
    
    val stack = new Stack[SectionBuilder]
    stack push new SectionBuilder(Header(0,Nil)) 
    
    def closeSections (toLevel: Int) = {
      while (!stack.isEmpty && stack.top >= toLevel) {
        val section = stack.pop.toSection
        stack.top += section
      }
    }
    
    doc.content.foreach { 
      case h @ Header(level, _, _) => closeSections(level); stack push new SectionBuilder(h)
      case block                   => stack.top += block
    }

    closeSections(1)
    Document(stack.pop.toSection.content)
  }
  
  
  private class SectionBuilder (header:Header) {
    
    private val buffer = new ListBuffer[Block]
    
    def += (block: Block) = buffer += block
    
    def >= (level: Int) = header.level >= level
    
    def toSection = Section(header, buffer.toList)
    
  }
  
  
  
}