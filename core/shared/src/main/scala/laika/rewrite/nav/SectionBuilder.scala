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

package laika.rewrite.nav

import laika.ast._
import laika.collection.Stack
import laika.rewrite.nav

import scala.collection.mutable.ListBuffer

/** Rewrite rules responsible for building the section structure
 *  of a document based on the header elements it contains and
 *  their level.
 * 
 * @author Jens Halm
 */
object SectionBuilder extends (DocumentCursor => RewriteRules) {

  
  class DefaultRule (cursor: DocumentCursor) {

    val (errorBlock, autonumberConfig) = cursor.config.getOpt[nav.AutonumberConfig].fold(
      error => (Some(InvalidElement(error.message, "").asBlock), AutonumberConfig.defaults),
      opt   => (None, opt.getOrElse(AutonumberConfig.defaults))
    )
    
    def addNumber (spans: Seq[Span], position: TreePosition): Seq[Span] = position.toSpan +: spans
    
    class Builder (header: Header, id: String, val position: TreePosition) {
    
      val titleWithNumber: Seq[Span] = 
        if (autonumberConfig.sections && position.depth <= autonumberConfig.maxDepth) addNumber(header.content, position)
        else header.content
      
      val styledHeader: Header = header.copy(
        content = titleWithNumber,
        options = header.options + Style.section
      )
      
      private val buffer = new ListBuffer[Block]
      
      def += (block: Block): Unit = buffer += block
      
      def >= (level: Int): Boolean = header.level >= level
      
      def toSection: Section = Section(styledHeader, buffer.toList)
      
    }
    
    def buildSections (document: RootElement): RootElement = {

      val docPosition = if (autonumberConfig.documents) cursor.position else TreePosition.root
      
      val (titleSection, rest) = {
        
        val title = document.content.collectFirst {
          case h: Header =>
            if (autonumberConfig.documents) Title(addNumber(h.content, docPosition), h.options + Style.title)
            else Title(h.content, h.options + Style.title)
        }
        
        title.fold((document.content, Seq.empty[Block])) { titleBlock =>
          val (preface, rest) = document.content.splitAt(document.content.indexWhere(_.isInstanceOf[Header]))
          (preface :+ titleBlock, rest.tail)
        }
      } 
      
      val sectionStructure: Seq[Block] = {
        
        val stack = new Stack[Builder]
        stack.push(new Builder(Header(0,Nil), "", docPosition)) 
        
        def closeSections (toLevel: Int): Int = {
          var nextPos = 1
          while (stack.nonEmpty && stack.top >= toLevel) {
            val section = stack.pop
            nextPos = section.position.toSeq.last + 1
            stack.top += section.toSection
          }
          nextPos
        }

        rest.foreach { 
          case h @ Header(level, _, Id(id)) => 
            val nextPos = closeSections(level)
            stack.push(new Builder(h, id, stack.top.position.forChild(nextPos)))
          case block => 
            stack.top += block
        }
    
        closeSections(1)
        
        stack.pop.toSection.content
      }
      
      RootElement(errorBlock.toSeq ++ titleSection ++ sectionStructure)
    }

    val rewrite: RewriteRules = RewriteRules.forBlocks { 
      case root: RootElement => Replace(buildSections(root)) 
    }
  }
  
  /** Provides the default rewrite rules for building the section structure
   *  for the specified document (without applying them).
   */
  def apply (cursor: DocumentCursor): RewriteRules = new DefaultRule(cursor).rewrite
  
}
