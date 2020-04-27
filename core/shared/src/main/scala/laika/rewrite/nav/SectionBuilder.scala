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

import scala.collection.mutable.ListBuffer

/** Rewrite rules responsible for building the section structure
 *  of a document based on the header elements it contains and
 *  their level.
 * 
 * @author Jens Halm
 */
object SectionBuilder extends (DocumentCursor => RewriteRules) {

  
  class DefaultRule (cursor: DocumentCursor) { 
    
    class Builder (header:Header, id: String) {
    
      val styledHeader: Header = header.copy(options = header.options + Style.section)
      
      private val buffer = new ListBuffer[Block]
      
      def += (block: Block): Unit = buffer += block
      
      def >= (level: Int): Boolean = header.level >= level
      
      def toSection: Section = Section(styledHeader, buffer.toList)
      
    }
    
    def buildSections (document: RootElement): RootElement = {

      val (errorBlock, autonumberConfig) = AutonumberConfig.fromConfig(cursor.config).fold(
        error => (Some(InvalidElement(error.message, "").asBlock), AutonumberConfig.defaults),
        (None, _)
      )

      def addNumber (spans: Seq[Span], position: TreePosition): Seq[Span] = position.toSpan +: spans

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
        stack.push(new Builder(Header(0,Nil), "")) 
        
        def closeSections (toLevel: Int): Unit = {
          while (stack.nonEmpty && stack.top >= toLevel) {
            val section = stack.pop.toSection
            stack.top += section
          }
        }

        rest.foreach { 
          case h @ Header(level, _, Id(id)) => closeSections(level); stack.push(new Builder(h, id))
          case block                        => stack.top += block
        }
    
        closeSections(1)
        
        stack.pop.toSection.content
      }
      
      val numberedSections: Seq[Block] = {
        
        def numberSection (s: Section, position: TreePosition): Section = s.copy(
          header = s.header.copy(
              content = addNumber(s.header.content, position), 
              options = s.header.options + Style.section
          ),
          content = numberSections(s.content, position)
        )  
        
        def numberSections (blocks: Seq[Block], parentPosition: TreePosition): Seq[Block] = {
          blocks.foldLeft((ListBuffer[Block](), 1)) {
            case ((acc, num), s: Section) =>
              val element =
                if (parentPosition.depth < autonumberConfig.maxDepth) numberSection(s, parentPosition.forChild(num))
                else s
              (acc += element, num + 1)
            case ((acc, num), block) => (acc += block, num)
          }._1.toList
        }
        
        if (autonumberConfig.sections) numberSections(sectionStructure, docPosition)
        else sectionStructure
      }
      
      RootElement(errorBlock.toSeq ++ titleSection ++ numberedSections)
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
