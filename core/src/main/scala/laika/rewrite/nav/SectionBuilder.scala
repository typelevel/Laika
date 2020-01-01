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
    
      val styledHeader: Header = header.copy(options = header.options + Styles("section"))
      
      private val buffer = new ListBuffer[Block]
      
      def += (block: Block): Unit = buffer += block
      
      def >= (level: Int): Boolean = header.level >= level
      
      def toSection: Section = Section(styledHeader, buffer.toList)
      
    }
    
    def buildSections (document: RootElement): RootElement = {
      
      val sectionStructure: Seq[Block] = {
        
        val stack = new Stack[Builder]
        stack push new Builder(Header(0,Nil), "") 
        
        def closeSections (toLevel: Int): Unit = {
          while (stack.nonEmpty && stack.top >= toLevel) {
            val section = stack.pop.toSection
            stack.top += section
          }
        }
        
        document.content.foreach { 
          case h @ Header(level, _, Id(id)) => closeSections(level); stack push new Builder(h, id)
          case block                        => stack.top += block
        }
    
        closeSections(1)
        
        stack.pop.toSection.content
      }
      
      val numberedSections: Seq[Block] = {
        
        val (errorBlock, autonumberConfig) = AutonumberConfig.fromConfig(cursor.config).fold(
          error => (Some(InvalidElement(error.message, "").asBlock), AutonumberConfig.defaults),
          (None, _)
        )

        val hasTitle = sectionStructure collect { case s:Section => s } match {
          case _ :: Nil => true
          case _ => false
        }
        val docPosition = 
          if (autonumberConfig.documents) cursor.position
          else TreePosition.root
          
        def transformRootBlocks (blocks: Seq[Block]): Seq[Block] =
          blocks.foldLeft(ListBuffer[Block]()) {
            case (acc, s: Section) => acc ++= transformRootSection(s)
            case (acc, block) => acc += block
          }.toList
        
        def transformRootSection (s: Section): Seq[Block] = {
          val options = SomeOpt(s.header.options.id, s.header.options.styles - "section" + "title")
          val title = if (autonumberConfig.documents) Title(addNumber(s.header.content, docPosition), options) 
                      else Title(s.header.content, options)
          val content = if (autonumberConfig.sections) numberSections(s.content, docPosition) else s.content
          title +: content
        }
        
        def addNumber (spans: Seq[Span], position: TreePosition): Seq[Span] = position.toSpan +: spans 
          
        def numberSection (s: Section, position: TreePosition): Section = s.copy(
          header = s.header.copy(
              content = addNumber(s.header.content, position), 
              options = s.header.options + Styles("section")
          ),
          content = numberSections(s.content, position)
        )  
        
        def numberSections (blocks: Seq[Block], parentPosition: TreePosition, hasTitle: Boolean = false): Seq[Block] = {
          blocks.foldLeft((ListBuffer[Block](), 1, hasTitle)) {
            case ((acc, num, title), s: Section) =>
              val elements =
                if (title) transformRootSection(s)
                else if (parentPosition.depth < autonumberConfig.maxDepth) numberSection(s, parentPosition.forChild(num)) :: Nil
                else List(s)
              (acc ++= elements, if (title) num else num + 1, false)
            case ((acc, num, title), block) => (acc += block, num, title)
          }._1.toList
        }
        
        val blocks = if (autonumberConfig.sections) numberSections(sectionStructure, docPosition, hasTitle)
          else if (hasTitle) transformRootBlocks(sectionStructure)
          else sectionStructure
        
        errorBlock.toSeq ++ blocks
      }
      
      RootElement(numberedSections)
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
