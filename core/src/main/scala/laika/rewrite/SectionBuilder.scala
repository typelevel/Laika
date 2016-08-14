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

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import laika.tree.Elements._
import laika.tree.Documents.DocumentContext

/** Rewrite rules responsible for building the section structure
 *  of a document based on the header elements it contains and
 *  their level.
 * 
 * @author Jens Halm
 */
object SectionBuilder extends (DocumentContext => RewriteRule) {

  
  class DefaultRule (context: DocumentContext) { 
    
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
          while (!stack.isEmpty && stack.top >= toLevel) {
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
        
        val hasSectionNumbers = context.autonumbering.config.sections
        val hasDocumentNumbers = context.autonumbering.config.documents
        val hasTitle = sectionStructure collect { case s:Section => s } match {
          case _ :: Nil => true
          case _ => false
        }
        val docNumber = context.autonumbering.number 
        val maxDepth = context.autonumbering.config.maxDepth
          
        def transformRootBlocks (blocks: Seq[Block]): Seq[Block] = 
          ((ListBuffer[Block]() /: blocks) {
            case (acc, s: Section) => acc ++= transformRootSection(s)
            case (acc, block) => acc += block
          }).toList
        
        def transformRootSection (s: Section): Seq[Block] = {
          val options = SomeOpt(s.header.options.id, s.header.options.styles - "section" + "title")
          val title = if (hasDocumentNumbers) Title(addNumber(s.header.content, docNumber, "title"), options) 
                      else Title(s.header.content, options)
          val content = if (hasSectionNumbers) numberSections(s.content, docNumber) else s.content
          title +: content
        }
        
        def addNumber (spans: Seq[Span], num: List[Int], style: String): Seq[Span] = 
          Text(num.mkString("","."," "), Styles(style+"Number")) +: spans 
          
        def numberSection (s: Section, num: List[Int]): Section = s.copy(
          header = s.header.copy(
              content = addNumber(s.header.content, num, "section"), 
              options = s.header.options + Styles("section")
          ),
          content = numberSections(s.content, num)
        )  
        
        def numberSections (blocks: Seq[Block], parentNumber: List[Int], hasTitle: Boolean = false): Seq[Block] = {
          (((ListBuffer[Block](), 1, hasTitle) /: blocks) { 
            case ((acc, num, title), s: Section) => {
              val elements = 
                if (title) transformRootSection(s) 
                else if (parentNumber.length < maxDepth) numberSection(s, parentNumber :+ num) :: Nil
                else List(s)
              (acc ++= elements, if (title) num else num + 1, false)
            }
            case ((acc, num, title), block) => (acc += block, num, title)
          })._1.toList
        }
        
        if (hasSectionNumbers) numberSections(sectionStructure, docNumber, hasTitle)
        else if (hasTitle) transformRootBlocks(sectionStructure)
        else sectionStructure
      }
      
      RootElement(numberedSections)
    }

    val rewrite: RewriteRule = { 
      case root: RootElement => Some(buildSections(root)) 
    }
  }
  
  /** Provides the default rewrite rules for building the section structure
   *  for the specified document (without applying them).
   */
  def apply (context: DocumentContext): RewriteRule = (new DefaultRule(context)).rewrite
  
}
