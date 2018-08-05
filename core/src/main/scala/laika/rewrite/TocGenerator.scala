/*
 * Copyright 2014-2016 the original author or authors.
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

import laika.ast._


/** Generates the tree model (consisting of BulletList elements) for
 *  the table of contents for a document or an entire document tree.
 */
object TocGenerator {

  
  private val bullet = StringBullet("*")
  
  private def styles (level: Int) = Styles("toc","level"+level)
  
  
  /** Generates the tree model (consisting of BulletList elements) for
   *  the table of contents for the specified document.
   *  
   *  @param doc the document to create a table of contents for
   *  @param depth the maximum depth to traverse when building the table, the depth is unlimited if the value is empty
   *  @param refPath the path from which the targets in the table will be linked
   *  @return a block element containing the table of contents as a BulltetList and its title
   */
  def fromDocument (doc: Document, depth: Int, refPath: Path): List[Block] = fromDocument(doc, 1, depth, refPath)
  
  private def fromDocument (doc: Document, curLevel: Int, maxLevel: Int, refPath: Path): List[Block] = {
    
    def sectionTitle (section: SectionInfo, path: Path, level: Int): Paragraph = {
      val title = section.title.content
      
      if (path == refPath)
        Paragraph(List(InternalLink(title, section.id)), options = styles(level))
      else
        Paragraph(List(CrossLink(title, section.id, PathInfo.fromPath(path, refPath.parent))), options = styles(level))
    }
    
    def sectionsToList (sections: Seq[SectionInfo], path: Path, curLevel: Int): List[Block] =
      if (sections.isEmpty || curLevel > maxLevel) Nil else {
        val items = for (section <- sections) yield {
          val title = sectionTitle(section, path, curLevel)
          val subSections = sectionsToList(section.content, path, curLevel + 1)
          BulletListItem(title :: subSections, bullet)
        }
        List(BulletList(items, bullet))
    }
    
    sectionsToList(doc.sections, doc.path, curLevel)
  }

  
  /** Generates the tree model (consisting of BulletList elements) for
   *  the table of contents for the specified document tree.
   *  
   *  @param tree the document tree to create a table of contents for
   *  @param depth the maximum depth to traverse when building the table, the depth is unlimited if the value is empty
   *  @param refPath the path from which the targets in the table will be linked
   *  @param treeTitleDoc the name for documents inserted for tree titles
   *  @return a block element containing the table of contents as a BulltetList and its title
   */
  def fromTree (tree: DocumentTree, depth: Int, refPath: Path, treeTitleDoc: Option[String] = None): List[Block] = 
    fromTree(tree, 1, depth, refPath, treeTitleDoc)
  
  private def fromTree (tree: DocumentTree, curLevel: Int, maxLevel: Int, refPath: Path, treeTitleDoc: Option[String]): List[Block] = {
    
    def titleOrName (content: TreeContent) = 
      if (content.title.nonEmpty) content.title
      else Seq(Text(content.name))
    
    def hasContent (nav: Navigatable): Boolean = nav match {
      case _:Document => true
      case tree: DocumentTree => tree.content.exists(hasContent)
    }
    
    def treeTitle (tree: DocumentTree, level: Int): Paragraph = 
      treeTitleDoc.fold(
        Paragraph(titleOrName(tree), options = styles(level))
      )( doc => 
        Paragraph(List(CrossLink(titleOrName(tree), "", PathInfo.fromPath(tree.path / doc, refPath.parent))), options = styles(level))
      )
    
    def docTitle (document: Document, level: Int): Paragraph =
      if (document.path == refPath)
        Paragraph(titleOrName(document), options = styles(level) + Styles("active"))
      else
        Paragraph(List(CrossLink(titleOrName(document), "", PathInfo.fromPath(document.path, refPath.parent))), options = styles(level))
    
    def navigatablesToList (navigatables: Seq[Navigatable], curLevel: Int): List[Block] = {
      if (curLevel > maxLevel) Nil else {
        val items = for (navigatable <- navigatables if hasContent(navigatable)) yield navigatable match {
          case doc: Document => 
            val title = docTitle(doc, curLevel)
            val sections = fromDocument(doc, curLevel + 1, maxLevel, refPath)
            BulletListItem(title :: sections, bullet)
          case tree: DocumentTree => 
            val title = treeTitle(tree, curLevel)
            val subtrees = navigatablesToList(tree.content, curLevel + 1)
            BulletListItem(title :: subtrees, bullet)
        }
          
        List(BulletList(items, bullet))
      }
    }
    
    navigatablesToList(tree.content, curLevel)
  }
    
  
}
