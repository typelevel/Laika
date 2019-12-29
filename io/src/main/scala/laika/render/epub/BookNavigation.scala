/*
 * Copyright 2013-2018 the original author or authors.
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

package laika.render.epub

import laika.ast.Path.Root
import laika.ast._
import laika.io.model.{RenderedDocument, RenderedTree}

/** Represents a recursive book navigation structure.
  */
trait BookNavigation {
  def title: String
  def pos: Int
  def children: Seq[BookNavigation]
}

/** Represents a book navigation entry that only serves as a section header without linking to content.
  */
case class BookSectionHeader (title: String, pos: Int, children: Seq[BookNavigation]) extends BookNavigation

/** Represents a book navigation entry that links to content in the document tree.
  */
case class BookNavigationLink (title: String, link: String, pos: Int, children: Seq[BookNavigation]) extends BookNavigation

object BookNavigation {

  /** Provides the full path to the document relative to the EPUB container root
    * from the specified virtual path of the Laika document tree.
    */
  def fullPath (path: Path, forceXhtml: Boolean = false): String = {
    val finalPath = if (forceXhtml || path.suffix == "html") path.withSuffix("epub.xhtml") else path
    val parent = finalPath.parent match {
      case Root => ""
      case _ => finalPath.parent.toString
    }
    s"content$parent/${finalPath.name}"
  }

  def forTree (tree: RenderedTree, depth: Int, pos: Iterator[Int] = Iterator.from(0)): Seq[BookNavigation] = {

    def hasContent (level: Int)(nav: Navigatable): Boolean = nav match {
      case _: RenderedDocument => true
      case tree: RenderedTree => if (level > 0) (tree.titleDocument.toSeq ++ tree.content).exists(hasContent(level - 1)) else false
    }

    def forSections (path: Path, sections: Seq[SectionInfo], levels: Int, pos: Iterator[Int]): Seq[BookNavigation] =
      if (levels == 0) Nil
      else for (section <- sections) yield {
        val title = section.title.extractText
        val parentPos = pos.next
        val children = forSections(path, section.content, levels - 1, pos)
        BookNavigationLink(title, fullPath(path, forceXhtml = true) + "#" + section.id, parentPos, children)
      }

    if (depth == 0) Nil
    else {
      for (nav <- tree.content if hasContent(depth - 1)(nav)) yield nav match {
        case doc: RenderedDocument =>
          val title = if (doc.title.nonEmpty) SpanSequence(doc.title).extractText else doc.name
          val parentPos = pos.next
          val children = forSections(doc.path, doc.sections, depth - 1, pos)
          BookNavigationLink(title, fullPath(doc.path, forceXhtml = true), parentPos, children)
        case subtree: RenderedTree =>
          val title = if (subtree.title.nonEmpty) SpanSequence(subtree.title).extractText else subtree.name
          val parentPos = pos.next
          val children = forTree(subtree, depth - 1, pos)
          val targetDoc = subtree.titleDocument.orElse(subtree.content.collectFirst{ case d: RenderedDocument => d }).get
          val link = fullPath(targetDoc.path, forceXhtml = true)
          if (depth == 1 || subtree.titleDocument.nonEmpty) BookNavigationLink(title, link, parentPos, children)
          else BookSectionHeader(title, parentPos, children)
      }
    }
  }
  
}
