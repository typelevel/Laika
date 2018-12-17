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

/** Renders the entire content of an NCX navigation file.
  * These files will be ignored by EPUB 3 readers and are only
  * added for backwards-compatibility with older readers.
  *
  * @author Jens Halm
  */
class NCXRenderer {


  /** Inserts the specified (pre-rendered) navPoints into the NCX document template
    * and returns the content of the entire NCX file.
    */
  def fileContent (uuid: String, title: String, navPoints: String, depth: Int): String =
    s"""<?xml version="1.0" encoding="UTF-8"?>
      |<ncx version="2005-1" xmlns="http://www.daisy.org/z3986/2005/ncx/">
      |  <head>
      |    <meta name="dtb:uid" content="urn:uuid:$uuid" />
      |    <meta name="dtb:depth" content="$depth" />
      |    <meta name="dtb:totalPageCount" content="0" />
      |    <meta name="dtb:maxPageNumber" content="0" />
      |  </head>
      |  <docTitle>
      |    <text>$title</text>
      |  </docTitle>
      |  <navMap>
      |$navPoints
      |  </navMap>
      |</ncx>
    """.stripMargin


  /** Renders a single navPoint node.
    */
  def navPoint (title: String, link: String, pos: Int, navPoints: String): String =
    s"""    <navPoint id="navPoint-$pos">
       |      <navLabel>
       |        <text>$title</text>
       |      </navLabel>
       |      <content src="$link" />
       |$navPoints
       |    </navPoint>""".stripMargin


  /** Indicates whether the specified navigation item contains at least one document.
    */
  protected def hasContent (level: Int)(nav: Navigatable): Boolean = nav match {
    case _: Document => true
    case tree: DocumentTree => if (level > 0) tree.content.exists(hasContent(level - 1)) else false
  }

  /** Generates navPoints for the structure of the DocumentTree. Individual
    * navPoints can stem from tree or subtree titles, document titles or
    * document sections, depending on which recursion depth is configured.
    * The configuration key for setting the recursion depth is `epub.toc.depth`.
    *
    * @param root the document tree to generate navPoints for
    * @param depth the recursion depth through trees, documents and sections
    * @return the navPoint XML nodes for the specified document tree
    */
  def navPoints (root: DocumentTree, depth: Int): String = {

    def fullPath (path: Path): String = {
      val parent = path.parent match {
        case Root => ""
        case _ => path.parent.toString
      }
      "text" + parent + "/" + path.basename + ".xhtml"
    }

    def navPointsForSections (path: Path, sections: Seq[SectionInfo], levels: Int, pos: Iterator[Int]): String =
      if (levels == 0) ""
      else (for (section <- sections) yield {
        val title = section.title.extractText
        val parentPos = pos.next
        val children = navPointsForSections(path, section.content, levels - 1, pos)
        navPoint(title, fullPath(path) + "#" + section.id, parentPos, children)
      }).mkString("\n")

    def navPointsForTree (tree: DocumentTree, levels: Int, pos: Iterator[Int]): String = {
      if (levels == 0) ""
      else (for (nav <- tree.content if hasContent(levels - 1)(nav)) yield nav match {
        case doc: Document =>
          val title = if (doc.title.nonEmpty) SpanSequence(doc.title).extractText else doc.name
          val parentPos = pos.next
          val children = navPointsForSections(doc.path, doc.sections, levels - 1, pos)
          navPoint(title, fullPath(doc.path), parentPos, children)
        case subtree: DocumentTree =>
          val title = if (subtree.title.nonEmpty) SpanSequence(subtree.title).extractText else subtree.name
          val parentPos = pos.next
          val children = navPointsForTree(subtree, levels - 1, pos)
          val link = fullPath(subtree.content.collectFirst{ case d: Document => d }.get.path)
          navPoint(title, link, parentPos, children)
      }).mkString("\n")
    }

    if (depth == 0) ""
    else navPointsForTree(root, depth, Iterator.from(0))
  }

  /** Renders the entire content of an NCX navigation file for
    * the specified document tree. The recursion depth will be applied
    * to the tree structure obtained by recursively processing titles of document
    * trees, documents and sections.
    * The configuration key for setting the recursion depth is `epub.toc.depth`.
    */
  def render (tree: DocumentTree, uuid: String, depth: Int): String = {
    val title = SpanSequence(tree.title).extractText
    val renderedNavPoints = navPoints(tree, depth)
    fileContent(uuid, title, renderedNavPoints, depth)
  }


}
