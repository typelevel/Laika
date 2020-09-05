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

package laika.render.epub

import laika.ast.{NavigationHeader, NavigationItem, NavigationLink}
import laika.io.model.RenderedTreeRoot
import laika.render.TagFormatter

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
  def fileContent (identifier: String, title: String, navPoints: String, depth: Int): String =
    s"""<?xml version="1.0" encoding="UTF-8"?>
      |<ncx version="2005-1" xmlns="http://www.daisy.org/z3986/2005/ncx/">
      |  <head>
      |    <meta name="dtb:uid" content="$identifier" />
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
  private def navPoint (title: String, link: String, pos: Int, children: String): String =
    s"""    <navPoint id="navPoint-$pos">
       |      <navLabel>
       |        <text>${TagFormatter.escape(title)}</text>
       |      </navLabel>
       |      <content src="$link" />
       |$children
       |    </navPoint>""".stripMargin

  /** Generates navPoints for the structure of the DocumentTree. 
    * Individual navPoints can stem from tree or subtree titles, document titles or document sections, 
    * depending on which recursion depth is configured.
    * The configuration key for setting the recursion depth is `epub.toc.depth`.
    *
    * @param bookNav the structure to generate navPoints for
    * @return the navPoint XML nodes for the specified document tree
    */
  private def navPoints (bookNav: Seq[NavigationItem], pos: Iterator[Int] = Iterator.from(0)): String = {

    def linkOfFirstChild(children: Seq[NavigationItem]): NavigationLink = children.head match {
      case link: NavigationLink => link
      case header: NavigationHeader => linkOfFirstChild(header.content)
    }

    bookNav.map {

      case NavigationHeader(title, children, _) =>
        navPoint(title.extractText, linkOfFirstChild(children).target.render(), pos.next(), navPoints(children, pos)) // NCX does not allow navigation headers without links

      case NavigationLink(title, target, children, _, _) =>
        navPoint(title.extractText, target.render(), pos.next(), navPoints(children, pos))

    }.mkString("\n")
  }

  /** Renders the entire content of an NCX navigation file for
    * the specified document tree. The recursion depth will be applied
    * to the tree structure obtained by recursively processing titles of document
    * trees, documents and sections.
    * The configuration key for setting the recursion depth is `epub.toc.depth`.
    */
  def render[F[_]] (result: RenderedTreeRoot[F], title: String, identifier: String, depth: Option[Int]): String = {
    val bookNav = NavigationBuilder.forTree(result.tree, depth)
    val renderedNavPoints = navPoints(bookNav)
    def flattenItems (items: Seq[NavigationItem], level: Int): Int = 
      if (items.isEmpty) level else items.map(item => if (item.content.isEmpty) level else flattenItems(item.content, level + 1)).max
    fileContent(identifier, title, renderedNavPoints, flattenItems(bookNav, 1))
  }


}
