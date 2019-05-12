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

import laika.ast._
import laika.io.RenderedTreeRoot

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
  def navPoint (title: String, link: String, pos: Int, children: String): String =
    s"""    <navPoint id="navPoint-$pos">
       |      <navLabel>
       |        <text>$title</text>
       |      </navLabel>
       |      <content src="$link" />
       |$children
       |    </navPoint>""".stripMargin

  /** Generates navPoints for the structure of the DocumentTree. Individual
    * navPoints can stem from tree or subtree titles, document titles or
    * document sections, depending on which recursion depth is configured.
    * The configuration key for setting the recursion depth is `epub.toc.depth`.
    *
    * @param root the document tree to generate navPoints for
    * @param depth the recursion depth through trees, documents and sections
    * @return the navPoint XML nodes for the specified document tree
    */
  def navPoints (bookNav: Seq[BookNavigation]): String = {

    def linkOfFirstChild(children: Seq[BookNavigation]): BookNavigationLink = children.head match {
      case link: BookNavigationLink => link
      case header: BookSectionHeader => linkOfFirstChild(header.children)
    }

    bookNav.map {

      case BookSectionHeader(title, pos, children) =>
        navPoint(title, linkOfFirstChild(children).link, pos, navPoints(children)) // NCX does not allow navigation headers without links

      case BookNavigationLink(title, link, pos, children) =>
        navPoint(title, link, pos, navPoints(children))

    }.mkString("\n")
  }

  /** Renders the entire content of an NCX navigation file for
    * the specified document tree. The recursion depth will be applied
    * to the tree structure obtained by recursively processing titles of document
    * trees, documents and sections.
    * The configuration key for setting the recursion depth is `epub.toc.depth`.
    */
  def render (result: RenderedTreeRoot, identifier: String, depth: Int): String = {
    val title = if (result.title.isEmpty) "UNTITLED" else SpanSequence(result.title).extractText
    val bookNav = BookNavigation.forTree(result.rootTree, depth)
    val renderedNavPoints = navPoints(bookNav)
    fileContent(identifier, title, renderedNavPoints, depth)
  }


}
