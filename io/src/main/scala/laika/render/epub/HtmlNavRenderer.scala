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

import laika.ast.{ NavigationItem, NavigationLink }
import laika.io.model.RenderedTreeRoot
import laika.render.TagFormatter
import laika.render.epub.StyleSupport.collectStylePaths

/** Renders the entire content of an EPUB HTML navigation file.
  *
  * @author Jens Halm
  */
class HtmlNavRenderer {

  /** Inserts the specified (pre-rendered) navPoints into the NCX document template
    * and returns the content of the entire NCX file.
    */
  def fileContent(
      title: String,
      styles: String,
      navItems: String
  ): String =
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<!DOCTYPE html>
       |<html xmlns="http://www.w3.org/1999/xhtml" xmlns:epub="http://www.idpf.org/2007/ops">
       |  <head>
       |    <meta charset="utf-8" />
       |    <meta name="generator" content="laika" />
       |    <title>$title</title>
       |    $styles
       |  </head>
       |  <body>
       |    <nav epub:type="toc" id="toc">
       |      <h1 id="toc-title">$title</h1>
       |$navItems
       |    </nav>
       |  </body>
       |</html>
       |
    """.stripMargin.replaceAll("[\n]+", "\n")

  /** Renders a single navigation link.
    */
  private def navLink(title: String, link: String, pos: Int, children: String): String =
    s"""        <li id="toc-li-$pos">
       |          <a href="$link">${TagFormatter.escape(title)}</a>
       |$children
       |        </li>""".stripMargin

  /** Always use the link to the first child of a navigation header.
    * Despite the fact that the EPUB spec supports "unlinked headers",
    * which are just a `<span>` element instead of an `<a>` element,
    * many e-book readers such as iBooks choke on them to the point of rendering navigation menus useless.
    *
    * Unlinked navigation headers are described in the EPUB spec here:
    * https://www.w3.org/publishing/epub32/epub-packages.html#sec-package-nav-def-model
    */
  private def linkOfFirstChild(children: Seq[NavigationItem]): NavigationLink =
    children.head.link.getOrElse(linkOfFirstChild(children.head.content))

  /** Generates navigation entries for the structure of the DocumentTree. Individual
    * entries can stem from tree or subtree titles, document titles or
    * document sections, depending on which recursion depth is configured.
    * The configuration key for setting the recursion depth is `epub.toc.depth`.
    *
    * @param bookNav the navigation items to generate navPoints for
    * @return the navPoint XML nodes for the specified document tree as a String
    */
  private def navItems(
      bookNav: Seq[NavigationItem],
      pos: Iterator[Int] = Iterator.from(0)
  ): String =
    if (bookNav.isEmpty) ""
    else
      bookNav.map { item =>
        navLink(
          item.title.extractText,
          item.link.getOrElse(linkOfFirstChild(item.content)).target.render(),
          pos.next(),
          navItems(item.content, pos)
        )
      }.mkString("      <ol class=\"toc\">\n", "\n", "\n      </ol>")

  /** Renders the entire content of an EPUB HTML navigation file for
    * the specified document tree. The recursion depth will be applied
    * to the tree structure obtained by recursively processing titles of document
    * trees, documents and sections.
    * The configuration key for setting the recursion depth is `epub.toc.depth`.
    */
  def render[F[_]](result: RenderedTreeRoot[F], title: String, depth: Option[Int]): String = {
    val bookNav           = NavigationBuilder.forTree(result.tree, depth)
    val styles            = collectStylePaths(result).map { path =>
      s"""<link rel="stylesheet" type="text/css" href="content${path.toString}" />"""
    }.mkString("\n    ")
    val renderedNavPoints = navItems(bookNav)
    fileContent(title, styles, renderedNavPoints)
  }

}
