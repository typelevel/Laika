/*
 * Copyright 2012-2019 the original author or authors.
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

import java.nio.charset.Charset

import laika.ast._
import laika.directive.Templates
import laika.io.{BinaryInput, ByteInput, CopiedDocument, RenderedTree}
import laika.parse.directive.TemplateParsers
import laika.parse.markup.DocumentParser.ParserInput
import laika.parse.text.TextParsers.unsafeParserFunction

/** Processes CSS inputs for EPUB containers.
  *
  * @author Jens Halm
  */
object StyleSupport {

  private val fallbackStyles = StaticDocument(ByteInput(StaticContent.fallbackStyles.getBytes(Charset.forName("UTF-8")), Path.Root / "styles" / "fallback.css"))

  /** Collects all CSS inputs (recursively) in the provided document tree.
    * CSS inputs are recognized by file suffix).
    */
  def collectStyles (tree: RenderedTree): Seq[BinaryInput] = {
    val children = tree.content.collect { case subTree: RenderedTree => subTree }.flatMap(collectStyles)
    children ++ tree.content.collect { case CopiedDocument(input) if input.path.suffix == "css" => input }
  }

  def collectStyles (tree: DocumentTree): Seq[BinaryInput] = {
    val children = tree.content.collect { case subTree: DocumentTree => subTree }.flatMap(collectStyles)
    children ++ tree.additionalContent.collect { case StaticDocument(input) if input.path.suffix == "css" => input }
  }

  /** Verifies that the specified document tree contains at least one CSS file
    * (determined by file suffix). If this is the case the tree is returned unchanged,
    * otherwise a new tree with a minimal fallback CSS inserted into the root is returned instead.
    */
  def ensureContainsStyles (tree: DocumentTree): DocumentTree = {

    val allStyles = collectStyles(tree)

    if (allStyles.isEmpty) tree.copy(additionalContent = tree.additionalContent :+ fallbackStyles)
    else tree
  }

  /** Template directive that inserts links to all CSS inputs found in the document tree, using a path
    * relative to the currently processed document.
    */
  lazy val styleLinksDirective: Templates.Directive = Templates.create("styleLinks") {
    import Templates.dsl._

    cursor.map { docCursor =>
      val refPath = docCursor.parent.target.path
      val allLinks = collectStyles(docCursor.root.target).map { input =>
        val path = input.path.relativeTo(refPath).toString
        s"""<link rel="stylesheet" type="text/css" href="$path" />"""
      }
      TemplateElement(RawContent(Seq("html","xhtml"), allLinks.mkString("\n    ")))
    }
  }

  /** Parser for the EPUB-XHTML default template that supports the `styleLinks` directive.
    */
  object XHTMLTemplateParser extends TemplateParsers(Map(styleLinksDirective.name -> styleLinksDirective)) {
    def parse (input: ParserInput): TemplateDocument = {
      val root = unsafeParserFunction(templateRoot)(input.context)
      TemplateDocument(input.path, root)
    }
  }

}
