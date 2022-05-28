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

package laika.directive.std

import cats.data.NonEmptyChain
import cats.syntax.all._
import laika.ast.Path.Root
import laika.ast._
import laika.config.LaikaKeys
import laika.directive.Templates

/** Provides the implementation for the standard directives for the head section in HTML templates.
  *
  * These include:
  * 
  * - `linkCSS`: Adds link elements to HTML/EPUB output for all or selected CSS files found in the document tree
  * - `linkJS`: Adds link elements to HTML/EPUB output for all or selected JavaScript files found in the document tree
  * 
  * For full documentation see the section about
  * [[https://planet42.github.io/Laika/07-reference/01-standard-directives.html#html-templates HTML Template Directives]]
  * in the manual.
  * 
  * @author Jens Halm
  */
object HTMLHeadDirectives {

  private def renderLinks (cursor: DocumentCursor,
                           suffixFilter: String => Boolean,
                           includes: NonEmptyChain[Path],
                           embedIn: (TemplateSpan, TemplateSpan)): Either[String, TemplateSpan] = {

    cursor.root.config.get[Path](LaikaKeys.site.apiPath, Root / "api").leftMap(_.message).map { excluded =>
      
      val formatSelector = cursor.root.outputContext.map(_.formatSelector)
      
      val preFiltered = cursor.root.target.staticDocuments.filter { doc =>
        doc.path.suffix.exists(suffixFilter) &&
          !doc.path.isSubPath(excluded) &&
          formatSelector.exists(doc.formats.contains)
      }
  
      val included = includes.foldLeft((preFiltered, Seq.empty[Path])) { case ((candidates, acc), include) =>
        val (newIncludes, remaining) = candidates.partition(_.path.isSubPath(include))
        (remaining, acc ++ newIncludes.map(_.path))
      }._2
  
      val allLinks: Seq[TemplateSpan] = included
        .map { path =>
          Seq(embedIn._1, TemplateElement(RawLink.internal(path)), embedIn._2)
        }
        .reduceOption((s1, s2) => s1 ++: TemplateString("\n    ") +: s2)
        .getOrElse(Seq())
      
      TemplateSpanSequence(allLinks)
    }
  }
  
  private def linkDirective (supportedSuffix: String, 
                             templateStart: String, 
                             templateEnd: String): Templates.DirectivePart[Either[String, TemplateSpan]] = {
    import Templates.dsl._

    (attribute("paths").as[Seq[Path]].optional.widen, cursor).mapN { (includes, cursor) =>
      val suffixFilter: String => Boolean = cursor.root.outputContext.map(_.formatSelector) match {
        case Some("epub") | Some("epub.xhtml") | Some("html") => 
          (suffix: String) => suffix.endsWith(supportedSuffix) && suffix != s"page.$supportedSuffix"
        case _ => _ => false
      }
      val includePaths: NonEmptyChain[Path] = NonEmptyChain.fromSeq(includes.getOrElse(Nil)).getOrElse(NonEmptyChain.one(Root))
      val embedIn = (TemplateString(templateStart), TemplateString(templateEnd))
      renderLinks(cursor, suffixFilter, includePaths, embedIn)
    }
  }
  
  /** Template directive that inserts links to all CSS inputs found in the document tree, using a path
    * relative to the currently processed document. 
    *
    * The optional include array attribute can be used to specify a sequence of directories or documents
    * based on Laika's virtual path to be included.
    * If omitted the entire input tree will be searched for CSS documents.
    *
    * Only has an effect for HTML and EPUB output, will be ignored for PDF output.
    */
  lazy val linkCSS: Templates.Directive = Templates.eval("linkCSS") {
    linkDirective("css", s"""<link rel="stylesheet" type="text/css" href="""", "\" />")
  }

  /** Template directive that inserts links to all JavaScript inputs found in the document tree, using a path
    * relative to the currently processed document. 
    *
    * The optional include array attribute can be used to specify a sequence of directories or documents
    * based on Laika's virtual path to be included.
    * If omitted the entire input tree will be searched for CSS documents.
    *
    * Only has an effect for HTML and EPUB output, will be ignored for PDF output.
    */
  lazy val linkJS: Templates.Directive = Templates.eval("linkJS") {
    linkDirective("js", s"""<script src="""", "\"></script>")
  }
  
}
