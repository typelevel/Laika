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

import cats.syntax.all._
import cats.data.{NonEmptyChain, NonEmptySet}
import laika.ast.{DocumentCursor, Path, RawContent, RelativePath, TemplateElement}
import laika.ast.Path.Root
import laika.config.{Config, LaikaKeys}
import laika.directive.Templates
import laika.rewrite.nav.{ConfigurablePathTranslator, TranslatorSpec}

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
                           format: Option[String],
                           includes: NonEmptyChain[Path],
                           excludes: Seq[Path],
                           render: RelativePath => String): TemplateElement = {

    val preFiltered = cursor.root.target.staticDocuments.filter { doc =>
      doc.path.suffix.exists(suffixFilter) &&
        !excludes.exists(doc.path.isSubPath) &&
        format.exists(doc.formats.contains)
    }

    val included = includes.foldLeft((preFiltered, Seq.empty[Path])) { case ((candidates, acc), include) =>
      val (newIncludes, remaining) = candidates.partition(_.path.isSubPath(include))
      (remaining, acc ++ newIncludes.map(_.path))
    }._2

    // TODO - this directive should insert a relative path resolver and leave the translation to the renderer
    // OR obtain access to a centralized path translator API (e.g. via the Cursor)
    def isVersioned (config: Config): Boolean = config.get[Boolean](LaikaKeys.versioned).getOrElse(false)

    val staticLookup = included.map { staticPath =>
      val config = cursor.root.treeConfig(staticPath)
      (staticPath, TranslatorSpec(isStatic = true, isVersioned = isVersioned(config)))
    }.toMap
    val lookup = staticLookup ++ Map(
      cursor.path -> TranslatorSpec(isStatic = false, isVersioned = isVersioned(cursor.config))
    )
    val translator = ConfigurablePathTranslator(cursor.root.config, "html", cursor.root.targetFormat.getOrElse("none"), cursor.path, lookup.get)
    
    val allLinks = included.map { staticPath =>
      val path = translator.translate(staticPath).relativeTo(translator.translate(cursor.path))
      render(path)
    }
    TemplateElement(RawContent(NonEmptySet.of("html","xhtml","epub"), allLinks.mkString("\n    ")))
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
  lazy val linkCSS: Templates.Directive = Templates.create("linkCSS") {
    import Templates.dsl._
    (attribute("paths").as[Seq[Path]].optional.widen, cursor).mapN { (includes, cursor) =>
      val suffixFilter: String => Boolean = cursor.root.targetFormat match {
        case Some("epub") | Some("epub.xhtml") | Some("html") => suffix: String => suffix.endsWith("css") && suffix != "page.css"
        case _ => _ => false
      }
      val excludePaths: Seq[Path] = Seq(cursor.root.config.get[Path](LaikaKeys.site.apiPath).toOption.getOrElse(Root / "api"))
      val includePaths: NonEmptyChain[Path] = NonEmptyChain.fromSeq(includes.getOrElse(Nil)).getOrElse(NonEmptyChain.one(Root))
      renderLinks(cursor, suffixFilter, cursor.root.targetFormat, includePaths, excludePaths, path => s"""<link rel="stylesheet" type="text/css" href="$path" />""")
    }
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
  lazy val linkJS: Templates.Directive = Templates.create("linkJS") {
    import Templates.dsl._
    (attribute("paths").as[Seq[Path]].optional.widen, cursor).mapN { (includes, cursor) =>
      val includePaths: NonEmptyChain[Path] = NonEmptyChain.fromSeq(includes.getOrElse(Nil)).getOrElse(NonEmptyChain.one(Root))
      val excludePaths: Seq[Path] = Seq(cursor.root.config.get[Path](LaikaKeys.site.apiPath).toOption.getOrElse(Root / "api"))
      renderLinks(cursor, _ == "js", cursor.root.targetFormat, includePaths, excludePaths, path => s"""<script src="$path"></script>""")
    }
  }
  
}
