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
import laika.ast.Path.Root
import laika.ast._
import laika.config.{ ConfigDecoder, Key, LaikaKeys }
import laika.directive.Templates

/** Provides the implementation for the standard directives for the head section in HTML templates.
  *
  * These include:
  *
  * - `linkCSS`: Adds link elements to HTML/EPUB output for all or selected CSS files found in the document tree
  * - `linkJS`: Adds link elements to HTML/EPUB output for all or selected JavaScript files found in the document tree
  *
  * For full documentation see the section about
  * [[https://typelevel.org/Laika/latest/07-reference/01-standard-directives.html#html-templates HTML Template Directives]]
  * in the manual.
  *
  * @author Jens Halm
  */
object HTMLHeadDirectives {

  private[std] case class SearchPaths(globalPaths: Seq[Path], localPaths: Seq[Path])

  private implicit val searchPathsDecoder: ConfigDecoder[SearchPaths] =
    ConfigDecoder.config.flatMap { config =>
      for {
        globalPaths <- config.get[Seq[Path]]("globalSearchPaths", Seq(Root))
        localPaths  <- config.get[Seq[Path]]("searchPaths", Nil)
      } yield {
        SearchPaths(globalPaths, localPaths)
      }
    }

  private def findDocuments(
      cursor: DocumentCursor,
      configKey: Key,
      supportedSuffix: String
  ): Either[String, Seq[Path]] = {

    cursor.config.get[Path](LaikaKeys.site.apiPath, Root / "api").leftMap(_.message).flatMap {
      excluded =>
        val formatSelector = cursor.root.outputContext.map(_.formatSelector)
        val preFiltered    = cursor.root.target.staticDocuments.filter { doc =>
          doc.path.suffix.exists(_.endsWith(supportedSuffix)) &&
          !doc.path.isSubPath(excluded) &&
          formatSelector.exists(doc.formats.contains)
        }

        cursor.config.get[SearchPaths](configKey, SearchPaths(Seq(Root), Nil)).leftMap(
          _.message
        ).map { searchPaths =>
          def filter(
              docs: Seq[StaticDocument],
              paths: Seq[Path],
              suffixCheck: String => Boolean
          ): (Seq[StaticDocument], Seq[Path]) =
            paths.foldLeft((docs, Seq.empty[Path])) { case ((candidates, acc), include) =>
              val (newIncludes, remaining) =
                candidates.partition(doc =>
                  doc.path.isSubPath(include) && doc.path.suffix.exists(suffixCheck)
                )
              (remaining, acc ++ newIncludes.map(_.path))
            }

          val (remaining, globalIncludes) =
            filter(preFiltered, searchPaths.globalPaths, _ != s"page.$supportedSuffix")
          val (_, localIncludes)          = filter(remaining, searchPaths.localPaths, _ => true)

          val (themeFiles, userFiles) = (globalIncludes ++ localIncludes).partition(
            _.isSubPath(Root / "helium")
          ) // TODO - generalize

          themeFiles ++ userFiles
        }

    }
  }

  private def renderLinks(
      documents: Seq[Path],
      templateStart: String,
      templateEnd: String
  ): TemplateSpan = {
    val allLinks: Seq[TemplateSpan] = documents
      .map { path =>
        Seq(
          TemplateString(templateStart),
          TemplateElement(RawLink.internal(path)),
          TemplateString(templateEnd)
        )
      }
      .reduceOption((s1, s2) => s1 ++: TemplateString("\n    ") +: s2)
      .getOrElse(Seq())

    TemplateSpanSequence(allLinks)
  }

  private def linkDirective(
      supportedSuffix: String,
      templateStart: String,
      templateEnd: String
  ): Templates.DirectivePart[Either[String, TemplateSpan]] = {
    import Templates.dsl._

    cursor.map { cursor =>
      val configKey = cursor.root.outputContext.map(_.formatSelector) match {
        case Some("epub") | Some("epub.xhtml") => Key("laika", "epub", supportedSuffix).some
        case Some("html")                      => Key("laika", "site", supportedSuffix).some
        case _                                 => None
      }
      configKey match {
        case Some(key) =>
          findDocuments(cursor, key, supportedSuffix).map(
            renderLinks(_, templateStart, templateEnd)
          )
        case None      => TemplateSpanSequence.empty.asRight
      }
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
