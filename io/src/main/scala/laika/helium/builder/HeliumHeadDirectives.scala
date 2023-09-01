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

package laika.helium.builder

import cats.syntax.all._
import laika.ast.Path.Root
import laika.ast._
import laika.directive.Templates
import laika.helium.config.{ InlineCSS, InlineJS, ScriptIncludes, StyleIncludes }
import laika.theme.config.{ CrossOrigin, ScriptAttributes, StyleAttributes }

private[helium] object HeliumHeadDirectives {

  private type Attributes = Seq[(String, String)]

  private def chooseIncludes[A](
      cursor: DocumentCursor,
      siteIncludes: A,
      epubIncludes: A
  ): Option[A] =
    cursor.root.outputContext.map(_.formatSelector) match {
      case Some("epub") | Some("epub.xhtml") => epubIncludes.some
      case Some("html")                      => siteIncludes.some
      case _                                 => None
    }

  private def findDocuments(
      cursor: DocumentCursor,
      specs: Seq[(Path, Attributes)],
      suffix: String
  ): Seq[(TemplateSpan, Attributes)] = {

    val formatSelector = cursor.root.outputContext.map(_.formatSelector)
    val candidates     = cursor.root.target.staticDocuments.filter { doc =>
      doc.path.suffix.exists(_.endsWith(suffix)) &&
      formatSelector.exists(doc.formats.contains)
    }

    def asLink(path: Path): TemplateSpan = TemplateElement(RawLink.internal(path))

    val targets = candidates.flatMap { staticDoc =>
      specs.collect {
        case (searchPath, attributes) if staticDoc.path.isSubPath(searchPath) =>
          staticDoc.path -> attributes
      }
    }

    val (themeTargets, userTargets) = targets.partition(
      _._1.isSubPath(Root / "helium")
    ) // TODO - generalize, e.g. via introducing Theme.managedPaths

    (themeTargets ++ userTargets).map { case (path, attrs) =>
      asLink(path) -> attrs
    }

  }

  private def collectTargets(
      cursor: DocumentCursor,
      suffix: String,
      external: Seq[(String, Attributes)],
      internal: Seq[(Path, Attributes)]
  ): Seq[(TemplateSpan, Attributes)] = {
    val ext = external.map { case (url, attributes) =>
      TemplateString(url) -> attributes
    }
    val int = findDocuments(cursor, internal, suffix)
    ext ++ int
  }

  private def collectAttributes(attributes: ScriptAttributes): Attributes = {
    val module  = if (attributes.isModule) Some("type" -> "module") else None
    val loading =
      if (attributes.isDefer) Some("defer" -> "")
      else if (attributes.isAsync) Some("async" -> "")
      else None
    loading.toSeq ++ module.toSeq ++ collectAttributes(attributes.integrity, attributes.crossOrigin)
  }

  private def collectAttributes(attributes: StyleAttributes): Attributes =
    collectAttributes(attributes.integrity, attributes.crossOrigin)

  private def collectAttributes(
      integrity: Option[String],
      crossOrigin: CrossOrigin
  ): Attributes = {
    val integrityAttr   = integrity.map("integrity" -> _)
    val crossOriginAttr = crossOrigin match {
      case CrossOrigin.UseCredentials => Some("crossorigin" -> "use-credentials")
      case CrossOrigin.Anonymous      => Some("crossorigin" -> "anonymous")
      case CrossOrigin.Unspecified    => None
    }
    integrityAttr.toSeq ++ crossOriginAttr.toSeq
  }

  private def renderAttribute(attr: (String, String)): String =
    if (attr._2.isEmpty) " " + attr._1
    else s""" ${attr._1}="${attr._2}""""

  /* Used for internal or external CSS/JS.
     Inline scripts or styles are handled separately.
   * */
  private def renderLinks(
      targets: Seq[(TemplateSpan, Attributes)],
      templateStart: String,
      templateEnd: String
  ): Seq[Seq[TemplateSpan]] = {
    targets
      .map { case (srcSpan, attributes) =>
        Seq(
          TemplateString(templateStart),
          srcSpan,
          TemplateString("\"" + attributes.map(renderAttribute).mkString),
          TemplateString(templateEnd)
        )
      }
  }

  private def renderInlineStyles(styles: Seq[InlineCSS]): Seq[Seq[TemplateSpan]] = {
    styles.map { styleDecl =>
      val content = s"""<style>
                       |${styleDecl.content}
                       |</style>""".stripMargin
      Seq(TemplateString(content))
    }
  }

  private def renderInlineScripts(scripts: Seq[InlineJS]): Seq[Seq[TemplateSpan]] = {
    scripts.map { script =>
      val moduleAttribute = if (script.isModule) """ type="module"""" else ""
      val content         =
        s"""<script$moduleAttribute>
           |${script.content}
           |</script>""".stripMargin
      Seq(TemplateString(content))
    }
  }

  private def concatLinks(links: Seq[Seq[TemplateSpan]]): TemplateSpan = {
    val allLinks: Seq[TemplateSpan] = links
      .reduceOption((s1, s2) => s1 ++: TemplateString("\n    ") +: s2)
      .getOrElse(Seq())

    TemplateSpanSequence(allLinks)
  }

  def includeCSS(siteIncludes: StyleIncludes, epubIncludes: StyleIncludes): Templates.Directive =
    Templates.create("includeCSS") {
      import Templates.dsl._

      val templateStart = """<link rel="stylesheet" type="text/css" href=""""
      val templateEnd   = " />"

      cursor.map { cursor =>
        chooseIncludes(cursor, siteIncludes, epubIncludes) match {
          case Some(allIncludes) =>
            val docIncludes = allIncludes.applyConditions(cursor.target)
            val external    = docIncludes.external.map { incl =>
              incl.url -> collectAttributes(incl.attributes)
            }
            val internal    = docIncludes.internal.map { incl =>
              incl.searchPath -> collectAttributes(incl.attributes)
            }
            val targets     = collectTargets(cursor, "css", external, internal)
            val links       = renderLinks(targets, templateStart, templateEnd)
            val inline      = renderInlineStyles(allIncludes.inline)
            concatLinks(links ++ inline)
          case None              => TemplateSpanSequence.empty
        }
      }
    }

  def includeJS(siteIncludes: ScriptIncludes, epubIncludes: ScriptIncludes): Templates.Directive =
    Templates.create("includeJS") {
      import Templates.dsl._

      val templateStart = """<script src=""""
      val templateEnd   = "></script>"

      cursor.map { cursor =>
        chooseIncludes(cursor, siteIncludes, epubIncludes) match {
          case Some(allIncludes) =>
            val docIncludes = allIncludes.applyConditions(cursor.target)
            val external    = docIncludes.external.map { incl =>
              incl.url -> collectAttributes(incl.attributes)
            }
            val internal    = docIncludes.internal.map { incl =>
              incl.searchPath -> collectAttributes(incl.attributes)
            }
            val targets     = collectTargets(cursor, "js", external, internal)
            val links       = renderLinks(targets, templateStart, templateEnd)
            val inline      = renderInlineScripts(allIncludes.inline)
            concatLinks(links ++ inline)
          case None              => TemplateSpanSequence.empty
        }
      }
    }

}
