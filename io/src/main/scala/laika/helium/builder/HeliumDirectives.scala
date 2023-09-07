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
import laika.ast.{ TemplateSpanSequence, TemplateString }
import laika.config.LaikaKeys
import laika.directive.Templates
import laika.helium.Helium
import laika.rewrite.Versions
import laika.rewrite.nav.PathTranslator

/** @author Jens Halm
  */
private[helium] object HeliumDirectives {

  val initVersions: Templates.Directive = Templates.create("heliumInitVersions") {
    Templates.dsl.cursor.map { cursor =>
      val versions       = cursor.config.get[Versions].toOption
      val pathTranslator = cursor.root.pathTranslator.map(PathTranslator.ignoreVersions)

      val html = (versions, pathTranslator).tupled.fold("") { case (versions, pathTranslator) =>
        val isVersioned  = cursor.config.get[Boolean](LaikaKeys.versioned).getOrElse(false)
        val relativeRoot = "../" * (cursor.path.depth - (if (isVersioned) 0 else 1))

        val (currentPath, currentVersion, absoluteRoot) = if (isVersioned) {
          val path           = pathTranslator.translate(cursor.path).toString
          val version        = versions.currentVersion.pathSegment
          val siteBaseURL    = cursor.config.get[String](LaikaKeys.siteBaseURL).toOption
          val siteBaseURLStr = siteBaseURL.fold("null")(url => s""""$url"""")
          (path, version, siteBaseURLStr)
        }
        else ("", "", "null")

        s"""<script>initVersions("$relativeRoot", "$currentPath", "$currentVersion", $absoluteRoot);</script>"""
      }
      TemplateString(html)
    }
  }

  val initPreview: Templates.Directive = Templates.eval("heliumInitPreview") {
    import Templates.dsl._
    (positionalAttributes.as[String].widen, cursor).mapN { (targetIds, cursor) =>
      val res = for {
        enabled <- cursor.config.get(LaikaKeys.preview.enabled, false)
      } yield {
        val idArray = targetIds.mkString("[\"", "\",\"", "\"]")
        if (enabled) TemplateString(s"""<script>initPreview($idArray);</script>""")
        else TemplateSpanSequence.empty
      }
      res.leftMap(_.message)
    }
  }

  def all(helium: Helium): Seq[Templates.Directive] =
    Seq(
      initVersions,
      initPreview,
      HeliumHeadDirectives.includeCSS(
        helium.siteSettings.content.styleIncludes,
        helium.epubSettings.styleIncludes
      ),
      HeliumHeadDirectives.includeJS(
        helium.siteSettings.content.scriptIncludes,
        helium.epubSettings.scriptIncludes
      )
    )

}
