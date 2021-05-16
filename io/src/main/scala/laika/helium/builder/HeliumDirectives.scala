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
import laika.ast.{Path, TemplateSpanSequence, TemplateString}
import laika.config.LaikaKeys
import laika.directive.Templates
import laika.rewrite.Versions
import laika.rewrite.nav.{ConfigurablePathTranslator, TranslatorConfig, TranslatorSpec}

/**
  * @author Jens Halm
  */
private[helium] object HeliumDirectives {

  val initVersions: Templates.Directive = Templates.create("heliumInitVersions") {
    Templates.dsl.cursor.map { cursor =>
      val versions = cursor.config.get[Versions].toOption
      val html = versions.fold("") { versions =>
        val localRootPrefix = "../" * cursor.path.depth
        val lookup: Path => Option[TranslatorSpec] = path => 
          if (path == cursor.path) Some(TranslatorSpec(isStatic = false, isVersioned = false)) else None
        val config = TranslatorConfig.readFrom(cursor.root.config).getOrElse(TranslatorConfig.empty)
        val translator = ConfigurablePathTranslator(config, "html", "html", Root / "doc", lookup)
        val currentPath = translator.translate(cursor.path).toString
        val currentVersion = versions.currentVersion.displayValue
        s"""<script>initVersions("$localRootPrefix", "$currentPath", "$currentVersion");</script>"""
      } 
      TemplateString(html)
    }
  }
  
  val initPreview: Templates.Directive = Templates.eval("heliumInitPreview") {
    import Templates.dsl._
    (positionalAttributes.as[String].widen, cursor).mapN { (targetIds, cursor) =>
      val res = for {
        enabled      <- cursor.config.get(LaikaKeys.preview.enabled, false)
      } yield {
        val idArray = targetIds.mkString("[\"", "\",\"", "\"]")
        if (enabled) TemplateString(s"""<script>initPreview($idArray);</script>""")
        else TemplateSpanSequence.empty
      }
      res.leftMap(_.message)
    }
  }
  
  val all: Seq[Templates.Directive] = Seq(initVersions, initPreview)
  
}
