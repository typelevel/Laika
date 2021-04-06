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

import laika.ast.Path.Root
import laika.ast.{Path, TemplateString}
import laika.directive.Templates
import laika.rewrite.Versions
import laika.rewrite.nav.{ConfigurablePathTranslator, TranslatorConfig, TranslatorSpec}

/**
  * @author Jens Halm
  */
private[helium] object InitVersionsDirective {

  val definition: Templates.Directive = Templates.create("heliumInitVersions") {
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
  
}
