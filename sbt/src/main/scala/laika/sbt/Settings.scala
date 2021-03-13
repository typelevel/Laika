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

package laika.sbt

import cats.effect.{IO, Resource}
import cats.effect.unsafe.implicits.global
import laika.api.builder.{OperationConfig, ParserBuilder}
import laika.api.{MarkupParser, Transformer}
import laika.bundle.{BundleOrigin, ExtensionBundle}
import laika.config.{Config, ConfigBuilder, LaikaKeys}
import laika.factory.MarkupFormat
import laika.format.{HTML, Markdown, ReStructuredText}
import laika.io.api.TreeParser
import laika.io.implicits._
import laika.io.model.{InputTree, InputTreeBuilder}
import laika.sbt.LaikaPlugin.autoImport._
import sbt.Keys._
import sbt._

/** Implementations for Laika's sbt settings.
  *
  * @author Jens Halm
  */
object Settings {

  import Def._

  val defaultInputs: Initialize[InputTreeBuilder[IO]] = setting {
    InputTree
      .apply[IO]((Laika / excludeFilter).value.accept _)
      .addDirectories((Laika / sourceDirectories).value)(laikaConfig.value.encoding)
  }
  
  val describe: Initialize[String] = setting {

    val userConfig = laikaConfig.value

    def mergedConfig (config: OperationConfig): OperationConfig = {
      config.copy(
        bundleFilter = userConfig.bundleFilter,
        renderMessages = userConfig.renderMessages,
        failOnMessages = userConfig.failOnMessages
      )
    }

    def createParser (format: MarkupFormat): ParserBuilder = {
      val parser = MarkupParser.of(format)
      parser.withConfig(mergedConfig(parser.config)).using(laikaExtensions.value: _*)
    }

    val transformer = Transformer
      .from(Markdown)
      .to(HTML)
      .withConfig(mergedConfig(createParser(Markdown).config))
      .using(laikaExtensions.value: _*)
      .parallel[IO]
      .withTheme(laikaTheme.value)
      .withAlternativeParser(createParser(ReStructuredText))
      .build

    val inputs = InputTree
      .apply[IO]((Laika / excludeFilter).value.accept _)
      .addDirectories((Laika / sourceDirectories).value)(laikaConfig.value.encoding)

    transformer
      .use(_
        .fromInput(inputs)
        .toDirectory((laikaSite / target).value)
        .describe
      )
      .unsafeRunSync()
      .copy(renderer = "Depending on task")
      .formatted
  }
  
  val parser: Initialize[Resource[IO, TreeParser[IO]]] = setting {
    
    val configFallbacks: ExtensionBundle = new ExtensionBundle {
      val description = "Config Defaults from sbt Plugin"
      override def origin = BundleOrigin.Library // for lowest precedence, as helium metadata should override this
      override def baseConfig: Config = ConfigBuilder.empty
        .withValue(LaikaKeys.metadata.child("title"), name.value)
        .withValue(LaikaKeys.site.metadata.child("title"), name.value)
        .withValue(LaikaKeys.metadata.child("description"), Keys.description.value)
        .withValue(LaikaKeys.site.metadata.child("description"), Keys.description.value)
        .withValue(LaikaKeys.metadata.child("version"), version.value)
        .withValue(LaikaKeys.site.metadata.child("version"), version.value)
        .withValue(LaikaKeys.artifactBaseName, name.value + "-" + version.value.split('.').take(2).mkString("."))
        .build
    }

    val userConfig = laikaConfig.value
    def createParser (format: MarkupFormat): ParserBuilder = {
      val parser = MarkupParser.of(format)
      val mergedConfig = parser.config.copy(
        bundles = parser.config.bundles :+ configFallbacks,
        bundleFilter = userConfig.bundleFilter,
        failOnMessages = userConfig.failOnMessages,
        renderMessages = userConfig.renderMessages,
        configBuilder = userConfig.configBuilder
      )
      parser.withConfig(mergedConfig).using(laikaExtensions.value: _*)
    }

    createParser(Markdown)
      .parallel[IO]
      .withTheme(laikaTheme.value)
      .withAlternativeParser(createParser(ReStructuredText))
      .build
  }
  
  val parserConfig: Initialize[OperationConfig] = setting {
    parser.value.use(p => IO.pure(p.config)).unsafeRunSync()
  }
  
  val artifactBaseName: Initialize[String] = setting {
    parserConfig.value.baseConfig.get[String](LaikaKeys.artifactBaseName).getOrElse(name.value)
  }

  /** The set of targets for the transformation tasks of all supported output formats.
    */
  val allTargets = setting {
    Set(
      (laikaSite / target).value, 
      (laikaXSLFO / target).value, 
      (laikaAST / target).value
    )
  }

}
