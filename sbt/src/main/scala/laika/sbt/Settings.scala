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

import cats.effect.{Async, IO, Resource}
import cats.effect.unsafe.implicits.global
import laika.api.builder.{OperationConfig, ParserBuilder}
import laika.api.{MarkupParser, Transformer}
import laika.bundle.{BundleOrigin, ExtensionBundle}
import laika.config.Config.ConfigResult
import laika.config.{Config, ConfigBuilder, LaikaKeys}
import laika.factory.MarkupFormat
import laika.format.{HTML, Markdown, ReStructuredText}
import laika.io.api.TreeParser
import laika.io.config.SiteConfig
import laika.io.implicits.*
import laika.io.model.{FilePath, InputTree, InputTreeBuilder}
import laika.sbt.LaikaPlugin.autoImport.*
import sbt.Keys.*
import sbt.*

/** Implementations for Laika's sbt settings.
  *
  * @author Jens Halm
  */
object Settings {

  import Def._
  
  private def asLaikaFileFilter(jFilter: java.io.FileFilter): laika.io.model.FileFilter = 
    new laika.io.model.FileFilter {
      def filter[F[_] : Async] (file: FilePath) = Async[F].delay(jFilter.accept(file.toJavaFile))
    }

  val defaultInputs: Initialize[InputTreeBuilder[IO]] = setting {
    InputTree
      .apply[IO](asLaikaFileFilter((Laika / excludeFilter).value))
      .addDirectories((Laika / sourceDirectories).value.map(FilePath.fromJavaFile))(laikaConfig.value.encoding)
  }
  
  val describe: Initialize[Task[String]] = task {

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

    val inputs = laikaInputs.value.delegate

    val result = transformer
      .use(_
        .fromInput(inputs)
        .toDirectory(FilePath.fromJavaFile((laikaSite / target).value))
        .describe
      )
      .unsafeRunSync()
      .copy(renderer = "Depending on task")
      .formatted
    
    streams.value.log.success("\n" + result)
    
    result
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
    validated(parserConfig.value.baseConfig.get[String](LaikaKeys.artifactBaseName, name.value))
  }
  
  val apiTargetDirectory: Initialize[File] = setting {
    (laikaSite / target).value / validated(SiteConfig.apiPath(Settings.parserConfig.value.baseConfig)).relative.toString
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

  /** Adapts a Laika configuration value to the synchronous/impure APIs of sbt.
    * This method throws an exception in case the provided value is a `Left`.
    */
  def validated[T] (value: ConfigResult[T]): T = value.fold[T](
    err => throw new RuntimeException(s"Error in project configuration: ${err.message}"),
    identity
  )

}
