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

import cats.effect.{ Async, IO, Resource }
import cats.effect.unsafe.implicits.global
import laika.api.builder.{ OperationConfig, ParserBuilder }
import laika.api.MarkupParser
import laika.api.bundle.{ BundleOrigin, ExtensionBundle }
import laika.api.config.{ Config, ConfigBuilder }
import laika.api.format.MarkupFormat
import laika.api.config.Config.ConfigResult
import laika.config.LaikaKeys
import laika.format.{ AST, EPUB, HTML, Markdown, PDF, ReStructuredText, XSLFO }
import laika.io.api.TreeParser
import laika.io.internal.config.SiteConfig
import laika.io.syntax.*
import laika.io.model.{ FilePath, InputTree, InputTreeBuilder }
import laika.sbt.LaikaPlugin.autoImport.*
import sbt.Keys.*
import sbt.*

/** Implementations for Laika's sbt settings.
  *
  * @author Jens Halm
  */
object Settings {

  import Def.*

  private def asLaikaFileFilter(jFilter: java.io.FileFilter): laika.io.model.FileFilter =
    new laika.io.model.FileFilter {
      def filter[F[_]: Async](file: FilePath) = Async[F].delay(jFilter.accept(file.toJavaFile))
    }

  val defaultInputs: Initialize[InputTreeBuilder[IO]] = setting {
    InputTree
      .apply[IO](asLaikaFileFilter((Laika / excludeFilter).value))
      .addDirectories((Laika / sourceDirectories).value.map(FilePath.fromJavaFile))(
        laikaConfig.value.encoding
      )
  }

  val parser: Initialize[Resource[IO, TreeParser[IO]]] = setting {

    val configFallbacks: ExtensionBundle = new ExtensionBundle {
      val description     = "Config Defaults from sbt Plugin"
      override def origin =
        BundleOrigin.Library // for lowest precedence, as helium metadata should override this
      override def baseConfig: Config = ConfigBuilder.empty
        .withValue(LaikaKeys.metadata.child("title"), name.value)
        .withValue(LaikaKeys.site.metadata.child("title"), name.value)
        .withValue(LaikaKeys.metadata.child("description"), Keys.description.value)
        .withValue(LaikaKeys.site.metadata.child("description"), Keys.description.value)
        .withValue(LaikaKeys.metadata.child("version"), version.value)
        .withValue(LaikaKeys.site.metadata.child("version"), version.value)
        .withValue(
          LaikaKeys.artifactBaseName,
          name.value + "-" + version.value.split('.').take(2).mkString(".")
        )
        .build
    }

    val userConfig                                        = laikaConfig.value
    def createParser(format: MarkupFormat): ParserBuilder = {
      val parser       = MarkupParser.of(format)
      val mergedConfig = new OperationConfig(
        bundles = parser.config.bundles :+ configFallbacks,
        bundleFilter = userConfig.bundleFilter,
        messageFilters = userConfig.messageFilters,
        configBuilder = userConfig.configBuilder,
        compactRendering = parser.config.compactRendering
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

  val rendererConfigs: Initialize[Seq[RendererConfig]] = setting {

    val baseConfig   = parserConfig.value.baseConfig
    val downloadPath =
      (laikaSite / target).value / validated(SiteConfig.downloadPath(baseConfig)).relative.toString

    Seq(
      TextRendererConfig(
        "html",
        HTML,
        (laikaSite / target).value,
        includeInSite = true
      ),
      TextRendererConfig(
        "xsl-fo",
        XSLFO,
        (laikaXSLFO / target).value,
        includeInSite = false
      ),
      TextRendererConfig(
        "ast",
        AST,
        (laikaAST / target).value,
        includeInSite = false
      ),
      BinaryRendererConfig(
        "pdf",
        PDF,
        downloadPath,
        artifactBaseName.value,
        "pdf",
        includeInSite = laikaIncludePDF.value,
        supportsSeparations = true
      ),
      BinaryRendererConfig(
        "epub",
        EPUB,
        downloadPath,
        artifactBaseName.value,
        "epub",
        includeInSite = laikaIncludeEPUB.value,
        supportsSeparations = true
      )
    )
  }

  val apiTargetDirectory: Initialize[File] = setting {
    (laikaSite / target).value / validated(
      SiteConfig.apiPath(Settings.parserConfig.value.baseConfig)
    ).relative.toString
  }

  /** The set of targets for the transformation tasks of all supported output formats.
    */
  val allTargets: Initialize[Set[File]] = setting {
    Set(
      (laikaSite / target).value,
      (laikaXSLFO / target).value,
      (laikaAST / target).value
    )
  }

  /** Adapts a Laika configuration value to the synchronous/impure APIs of sbt.
    * This method throws an exception in case the provided value is a `Left`.
    */
  private[sbt] def validated[T](value: ConfigResult[T]): T = value.fold[T](
    err => throw new RuntimeException(s"Error in project configuration: ${err.message}"),
    identity
  )

}
