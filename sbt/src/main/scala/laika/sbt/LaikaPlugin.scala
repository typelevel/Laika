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

import cats.effect.IO
import laika.bundle.ExtensionBundle
import laika.helium.Helium
import laika.theme.ThemeProvider
import sbt.Keys._
import sbt._

/** Plugin that adapts the features of the Laika library for use from within sbt.
  *
  * It is only a thin layer on top the library API, defining sbt settings and tasks that allow
  * to configure a Laika transformation. The only feature not available in the library API
  * is the optional integration with the scaladoc API generator to be included in the
  * generated html site.
  *
  * The following settings are available:
  *
  * - `Laika / sourceDirectories`: one or more directories containing text markup and other markup files (default `src/docs`)
  *
  * - `laikaInputs`: freely compose the input tree from files, directories, streams, in-memory data. If this setting
  *   is defined `Laika / sourceDirectories` is discarded.
  *
  * - `Laika / target`: the output directory root (default `target/docs`)
  *
  * - `Laika / excludeFilter`: files in the source directories to be excluded (default `HiddenFileFilter`)
  *
  * - `laikaArtifactNameBuilder`: function that builds the name for artifacts (EPUB, PDF, ZIP) based on the provided
  *   context info
  *
  * - `laikaExtensions`: the main extension hook that allows to add one or more `ExtensionBundle` instances for adding
  *   directives, parser extensions, rewrite rules or custom renderers. See the API of [[laika.bundle.ExtensionBundle]].
  *
  * - `laikaConfig`: allows to specify additional flags and settings through instances of `LaikaConfig`:
  *     - `encoding`: specifies the character encoding (default `UTF-8`)
  *     - `strict`: switches off all extensions and only uses features defined in the spec of the markup languages (default `false`)
  *     - `withRawContent`: allows to include raw content like HTML embedded in text markup (default `false`)
  *     - `renderMessageLevel`: the minimum level required for an invalid node to be rendered to the output (default `Warning`)
  *     - `logMessageLevel`: the minimum level required for an invalid node to be logged to the console (default `Warning`)
  *     - `withConfigValue`: applies a global configuration value (that can be overridden in folders and documents)
  *
  * - `laikaTheme`: configuration for the theme to use for all transformations, if not specified the default
  *   `Helium` theme with all default colors and fonts will be used.
  *
  * - `laikaIncludeAPI`, `laikaIncludeEPUB` and `laikaIncludePDF`:
  *   specifies whether to include scaladoc and/or PDF output in the generated site.
  *
  * - `laikaDescribe`: inspects your setup and prints information about your inputs, outputs and installed extensions.
  *
  * The actual tasks for running a transformation are:
  *
  * - `laikaGenerate`: main transformation task of the plugin, accepts one or more arguments specifying the output
  *   formats. Valid arguments are `html`, `pdf`, `xsl-fo` and `ast`.
  *
  * - `laikaHTML`, `laikaPDF`, `laikaXSLFO`, `laikaAST`: shortcuts for `laikaGenerate` when only a single output format
  *   is required
  *
  * - `laikaSite`: combines the html generator with optionally also rendering a PDF document from the same input and
  *   creating scaladoc documentation and copying both over to the target directory.
  *
  * - `laikaPreview`: launches an HTTP server for the generated site and e-books, auto-refreshing when inputs change.
  *
  * - `laikaPackageSite`: packages the generated html site and (optionally) the included API documentation and
  *   PDF file into a zip archive.
  */
object LaikaPlugin extends AutoPlugin {

  val requirements     = plugins.JvmPlugin
  override val trigger = noTrigger

  object autoImport extends ExtensionBundles {

    // settingKey macro does not accept HK types
    implicit class InputTreeBuilder(val delegate: laika.io.model.InputTreeBuilder[IO])

    val Laika = sbt.config("laika")

    val laikaSite = taskKey[Set[File]]("Generates a static website")

    val laikaGenerate = inputKey[Set[File]]("Generates the specified output formats")

    val laikaHTML = taskKey[Set[File]]("Generates HTML output")

    val laikaEPUB = taskKey[Set[File]]("Generates EPUB output")

    val laikaPDF = taskKey[Set[File]]("Generates PDF output")

    val laikaXSLFO = taskKey[Set[File]]("Generates XSL-FO output")

    val laikaAST =
      taskKey[Set[File]]("Generates a formatted output of the AST obtained from a parser")

    val laikaDescribe =
      taskKey[String]("Describe the current configuration, formats and input and output files")

    val laikaExtensions =
      settingKey[Seq[ExtensionBundle]]("Custom extension bundles to use in each transformation")

    val laikaConfig = settingKey[LaikaConfig]("Configuration options for all transformations")

    val laikaInputs =
      settingKey[InputTreeBuilder]("Freely composed input tree, overriding sourceDirectories")

    val laikaTheme =
      settingKey[ThemeProvider]("Configures the theme to use for all transformations")

    val laikaGenerateAPI =
      taskKey[Seq[String]]("Generates API documentation and moves it to the site's API target")

    val laikaIncludeAPI =
      settingKey[Boolean]("Indicates whether API documentation should be copied to the site")

    val laikaIncludeEPUB =
      settingKey[Boolean]("Indicates whether EPUB output should be copied to the site")

    val laikaIncludePDF =
      settingKey[Boolean]("Indicates whether PDF output should be copied to the site")

    val laikaPreview = taskKey[Unit]("Launches an HTTP server for the generated site and e-books")

    val laikaPreviewConfig =
      taskKey[LaikaPreviewConfig]("Configuration options for the preview server")

    val laikaPackageSite = taskKey[File]("Create a zip file of the site")

    val LaikaConfig = laika.sbt.LaikaConfig

  }

  import autoImport._

  override def projectSettings: Seq[Setting[_]] = Seq(
    Laika / sourceDirectories   := Seq(sourceDirectory.value / "docs"),
    Laika / excludeFilter       := HiddenFileFilter,
    laikaInputs                 := Settings.defaultInputs.value,
    Laika / target              := target.value / "docs",
    laikaSite / target          := (Laika / target).value / "site",
    laikaXSLFO / target         := (Laika / target).value / "fo",
    laikaAST / target           := (Laika / target).value / "ast",
    laikaExtensions             := Nil,
    laikaConfig                 := LaikaConfig(),
    laikaPreviewConfig          := LaikaPreviewConfig.defaults,
    laikaTheme                  := Helium.defaults.build,
    laikaDescribe               := Settings.describe.value,
    laikaIncludeAPI             := false,
    laikaIncludeEPUB            := Settings.validated(
      Settings.parserConfig.value.baseConfig.get[Boolean]("helium.site.includeEPUB", false)
    ),
    laikaIncludePDF             := Settings.validated(
      Settings.parserConfig.value.baseConfig.get[Boolean]("helium.site.includePDF", false)
    ),
    laikaSite                   := Tasks.site.value,
    laikaGenerate               := Tasks.generate.evaluated,
    laikaGenerateAPI            := Tasks.generateAPI.value,
    laikaGenerateAPI / mappings := (Compile / packageDoc / mappings).value,
    laikaHTML                   := Tasks.generate.toTask(" html").value,
    laikaXSLFO                  := Tasks.generate.toTask(" xslfo").value,
    laikaEPUB                   := Tasks.generate.toTask(" epub").value,
    laikaPDF                    := Tasks.generate.toTask(" pdf").value,
    laikaAST                    := Tasks.generate.toTask(" ast").value,
    laikaPackageSite            := Tasks.packageSite.value,
    laikaPreview                := Tasks.startPreviewServer.value,
    Laika / clean               := Tasks.clean.value,
    laikaSite / mappings        := Def.sequential(Tasks.site, Tasks.mappings).value
  ) :+ (cleanFiles += (Laika / target).value)

}
