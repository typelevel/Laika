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
import org.apache.fop.apps.FopFactory
import sbt.Keys._
import sbt._


/** Plugin that adapts the features of the Laika library for use from within sbt.
  *
  * It is only a thin layer on top the library API, defining sbt settings and tasks that allow
  * to configure a Laika transformation. The only feature not available in the library API
  * is the optional integration with the scaladoc API generator to be included in the
  * generated html site.
  *
  *
  * The following settings are available:
  *
  * - `sourceDirectories in Laika`: one or more directories containing text markup and other markup files (default `src/docs`)
  *
  * - `target in Laika`: the output directory root (default `target/docs`)
  *
  * - `excludeFilter in Laika`: files in the source directories to be excluded (default `HiddenFileFilter`)
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
  * - `includeAPI` and `includePDF`: specifies whether to include scaladoc and/or PDF output in the generated site.
  *
  * - `fopConfig` and `fopFactory`: specify custom Apache FOP factories to be used for rendering PDF.
  *
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
  * - `laikaPackageSite`: packages the generated html site and (optionally) the included API documentation and
  *   PDF file into a zip archive.
  */
object LaikaPlugin extends AutoPlugin {

  val requirements = plugins.JvmPlugin
  override val trigger = noTrigger


  object autoImport extends ExtensionBundles {

    implicit class InputTreeBuilder (val delegate: laika.io.model.InputTreeBuilder[IO]) // settingKey macro does not accept HK types
    
    val Laika             = sbt.config("laika")


    val laikaSite         = taskKey[File]("Generates a static website")

    val laikaGenerate     = inputKey[Set[File]]("Generates the specified output formats")

    val laikaHTML         = taskKey[Set[File]]("Generates HTML output")

    val laikaEPUB         = taskKey[File]("Generates EPUB output")

    val laikaPDF          = taskKey[File]("Generates PDF output")

    val laikaXSLFO        = taskKey[Set[File]]("Generates XSL-FO output")

    val laikaAST          = taskKey[Set[File]]("Generates a formatted output of the AST obtained from a parser")
    
    
    val laikaDescribe     = settingKey[String]("Describe the current configuration, formats and input and output files")

    val laikaExtensions   = settingKey[Seq[ExtensionBundle]]("Custom extension bundles to use in each transformation")

    val laikaConfig       = settingKey[LaikaConfig]("Configuration options for all transformations")
    
    val laikaInputs       = settingKey[InputTreeBuilder]("Freely composed input tree, overriding sourceDirectories")

    @deprecated("custom fop factories are deprecated as this would bypass laika's font registration", "0.16.0")
    val fopFactory        = settingKey[Option[FopFactory]]("The FopFactory for the PDF renderer")


    val laikaIncludeAPI   = settingKey[Boolean]("Indicates whether API documentation should be copied to the site")

    val laikaIncludeEPUB  = settingKey[Boolean]("Indicates whether EPUB output should be copied to the site")

    val laikaIncludePDF   = settingKey[Boolean]("Indicates whether PDF output should be copied to the site")

    val laikaCopyAPI      = taskKey[File]("Copies the API documentation to the site")

    val laikaCopyEPUB     = taskKey[File]("Copies the EPUB output to the site")

    val laikaCopyPDF      = taskKey[File]("Copies the PDF output to the site")

    val laikaPackageSite  = taskKey[File]("Create a zip file of the site")
    
    val LaikaConfig = laika.sbt.LaikaConfig
  }


  import autoImport._

  override def projectSettings: Seq[Setting[_]] = Seq(
    sourceDirectories in Laika  := Seq(sourceDirectory.value / "docs"),

    target in Laika         := target.value / "docs",
    target in laikaSite     := (target in Laika).value / "site",
    target in laikaXSLFO    := (target in Laika).value / "fo",
    target in laikaAST      := (target in Laika).value / "ast",
    target in laikaCopyAPI  := (target in laikaSite).value / "api",

    excludeFilter in Laika  := HiddenFileFilter,

    laikaExtensions         := Nil,
    laikaConfig             := LaikaConfig(),
    laikaInputs             := Settings.defaultInputs.value,

    laikaIncludeAPI         := false,
    laikaIncludeEPUB        := false,
    laikaIncludePDF         := false,

    fopFactory              := None,

    laikaSite               := Def.sequential(Tasks.site, Tasks.copy).value,
    laikaGenerate           := Tasks.generate.evaluated,
    laikaHTML               := Tasks.generate.toTask(" html").value,
    laikaXSLFO              := Tasks.generate.toTask(" xslfo").value,
    laikaEPUB               := Tasks.generate.toTask(" epub").value.headOption.getOrElse((artifactPath in laikaEPUB).value),
    laikaPDF                := Tasks.generate.toTask(" pdf").value.headOption.getOrElse((artifactPath in laikaPDF).value),
    laikaAST                := Tasks.generate.toTask(" ast").value,
    laikaDescribe           := Settings.describe.value,
    laikaCopyAPI            := Tasks.copyAPI.value,
    laikaCopyEPUB           := Tasks.copyEPUB.value,
    laikaCopyPDF            := Tasks.copyPDF.value,
    laikaPackageSite        := Tasks.packageSite.value,
    clean in Laika          := Tasks.clean.value,

    mappings in laikaSite   := sbt.Path.allSubpaths(laikaSite.value).toSeq,

    artifact in laikaPackageSite     := Artifact(moduleName.value, Artifact.DocType, "zip", "site"),
    artifact in laikaEPUB            := Artifact(moduleName.value, Artifact.DocType, "epub"),
    artifact in laikaPDF             := Artifact(moduleName.value, Artifact.DocType, "pdf"),
    artifactPath in laikaPackageSite := Settings.createArtifactPath(laikaPackageSite).value,
    artifactPath in laikaEPUB        := Settings.createArtifactPath(laikaEPUB).value,
    artifactPath in laikaPDF         := Settings.createArtifactPath(laikaPDF).value,

  ) :+ (cleanFiles += (target in Laika).value)

}
