/*
 * Copyright 2013-2018 the original author or authors.
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

import laika.api.{Parse, Render}
import laika.api.config.BundleFilter
import laika.factory.{RenderResultProcessor, RendererFactory}
import laika.io.Input.LazyFileInput
import laika.io.{Input, InputTree}
import laika.parse.markdown.Markdown
import laika.parse.rst.ReStructuredText
import laika.render.{HTML, PDF, PrettyPrint, XSLFO}
import LaikaPlugin.autoImport._
import sbt._
import sbt.Keys._
import sbt.util.CacheStore

/** Implementations for Laika's sbt tasks.
  *
  * @author Jens Halm
  */
object Tasks {

  import Def._

  /** The main transformation task of the sbt plugin.
    *
    * It accepts one or more arguments specifying the output formats. Valid arguments
    * are `html`, `pdf`, `xsl-fo` and `ast`.
    *
    * The implementation parses the input document only once and then uses the AST
    * obtained from the parsing operation to feed all specified renderers.
    *
    * The implementation is slightly bulky due to the nature of the sbt macros
    * which would require any method being factored out to be tasks, too, which
    * is not always desirable.
    */
  val generate: Initialize[InputTask[Set[File]]] = inputTask {

    val formats = spaceDelimited("<format>").parsed.map(OutputFormat.fromString)
    if (formats.isEmpty) throw new IllegalArgumentException("At least one format must be specified")

    val parser = {
      val parser = Parse.as(Markdown).or(ReStructuredText)
      val userConfig = laikaConfig.value
      val mergedConfig = parser.config.copy(
        bundleFilter = BundleFilter(strict = userConfig.strict, acceptRawContent = userConfig.rawContent),
        parallel = userConfig.parallel,
        minMessageLevel = userConfig.renderMessageLevel
      )
      parser.withConfig(mergedConfig).using(laikaExtensions.value: _*)
    }

    val inputs = InputTree.forRootDirectories((sourceDirectories in Laika).value, parser.config.docTypeMatcher,
      (excludeFilter in Laika).value.accept)(laikaConfig.value.encoding)

    lazy val tree = {
      streams.value.log.info("Reading files from " + (sourceDirectories in Laika).value.mkString(", "))
      streams.value.log.info(Logs.inputs(inputs))

      val tree = parser.fromInputTree(inputs)

      Logs.systemMessages(streams.value.log, tree, laikaConfig.value.logMessageLevel)

      tree
    }

    def renderWithFactory[W] (factory: RendererFactory[W], targetDir: File, formatDesc: String): Set[File] = {
      val apiInSite = (target in laikaCopyAPI).value
      val pdfInSite = (artifactPath in laikaPDF).value
      val filesToDelete = (targetDir.allPaths --- targetDir --- pdfInSite --- apiInSite.allPaths --- collectParents(apiInSite)).get
      IO.delete(filesToDelete)

      if (!targetDir.exists) targetDir.mkdirs()

      Render.as(factory).withConfig(parser.config).from(tree).toDirectory(targetDir)(laikaConfig.value.encoding)

      streams.value.log.info(Logs.outputs(tree, formatDesc))
      streams.value.log.info(s"Generated $formatDesc in $targetDir")

      targetDir.allPaths.get.toSet.filter(_.isFile)
    }

    def renderWithProcessor[W] (processor: RenderResultProcessor[W], targetFile: File, formatDesc: String): Set[File] = {
      targetFile.getParentFile.mkdirs()

      val render = Render.as(processor).withConfig(parser.config)
      render from tree toFile targetFile

      streams.value.log.info(s"Generated $formatDesc in $targetFile")

      Set(targetFile)
    }

    val cacheDir = streams.value.cacheDirectory / "laika"
    val inputFiles = collectInputFiles(inputs)

    val results = formats map { format =>

      val fun = FileFunction.cached(cacheDir / format.toString.toLowerCase, FilesInfo.lastModified, FilesInfo.exists) { _ =>
        format match {
          case OutputFormat.HTML  => renderWithFactory(HTML, (target in laikaSite).value, "HTML")
          case OutputFormat.AST   => renderWithFactory(PrettyPrint, (target in laikaAST).value, "Formatted AST")
          case OutputFormat.XSLFO => renderWithFactory(XSLFO, (target in laikaXSLFO).value, "XSL-FO")
          case OutputFormat.PDF   => renderWithProcessor(PDF.withFopFactory(fopFactory.value), (artifactPath in laikaPDF).value, "PDF")
        }
      }
      fun(inputFiles)
    }

    val outputFiles = results reduce (_ ++ _)
    outputFiles intersect Settings.allTargets.value
  }

  /** The site task combines the html generator with optionally also
    * rendering a PDF document from the same input and creating scaladoc
    * documentation and copying both over to the target directory.
    *
    * PDF rendering and API generation is triggered by the `laikaIncludeAPI`
    * and `laikaIncludePDF` settings respectively.
    */
  val site: Initialize[Task[Set[File]]] = taskDyn {
    if (laikaIncludePDF.value) generate.toTask(" html pdf")
    else generate.toTask(" html")
  }

  /** Copies the API documentation and the rendered PDF file
    * to the target directory of the site task.
    */
  val copy: Initialize[Task[File]] = task {
    val api = laikaCopyAPI.value
    val pdf = laikaCopyPDF.value

    (target in laikaSite).value
  }

  /** Copies the API documentation to the target directory of the site task.
    * Does nothing if the `laikaIncludeAPI` setting is set to false (the default).
    */
  val copyAPI: Initialize[Task[File]] = taskDyn {
    val targetDir = (target in laikaCopyAPI).value
    if (laikaIncludeAPI.value) task {

      val cacheDir = streams.value.cacheDirectory / "laika" / "api"
      val apiMappings = (mappings in packageDoc in Compile).value
      val targetMappings = apiMappings map { case (file, target) => (file, targetDir / target) }

      Sync(CacheStore(cacheDir))(targetMappings)

      streams.value.log.info("Copied API documentation to " + targetDir)
      targetDir
    }
    else task {
      IO.delete(targetDir)
      targetDir
    }
  }

  /** Copies the rendered PDF document to the target directory of the site task.
    * Does nothing if the `laikaIncludePDF` setting is set to false (the default).
    */
  val copyPDF: Initialize[Task[File]] = taskDyn {
    val targetDir = (target in laikaSite).value
    val pdfSource = (artifactPath in laikaPDF).value
    val pdfTarget = targetDir / pdfSource.getName

    if (laikaIncludePDF.value) task {
      val cacheDir = streams.value.cacheDirectory / "laika" / "site-pdf"
      Sync(CacheStore(cacheDir))(Seq((pdfSource, pdfTarget)))

      streams.value.log.info("Copied PDF output to " + targetDir)
      targetDir
    }
    else task {
      IO.delete(pdfTarget)
      targetDir
    }
  }

  /** Packages the generated html site and (optionally) the included
    * API documentation and PDF file into a zip archive.
    */
  val packageSite: Initialize[Task[File]] = task {
    val zipFile = (artifactPath in laikaPackageSite).value
    streams.value.log.info(s"Packaging $zipFile ...")

    IO.zip((mappings in laikaSite).value, zipFile)

    streams.value.log.info("Done packaging.")
    zipFile
  }

  /** Cleans the target directory of the site task.
    */
  val clean: Initialize[Task[Unit]] = task {
    IO.delete((target in laikaSite).value)
  }

  /** Recursively collects all input files from the specified
    * input tree. Ignores any virtual files in the input trees.
    */
  def collectInputFiles (tree: InputTree): Set[File] = {
    def allFiles (inputs: Seq[Input]) = (inputs collect {
      case f: LazyFileInput => f.file
    }).toSet

    allFiles(tree.markupDocuments) ++
      allFiles(tree.dynamicDocuments) ++
      allFiles(tree.templates) ++
      allFiles(tree.configDocuments) ++
      allFiles(tree.staticDocuments) ++
      (tree.subtrees flatMap collectInputFiles)
  }

  /** Collects all parent directories of the specified file or directory.
    */
  def collectParents (file: File): Set[File] = {
    def collect (file: File, acc: Set[File]): Set[File] = {
      file.getParentFile match {
        case null => acc
        case p => collect(p, acc + p)
      }
    }
    collect(file, Set())
  }

  /** Enumeration of output formats supported by the plugin.
    */
  sealed trait OutputFormat

  object OutputFormat {

    case object HTML extends OutputFormat

    case object PDF extends OutputFormat

    case object XSLFO extends OutputFormat

    case object AST extends OutputFormat

    def fromString (name: String): OutputFormat = name.toLowerCase match {
      case "html" => HTML
      case "pdf" => PDF
      case "fo" | "xslfo" | "xsl-fo" => XSLFO
      case "formatted-ast" | "ast" => AST
      case _ => throw new IllegalArgumentException(s"Unsupported format: $name")
    }

  }

}
