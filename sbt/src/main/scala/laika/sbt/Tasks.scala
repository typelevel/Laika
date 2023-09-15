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

import cats.effect.{ IO, Resource }
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import laika.api.{ MarkupParser, Renderer, Transformer }
import laika.format.*
import laika.io.config.SiteConfig
import laika.io.implicits.*
import laika.io.model.*
import laika.rewrite.Versions
import laika.rewrite.nav.Selections
import laika.sbt.LaikaPlugin.autoImport.*
import sbt.Keys.*
import sbt.*
import sbt.util.CacheStore
import Settings.validated
import laika.api.builder.{ OperationConfig, ParserBuilder }
import laika.api.format.{
  BinaryPostProcessorBuilder,
  MarkupFormat,
  RenderFormat,
  TwoPhaseRenderFormat
}
import laika.config.Config
import laika.preview.{ ServerBuilder, ServerConfig }
import org.http4s.server.Server

import scala.annotation.tailrec

/** Implementations for Laika's sbt tasks.
  *
  * @author Jens Halm
  */
object Tasks {

  import Def.*

  /** Generates and copies the API documentation to the target directory of the site task.
    * Does nothing if the `laikaIncludeAPI` setting is set to false (the default).
    */
  val generateAPI: Initialize[Task[Seq[String]]] = taskDyn {
    val targetDir = Settings.apiTargetDirectory.value
    if (laikaIncludeAPI.value) task {

      val cacheDir       = streams.value.cacheDirectory / "laika" / "api"
      val apiMappings    = (laikaGenerateAPI / sbt.Keys.mappings).value
      val targetMappings = apiMappings map { case (file, name) => (file, targetDir / name) }

      Sync.sync(CacheStore(cacheDir))(targetMappings)

      streams.value.log.info("Copied API documentation to " + targetDir)
      apiMappings.map(_._2)
    }
    else
      task {
        streams.value.log.info("Delete API dir")
        sbt.IO.delete(targetDir)
        Nil
      }
  }

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

    val userConfig       = laikaConfig.value
    val parser           = Settings.parser.value
    val baseConfig       = Settings.parserConfig.value.baseConfig
    val downloadPath     =
      (laikaSite / target).value / validated(SiteConfig.downloadPath(baseConfig)).relative.toString
    val artifactBaseName = Settings.artifactBaseName.value

    lazy val tree = {
      val apiPath = validated(SiteConfig.apiPath(baseConfig))
      val inputs  = generateAPI.value.foldLeft(laikaInputs.value.delegate) { (inputs, path) =>
        inputs.addProvidedPath(apiPath / path)
      }
      val tree    = parser.use(_.fromInput(inputs).parse).unsafeRunSync()

      Logs.runtimeMessages(streams.value.log, tree.root, userConfig.logMessages)

      tree
    }

    def renderWithFormat[FMT](
        format: RenderFormat[FMT],
        targetDir: File,
        formatDesc: String
    ): Set[File] = {

      if (targetDir.exists) cleanTarget(targetDir, baseConfig)
      else targetDir.mkdirs()

      Renderer
        .of(format)
        .withConfig(Settings.parserConfig.value)
        .parallel[IO]
        .withTheme(laikaTheme.value)
        .build
        .use(
          _
            .from(tree)
            .toDirectory(FilePath.fromJavaFile(targetDir))(userConfig.encoding)
            .render
        )
        .unsafeRunSync()

      streams.value.log.info(Logs.outputs(tree.root, formatDesc))
      streams.value.log.info(s"Generated $formatDesc in $targetDir")

      targetDir.allPaths.get.toSet.filter(_.isFile)
    }

    def renderWithProcessor[FMT](
        format: TwoPhaseRenderFormat[FMT, BinaryPostProcessorBuilder],
        formatDesc: String
    ): Set[File] = {

      downloadPath.mkdirs()

      val ops = Renderer
        .of(format)
        .withConfig(Settings.parserConfig.value)
        .parallel[IO]
        .withTheme(laikaTheme.value)
        .build
        .use { renderer =>
          val roots = validated(Selections.createCombinations(tree.root))
          roots.traverse { case (root, classifiers) =>
            val classifier =
              if (classifiers.value.isEmpty) "" else "-" + classifiers.value.mkString("-")
            val docName    = artifactBaseName + classifier + "." + formatDesc.toLowerCase
            val file       = downloadPath / docName
            renderer
              .from(root)
              .copying(tree.staticDocuments)
              .toFile(FilePath.fromJavaFile(file))
              .render
              .as(file)
          }
        }

      val res = ops.unsafeRunSync()

      res.toList.foreach { f =>
        streams.value.log.info(s"Generated $formatDesc in $f")
      }

      res.toList.toSet
    }

    val cacheDir        = streams.value.cacheDirectory / "laika"
    val inputCollection =
      laikaInputs.value.delegate.build(Settings.parserConfig.value.docTypeMatcher).unsafeRunSync()
    streams.value.log.info(Logs.inputs(inputCollection))
    val inputFiles      = collectInputFiles(inputCollection)

    val results = formats map { format =>
      val cacheFormatDir = format.toString.toLowerCase

      val fun =
        FileFunction.cached(cacheDir / cacheFormatDir, FilesInfo.lastModified, FilesInfo.exists) {
          _ =>
            format match {
              case OutputFormat.HTML  => renderWithFormat(HTML, (laikaSite / target).value, "HTML")
              case OutputFormat.AST   =>
                renderWithFormat(AST, (laikaAST / target).value, "Formatted AST")
              case OutputFormat.XSLFO =>
                renderWithFormat(XSLFO, (laikaXSLFO / target).value, "XSL-FO")
              case OutputFormat.EPUB  => renderWithProcessor(EPUB, "EPUB")
              case OutputFormat.PDF   => renderWithProcessor(PDF, "PDF")
            }
        }
      fun(inputFiles)
    }

    val outputFiles = results.reduce(_ ++ _)
    outputFiles.intersect(Settings.allTargets.value)
  }

  /** Creates a preview server as a `cats.effect.Resource` based on parser and input settings.
    */
  val buildPreviewServer: Initialize[Task[Resource[IO, Server]]] = task {

    val logger = streams.value.log
    logger.info("Initializing server...")

    def applyIf(flag: Boolean, f: ServerConfig => ServerConfig): ServerConfig => ServerConfig =
      if (flag) f else identity

    val previewConfig = laikaPreviewConfig.value

    val applyFlags = applyIf(laikaIncludeEPUB.value, _.withEPUBDownloads)
      .andThen(applyIf(laikaIncludePDF.value, _.withPDFDownloads))
      .andThen(
        applyIf(
          laikaIncludeAPI.value,
          _.withAPIDirectory(FilePath.fromJavaFile(Settings.apiTargetDirectory.value))
        )
      )
      .andThen(applyIf(previewConfig.isVerbose, _.verbose))

    val config = ServerConfig.defaults
      .withArtifactBasename(name.value)
      .withHost(previewConfig.host)
      .withPort(previewConfig.port)
      .withPollInterval(previewConfig.pollInterval)

    ServerBuilder[IO](Settings.parser.value, laikaInputs.value.delegate)
      .withLogger(s => IO(logger.info(s)))
      .withConfig(applyFlags(config))
      .build

  }

  /** Launches an HTTP server for the generated site and e-books, auto-refreshing when inputs change.
    * The server can be stopped with return/enter keys.
    */
  val startPreviewServer: Initialize[Task[Unit]] = task {

    val _ = generateAPI.value

    val (_, cancel) = buildPreviewServer.value.allocated.unsafeRunSync()

    streams.value.log.info(
      s"Preview server started at http://${laikaPreviewConfig.value.host}:${laikaPreviewConfig.value.port}. Press return/enter to exit."
    )

    try {
      System.in.read
    }
    finally {
      streams.value.log.info(s"Shutting down preview server...")
      cancel.unsafeRunSync()
    }
  }

  /** The site task combines the html generator with optionally also
    * rendering a PDF document from the same input and creating scaladoc
    * documentation and copying both over to the target directory.
    *
    * PDF rendering and API generation is triggered by the `laikaIncludeAPI`
    * and `laikaIncludePDF` settings respectively.
    */
  val site: Initialize[Task[Set[File]]] = taskDyn {
    val epub = if (laikaIncludeEPUB.value) " epub" else ""
    val pdf  = if (laikaIncludePDF.value) " pdf" else ""
    generate.toTask(" html" + epub + pdf)
  }

  val mappings: Initialize[Task[Seq[(File, String)]]] = task {
    sbt.Path.allSubpaths((laikaSite / target).value).toSeq
  }

  val describe: Initialize[Task[String]] = task {

    val userConfig = laikaConfig.value

    def mergedConfig(config: OperationConfig): OperationConfig = {
      config
        .withMessageFilters(
          render = userConfig.renderMessages,
          failOn = userConfig.failOnMessages
        )
        .withBundleFilter(userConfig.bundleFilter)
    }

    def createParser(format: MarkupFormat): ParserBuilder = {
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
      .use(
        _
          .fromInput(inputs)
          .toDirectory(FilePath.fromJavaFile((laikaSite / target).value))
          .describe
      )
      .unsafeRunSync()
      .withRendererDescription("Depending on task")
      .formatted

    streams.value.log.success("\n" + result)

    result
  }

  /** Packages the generated html site and (optionally) the included
    * API documentation and PDF/EPUB files into a zip archive.
    */
  val packageSite: Initialize[Task[File]] = task {
    val artifactName = Settings.artifactBaseName.value + ".zip"
    val zipFile      = (Laika / target).value / artifactName
    streams.value.log.info(s"Packaging $zipFile ...")

    sbt.IO.zip((laikaSite / sbt.Keys.mappings).value, zipFile, None)

    streams.value.log.info("Done packaging.")
    zipFile
  }

  /** Cleans the target directory of the site task.
    */
  val clean: Initialize[Task[Unit]] = task {
    cleanTarget((laikaSite / target).value, Settings.parserConfig.value.baseConfig)
  }

  private def cleanTarget(targetDir: File, config: Config): Unit = {
    val downloadPath   = targetDir / validated(SiteConfig.downloadPath(config)).relative.toString
    val apiPath        = targetDir / validated(SiteConfig.apiPath(config)).relative.toString
    val versionedPaths = config.get[Versions].toOption.map { versions =>
      (versions.olderVersions ++ versions.newerVersions).map(v =>
        new File(targetDir, v.pathSegment)
      ).allPaths
    }.reduceLeftOption(_ +++ _).getOrElse(PathFinder.empty)

    val filesToDelete = targetDir.allPaths --- targetDir ---
      versionedPaths ---
      pathFinderWithParents(downloadPath) ---
      pathFinderWithParents(apiPath)

    sbt.IO.delete(filesToDelete.get)
  }

  private def pathFinderWithParents(dir: File): PathFinder = {

    @tailrec
    def collect(file: File, acc: Set[File]): Set[File] = {
      file.getParentFile match {
        case null => acc
        case p    => collect(p, acc + p)
      }
    }

    dir.allPaths +++ collect(dir, Set())
  }

  /** Collects all input files from the specified input tree.
    * Ignores any virtual inputs in the input trees.
    */
  private def collectInputFiles(inputs: InputTree[IO]): Set[File] =
    inputs.textInputs.flatMap(_.sourceFile.map(_.toJavaFile)).toSet ++
      inputs.binaryInputs.flatMap(_.sourceFile.map(_.toJavaFile))

  /** Enumeration of output formats supported by the plugin.
    */
  private sealed trait OutputFormat

  private object OutputFormat {

    case object HTML extends OutputFormat

    case object EPUB extends OutputFormat

    case object PDF extends OutputFormat

    case object XSLFO extends OutputFormat

    case object AST extends OutputFormat

    def fromString(name: String): OutputFormat = name.toLowerCase match {
      case "html"                    => HTML
      case "epub"                    => EPUB
      case "pdf"                     => PDF
      case "fo" | "xslfo" | "xsl-fo" => XSLFO
      case "formatted-ast" | "ast"   => AST
      case _ => throw new IllegalArgumentException(s"Unsupported format: $name")
    }

  }

}
