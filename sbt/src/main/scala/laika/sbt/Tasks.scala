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
import laika.io.syntax.*
import laika.io.model.*
import laika.sbt.LaikaPlugin.autoImport.*
import sbt.Keys.*
import sbt.*
import sbt.util.CacheStore
import Settings.validated
import cats.data.NonEmptyChain
import laika.api.builder.{ OperationConfig, ParserBuilder }
import laika.api.config.Config
import laika.api.format.MarkupFormat
import laika.ast.OutputContext
import laika.ast.Path.Root
import laika.config.{ LaikaKeys, Selections, Versions }
import laika.io.config.{ BinaryRendererConfig, TextRendererConfig }
import laika.io.internal.config.SiteConfig
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

    val aliasMap = Map(
      "fo"            -> "xsl-fo",
      "xslfo"         -> "xsl-fo",
      "formatted-ast" -> "ast"
    ).withDefault(identity)

    val configMap = laikaRenderers.value.map { config =>
      config.alias -> config
    }.toMap

    val formats = spaceDelimited("<format>").parsed
    if (formats.isEmpty) throw new IllegalArgumentException("At least one format must be specified")

    val supportedFormats = (configMap.keys ++ aliasMap.keys).toList.sorted
    val invalid          = formats.diff(supportedFormats)
    if (invalid.nonEmpty)
      throw new IllegalArgumentException(
        s"Unsupported formats ${
            invalid.mkString(", ")
          } - supported are ${supportedFormats.mkString(", ")}"
      )

    val configs = formats.map(aliasMap.apply).map(configMap.apply)

    val userConfig = laikaConfig.value
    val baseConfig = Settings.parserConfig.value.baseConfig

    val siteTarget = (laikaSite / target).value

    lazy val tree = {
      val apiPath = validated(SiteConfig.apiPath(baseConfig))
      val parser  = Settings.parser.value
      val inputs  = generateAPI.value.foldLeft(laikaInputs.value.delegate) { (inputs, path) =>
        inputs.addProvidedPath(apiPath / path)
      }
      val tree    = parser.use(_.fromInput(inputs).parse).unsafeRunSync()

      Logs.runtimeMessages(streams.value.log, tree.root, userConfig.logLevel)

      tree
    }

    def prepareTree(
        treeProcessors: Seq[LaikaTreeProcessor],
        context: OutputContext
    ) = {
      val processor = treeProcessors
        .map(_.delegate.apply(context))
        .reduceOption(_.andThen(_))
      processor.fold(IO.pure(tree))(_.run(tree))
    }

    def renderText(config: TextRendererConfig): Set[File] = {

      val target =
        if (config.format == HTML) siteTarget
        else siteTarget / config.alias

      if (target.exists) cleanTarget(target, baseConfig)
      else target.mkdirs()
      val dirPath = FilePath.fromJavaFile(target)

      val op = prepareTree(laikaTreeProcessors.value, OutputContext(config.format))
        .flatMap { tree =>
          Renderer
            .of(config.format)
            .withConfig(Settings.parserConfig.value)
            .parallel[IO]
            .withTheme(laikaTheme.value)
            .build
            .use(
              _
                .from(tree)
                .toDirectory(dirPath)(userConfig.encoding)
                .render
            )
        }

      op.unsafeRunSync()

      streams.value.log.info(Logs.outputs(tree.root, config.alias))
      streams.value.log.info(s"Generated ${config.alias} in $target")

      target.allPaths.get().toSet.filter(_.isFile)
    }

    def renderBinary(config: BinaryRendererConfig): Set[File] = {

      val targetDirectory = siteTarget / config.artifact.fullPath.parent.toString
      targetDirectory.mkdirs()

      val currentVersion = tree.root.config.get[Versions].toOption.map(_.currentVersion)

      val ops = prepareTree(laikaTreeProcessors.value, OutputContext(config.format))
        .flatMap { tree =>
          Renderer
            .of(config.format)
            .withConfig(Settings.parserConfig.value)
            .parallel[IO]
            .withTheme(laikaTheme.value)
            .build
            .use { renderer =>
              val roots =
                if (config.supportsSeparations) validated(Selections.createCombinations(tree.root))
                else NonEmptyChain.one(tree.root -> Selections.Classifiers(Nil))
              roots.traverse { case (root, classifiers) =>
                val artifactPath = config.artifact.withClassifiers(classifiers.value).fullPath
                val isVersioned  =
                  currentVersion.isDefined &&
                  tree.root
                    .selectTreeConfig(artifactPath.parent)
                    .get[Boolean](LaikaKeys.versioned)
                    .getOrElse(false)
                val finalPath    =
                  if (isVersioned) Root / currentVersion.get.pathSegment / artifactPath.relative
                  else artifactPath
                val file         = siteTarget / finalPath.toString
                renderer
                  .from(root)
                  .copying(tree.staticDocuments)
                  .toFile(FilePath.fromJavaFile(file))
                  .render
                  .as(file)
              }
            }
        }

      val res = ops.unsafeRunSync()

      res.toList.foreach { f =>
        streams.value.log.info(s"Generated ${config.alias} in $f")
      }

      res.toList.toSet
    }

    val cacheDir        = streams.value.cacheDirectory / "laika"
    val inputCollection =
      laikaInputs.value.delegate.build(Settings.parserConfig.value.docTypeMatcher).unsafeRunSync()
    streams.value.log.info(Logs.inputs(inputCollection))
    val inputFiles      = collectInputFiles(inputCollection)

    val results = configs map { config =>
      val cacheFormatDir = config.alias.toLowerCase

      val fun =
        FileFunction.cached(cacheDir / cacheFormatDir, FilesInfo.lastModified, FilesInfo.exists) {
          _ =>
            config match {
              case c: TextRendererConfig   => renderText(c)
              case c: BinaryRendererConfig => renderBinary(c)
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

    val binaryFormats = laikaRenderers.value
      .collect { case bin: BinaryRendererConfig => bin }

    val config = ServerConfig.defaults
      .withArtifactBasename(name.value)
      .withHost(previewConfig.host)
      .withPort(previewConfig.port)
      .withPollInterval(previewConfig.pollInterval)
      .withBinaryRenderers(binaryFormats)

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
      s"Preview server started at http://${laikaPreviewConfig.value.host}:${
          laikaPreviewConfig.value.port
        }. Press return/enter to exit."
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
    val formats = laikaRenderers.value.filter(_.includeInSite).map(_.alias)
    generate.toTask(formats.mkString(" ", " ", ""))
  }

  val mappings: Initialize[Task[Seq[(File, String)]]] = task {
    sbt.Path.allSubpaths((laikaSite / target).value).toSeq
  }

  val describe: Initialize[Task[String]] = task {

    val userConfig = laikaConfig.value

    def mergedConfig(config: OperationConfig): OperationConfig = {
      config
        .withMessageFilters(userConfig.messageFilters)
        .withBundleFilter(userConfig.bundleFilter)
    }

    def createParser(format: MarkupFormat): ParserBuilder = {
      val parser = MarkupParser.of(format)
      parser.withConfig(mergedConfig(parser.config)).using(laikaExtensions.value *)
    }

    val transformer = Transformer
      .from(Markdown)
      .to(HTML)
      .withConfig(mergedConfig(createParser(Markdown).config))
      .using(laikaExtensions.value *)
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

    sbt.IO.delete(filesToDelete.get())
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

}
