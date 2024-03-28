/*
 * Copyright 2012-2021 the original author or authors.
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

package laika.preview.internal

import cats.data.NonEmptyChain
import cats.effect.syntax.all.*
import cats.effect.{ Async, Resource }
import cats.syntax.all.*
import fs2.Chunk
import laika.api.Renderer
import laika.api.builder.OperationConfig
import laika.api.config.Config.ConfigResult
import laika.api.format.{ BinaryPostProcessor, TwoPhaseRenderFormat }
import laika.ast.Path
import laika.ast.Path.Root
import laika.config.{ LaikaKeys, MessageFilters, Selections, TargetFormats, Versions }
import laika.format.HTML
import laika.io.api.{ BinaryTreeRenderer, TreeParser, TreeRenderer }
import laika.io.config.{ Artifact, BinaryRendererConfig }
import laika.io.internal.config.SiteConfig
import laika.io.internal.errors.ConfigException
import laika.io.model.*
import laika.io.syntax.*
import laika.preview.internal.SiteTransformer.{ BinaryRendererSetup, ResultMap }
import laika.theme.Theme

import java.io.ByteArrayOutputStream

private[preview] class SiteTransformer[F[_]: Async](
    val parser: TreeParser[F],
    htmlRenderer: TreeRenderer[F],
    astRenderer: TreeRenderer[F],
    binaryRenderers: Seq[BinaryRendererSetup[F]],
    inputs: InputTreeBuilder[F],
    staticFiles: ResultMap[F],
    artifactBasename: String
) {

  private val parse = parser.fromInput(inputs).parse

  def renderBinary(renderer: BinaryTreeRenderer[F], tree: ParsedTree[F]): fs2.Stream[F, Byte] = {
    val renderResult = for {
      out <- Async[F].delay(new ByteArrayOutputStream(1024 * 64))
      _   <- renderer.from(tree).toStream(Async[F].pure(out)).render
    } yield Chunk.array(out.toByteArray)

    fs2.Stream.evalUnChunk(renderResult)
  }

  def transformBinaries(tree: ParsedTree[F]): ConfigResult[ResultMap[F]] = {
    val unseparated = NonEmptyChain.one((tree.root, Selections.Classifiers(Nil)))
    for {
      roots            <- Selections.createCombinations(tree.root)
      downloadPath     <- SiteConfig.downloadPath(tree.root.config)
      artifactBaseName <- tree.root.config.get[String](LaikaKeys.artifactBaseName, artifactBasename)
    } yield {

      val fallbackArtifactBase = downloadPath / artifactBaseName
      val combinations         = for {
        renderer <- binaryRenderers
        root     <- (if (renderer.supportsSeparations) roots else unseparated).toList
      } yield (root, renderer)

      val currentVersion = tree.root.config.get[Versions].toOption.map(_.currentVersion)

      combinations.map { case ((root, classifiers), rendererSetup) =>
        val path        =
          rendererSetup.artifact(fallbackArtifactBase).withClassifiers(classifiers.value).fullPath
        val isVersioned =
          currentVersion.isDefined &&
          tree.root.selectTreeConfig(path.parent).get[Boolean](LaikaKeys.versioned).getOrElse(false)
        val servedPath  =
          if (isVersioned) Root / currentVersion.get.pathSegment / path.relative else path
        val finalTree   = ParsedTree(root).addStaticDocuments(tree.staticDocuments)
        (servedPath, StaticResult(renderBinary(rendererSetup.renderer, finalTree)))
      }.toMap
    }
  }

  def transformHTML(tree: ParsedTree[F], renderer: TreeRenderer[F]): F[ResultMap[F]] = {
    renderer
      .from(tree)
      .toMemory
      .render
      .map { root =>
        val map: Map[Path, SiteResult[F]]   = root.allDocuments.map { doc =>
          (doc.path, RenderedResult[F](doc.content))
        }.toMap ++
          root.staticDocuments.map { doc =>
            (doc.path, StaticResult(doc.input))
          }.toMap
        val roots: Map[Path, SiteResult[F]] = map.flatMap { case (path, result) =>
          if (path.name == "index.html") Some((path.parent, result)) else None
        }
        map ++ roots
      }
  }

  private def transformASTLazily(tree: ParsedTree[F], html: ResultMap[F]): F[ResultMap[F]] = {

    val transformer = for {
      modifiedRoot <- Async[F].delay(
        tree.modifyRoot(ASTPageTransformer.transform(_, parser.config))
      )
      resultMap    <- transformHTML(modifiedRoot, astRenderer)
    } yield resultMap

    def buildLazyMap(delegate: F[ResultMap[F]]): ResultMap[F] = {
      html.keySet
        .map(ASTPageTransformer.ASTPathTranslator.translateASTPath)
        .map { astPath =>
          val result = LazyResult(delegate.map(_.get(astPath)))
          (astPath, result: SiteResult[F])
        }
        .toMap
    }

    transformer.memoize.map(buildLazyMap)
  }

  val transform: F[SiteResults[F]] = for {
    tree   <- parse
    html   <- transformHTML(tree, htmlRenderer)
    ast    <- transformASTLazily(tree, html)
    ebooks <- Async[F].fromEither(transformBinaries(tree).leftMap(ConfigException.apply))
  } yield {
    new SiteResults(staticFiles ++ ast ++ html ++ ebooks)
  }

}

private[preview] object SiteTransformer {

  private[preview] class BinaryRendererSetup[F[_]](
      val artifact: Path => Artifact,
      val supportsSeparations: Boolean,
      val renderer: BinaryTreeRenderer[F]
  )

  type ResultMap[F[_]] = Map[Path, SiteResult[F]]

  def htmlRenderer[F[_]: Async](
      config: OperationConfig,
      theme: Theme[F]
  ): Resource[F, TreeRenderer[F]] =
    Renderer
      .of(HTML)
      .withConfig(config)
      .parallel[F]
      .withTheme(theme)
      .build

  def binaryRenderer[F[_]: Async](
      format: TwoPhaseRenderFormat[?, BinaryPostProcessor.Builder],
      opConfig: OperationConfig,
      theme: Theme[F],
      artifact: Path => Artifact,
      supportsSeparations: Boolean
  ): Resource[F, BinaryRendererSetup[F]] = {
    val renderer = Renderer
      .of(format)
      .withConfig(opConfig)
      .parallel[F]
      .withTheme(theme)
      .build
    renderer.map(new BinaryRendererSetup(artifact, supportsSeparations, _))
  }

  def create[F[_]: Async](
      parser: Resource[F, TreeParser[F]],
      inputs: InputTreeBuilder[F],
      builtinFormats: List[TwoPhaseRenderFormat[?, BinaryPostProcessor.Builder]],
      extraFormats: List[BinaryRendererConfig],
      apiDir: Option[FilePath],
      artifactBasename: String
  ): Resource[F, SiteTransformer[F]] = {

    def adjustConfig(p: TreeParser[F]): TreeParser[F] = p.modifyConfig {
      _
        .withMessageFilters(MessageFilters.forVisualDebugging)
        .withConfigValue(LaikaKeys.preview.enabled, true)
    }

    def asInputTree(map: ResultMap[F]): InputTree[F] = {
      val inputs = map.collect { case (path, static: StaticResult[F]) =>
        BinaryInput.fromStream(static.content, path, TargetFormats.Selected("html"))
      }
      new InputTree[F](binaryInputs = inputs.toSeq)
    }

    def collectAPIFiles(config: OperationConfig): Resource[F, ResultMap[F]] =
      apiDir.fold(Resource.pure[F, ResultMap[F]](Map.empty)) { dir =>
        Resource.eval(StaticFileScanner.collectAPIFiles(config, dir))
      }

    def artifact(format: TwoPhaseRenderFormat[?, BinaryPostProcessor.Builder]): Path => Artifact =
      path => Artifact(path, format.description.toLowerCase)

    for {
      p    <- parser.map(adjustConfig)
      html <- htmlRenderer(p.config, p.theme)
      ast  <- htmlRenderer(p.config.withBundles(Seq(ASTPageTransformer.ASTPathTranslator)), p.theme)
      bin1 <- builtinFormats.traverse(fmt =>
        binaryRenderer(fmt, p.config, p.theme, artifact(fmt), supportsSeparations = true)
      )
      bin2 <- extraFormats.traverse(c =>
        binaryRenderer(c.format, p.config, p.theme, _ => c.artifact, c.supportsSeparations)
      )
      vFiles   <- Resource.eval(StaticFileScanner.collectVersionedFiles(p.config))
      apiFiles <- collectAPIFiles(p.config)
    } yield {
      val allInputs = inputs.merge(asInputTree(apiFiles))
      new SiteTransformer[F](p, html, ast, bin1 ++ bin2, allInputs, vFiles, artifactBasename)
    }

  }

}

private[preview] class SiteResults[F[_]](map: Map[Path, SiteResult[F]]) {

  def get(path: Path): Option[SiteResult[F]] = map.get(path)

  def list: List[Path] = map.keySet.toList.sortBy(_.toString)

}

private[preview] sealed abstract class SiteResult[F[_]] extends Product with Serializable

private[preview] case class RenderedResult[F[_]](content: String) extends SiteResult[F]

private[preview] case class StaticResult[F[_]](content: fs2.Stream[F, Byte])
    extends SiteResult[F]

private[preview] case class LazyResult[F[_]](result: F[Option[SiteResult[F]]])
    extends SiteResult[F]
