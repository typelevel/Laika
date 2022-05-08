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

package laika.preview

import cats.effect.{Async, Resource}
import cats.syntax.all._
import fs2.Chunk
import laika.api.Renderer
import laika.api.builder.OperationConfig
import laika.ast.{MessageFilter, Path}
import laika.config.Config.ConfigResult
import laika.config.{ConfigException, LaikaKeys}
import laika.factory.{BinaryPostProcessorBuilder, TwoPhaseRenderFormat}
import laika.format.HTML
import laika.io.api.{BinaryTreeRenderer, TreeParser, TreeRenderer}
import laika.io.config.SiteConfig
import laika.io.implicits._
import laika.io.model._
import laika.preview.SiteTransformer.ResultMap
import laika.rewrite.nav.{Selections, TargetFormats}
import laika.theme.Theme

import java.io.ByteArrayOutputStream

private [preview] class SiteTransformer[F[_]: Async] (val parser: TreeParser[F], 
                                                      htmlRenderer: TreeRenderer[F],
                                                      binaryRenderers: Seq[(BinaryTreeRenderer[F], String)],
                                                      inputs: InputTreeBuilder[F],
                                                      staticFiles: ResultMap[F],
                                                      artifactBasename: String) {

  private val parse = parser.fromInput(inputs).parse

  def renderBinary (renderer: BinaryTreeRenderer[F], tree: ParsedTree[F]): fs2.Stream[F, Byte] = {
    val renderResult = for {
      out <- Async[F].delay(new ByteArrayOutputStream(1024 * 64))
      _   <- renderer.from(tree).toStream(Async[F].pure(out)).render
    } yield Chunk.array(out.toByteArray)
    
    fs2.Stream.evalUnChunk(renderResult)
  }
  
  def transformBinaries (tree: ParsedTree[F]): ConfigResult[ResultMap[F]] = {
    for {
      roots            <- Selections.createCombinations(tree.root)
      downloadPath     <- SiteConfig.downloadPath(tree.root.config)
      artifactBaseName <- tree.root.config.get[String](LaikaKeys.artifactBaseName, artifactBasename)
    } yield {
      val combinations = for {
        root <- roots.toList
        renderer <- binaryRenderers
      } yield (root, renderer)
      combinations.map { case ((root, classifiers), (renderer, suffix)) =>
        val classifier = if (classifiers.value.isEmpty) "" else "-" + classifiers.value.mkString("-")
        val docName = artifactBaseName + classifier + "." + suffix
        val path = downloadPath / docName
        (path, StaticResult(renderBinary(renderer, tree.copy(root = root))))
      }.toMap
    }
  }

  def transformHTML (tree: ParsedTree[F]): F[ResultMap[F]] = {
    htmlRenderer
      .from(tree)
      .toOutput(StringTreeOutput)
      .render
      .map { root =>
        val map = root.allDocuments.map { doc =>
          (doc.path, RenderedResult[F](doc.content))
        }.toMap ++
        root.staticDocuments.map { doc =>
          (doc.path, StaticResult(doc.input))
        }.toMap
        val roots = map.flatMap { case (path, result) =>
          if (path.name == "index.html") Some((path.parent, result)) else None 
        }
        map ++ roots
      }
  }

  val transform: F[SiteResults[F]] = for { 
    tree     <- parse
    rendered <- transformHTML(tree)
    ebooks   <- Async[F].fromEither(transformBinaries(tree).leftMap(ConfigException.apply))
  } yield {
    new SiteResults(staticFiles ++ rendered ++ ebooks)
  }
  
}

private [preview] object SiteTransformer {

  type ResultMap[F[_]] = Map[Path, SiteResult[F]]
  
  def htmlRenderer[F[_]: Async] (config: OperationConfig, 
                                 theme: Theme[F]): Resource[F, TreeRenderer[F]] = 
    Renderer
      .of(HTML)
      .withConfig(config)
      .renderMessages(MessageFilter.Info)
      .parallel[F]
      .withTheme(theme)
      .build
  
  def binaryRenderer[F[_]: Async, FMT] (format: TwoPhaseRenderFormat[FMT, BinaryPostProcessorBuilder],
                                        config: OperationConfig,
                                        theme: Theme[F]): Resource[F, BinaryTreeRenderer[F]] = {
    Renderer
      .of(format)
      .withConfig(config)
      .renderMessages(MessageFilter.Info)
      .parallel[F]
      .withTheme(theme)
      .build
  }

  def create[F[_]: Async](parser: Resource[F, TreeParser[F]],
                          inputs: InputTreeBuilder[F],
                          renderFormats: List[TwoPhaseRenderFormat[_, BinaryPostProcessorBuilder]],
                          apiDir: Option[FilePath],
                          artifactBasename: String): Resource[F, SiteTransformer[F]] = {
    
    def adjustConfig (p: TreeParser[F]): TreeParser[F] = p.modifyConfig(oc => oc.copy(
      failOnMessages = MessageFilter.None,
      configBuilder = oc.configBuilder
        .withValue(LaikaKeys.preview.enabled, true)
    ))
    
    def asInputTree (map: ResultMap[F]): InputTree[F] = {
      val inputs = map.collect {
        case (path, static: StaticResult[F]) => BinaryInput(static.content, path, TargetFormats.Selected("html")) 
      }
      new InputTree[F](binaryInputs = inputs.toSeq)
    }

    def collectAPIFiles (config: OperationConfig): Resource[F, ResultMap[F]] =
      apiDir.fold(Resource.pure[F, ResultMap[F]](Map.empty)) { dir =>
        Resource.eval(StaticFileScanner.collectAPIFiles(config, dir))
      }
    
    for {
      p        <- parser.map(adjustConfig)
      html     <- htmlRenderer(p.config, p.theme)
      bin      <- renderFormats.map(f => binaryRenderer(f, p.config, p.theme).map((_, f.description.toLowerCase))).sequence
      vFiles   <- Resource.eval(StaticFileScanner.collectVersionedFiles(p.config))
      apiFiles <- collectAPIFiles(p.config)
    } yield {
      val allInputs = inputs.merge(asInputTree(apiFiles))
      new SiteTransformer[F](p, html, bin, allInputs, vFiles, artifactBasename)
    }
    
  }
  
}

class SiteResults[F[_]: Async] (map: Map[Path, SiteResult[F]]) {
  
  def get (path: Path): Option[SiteResult[F]] = map.get(path)
  
  def list: List[Path] = map.keySet.toList.sortBy(_.toString)
  
}

sealed abstract class SiteResult[F[_]: Async] extends Product with Serializable
case class RenderedResult[F[_]: Async] (content: String) extends SiteResult[F]
case class StaticResult[F[_]: Async] (content: fs2.Stream[F, Byte]) extends SiteResult[F]
