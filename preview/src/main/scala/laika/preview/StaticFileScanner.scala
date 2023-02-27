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

import cats.syntax.all._
import cats.effect.Async
import fs2.io.file.Files
import laika.api.builder.OperationConfig
import laika.ast.Path
import laika.ast.Path.Root
import laika.config.ConfigException
import laika.io.config.SiteConfig
import laika.io.model.{ BinaryInput, FilePath }
import laika.io.runtime.DirectoryScanner
import laika.rewrite.Versions

private[preview] object StaticFileScanner {

  private def collect[F[_]: Async](
      filePath: FilePath,
      vPath: Path = Root
  ): F[List[(Path, SiteResult[F])]] = {
    DirectoryScanner.scanDirectory(filePath) { paths =>
      paths.toList
        .traverse { filePath =>
          val vChild                        = vPath / filePath.name
          def result: (Path, SiteResult[F]) =
            (vChild, StaticResult(BinaryInput.fromFile(filePath, vPath).input))
          Files[F].isDirectory(filePath.toFS2Path).ifM(
            collect(filePath, vChild),
            Async[F].pure(List(result))
          )
        }
        .map(_.flatten)
    }
  }

  def collectVersionedFiles[F[_]: Async](config: OperationConfig): F[Map[Path, SiteResult[F]]] = {

    def otherVersions(versions: Option[Versions]): F[List[(Path, SiteResult[F])]] = {
      (versions, versions.flatMap(_.scannerConfig)) match {
        case (Some(vs), Some(scanner)) =>
          val versionRoot = FilePath.parse(scanner.rootDirectory)
          (vs.olderVersions ++ vs.newerVersions)
            .toList
            .traverse { v =>
              collect(versionRoot / v.pathSegment, Root / v.pathSegment)
            }
            .map(_.flatten)
        case _                         => List.empty[(Path, SiteResult[F])].pure[F]
      }

    }

    for {
      versions <- Async[F].fromEither(
        config.baseConfig.getOpt[Versions].leftMap(ConfigException.apply)
      )
      files    <- otherVersions(versions)
    } yield files.toMap
  }

  def collectAPIFiles[F[_]: Async](
      config: OperationConfig,
      apiDir: FilePath
  ): F[Map[Path, SiteResult[F]]] =
    for {
      apiPath <- Async[F].fromEither(
        SiteConfig.apiPath(config.baseConfig).leftMap(ConfigException.apply)
      )
      files   <- collect(apiDir, apiPath)
    } yield files.toMap

}
