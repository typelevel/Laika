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

package laika.io.runtime

import cats.data.NonEmptyChain
import cats.effect.kernel.Concurrent
import cats.effect.{Async, Sync}
import cats.syntax.all._
import laika.collection.TransitionalCollectionOps._
import laika.ast.DocumentType.{Ignored, Static}
import laika.ast.Path.Root
import laika.ast.{DocumentType, Path, SegmentedPath}
import laika.config.Config.ConfigResult
import laika.config.{ConfigDecoder, ConfigException, ConfigParser}
import laika.io.model.{BinaryInput, DirectoryInput}
import laika.rewrite.{VersionScannerConfig, Versions}

import java.io.File
import scala.io.Codec

private[runtime] object VersionedLinkTargets {

  private def scanTargetDirectory[F[_]: Async] (versions: Versions, config: VersionScannerConfig): F[Map[String, Seq[Path]]] = {
    val existingVersions = (versions.newerVersions ++ versions.olderVersions).map(_.pathSegment).toSet
    val excluded = config.exclude.flatMap { exclude =>
      existingVersions.toSeq.map(v => Root / v / exclude.relative)
    }.toSet

    def included (path: Path, segments: NonEmptyChain[String]): Boolean = {
      path.suffix.contains("html") &&
        existingVersions.contains(segments.head) &&
        segments.size > 1 &&
        !excluded.exists(ex => path.isSubPath(ex))
    }
    val docTypeMather: Path => DocumentType = {
      case p @ SegmentedPath(segments, _, _) if included(p, segments) => Static()
      case _ => Ignored
    }
    val input = DirectoryInput(Seq(new File(config.rootDirectory)), Codec.UTF8, docTypeMather)
    DirectoryScanner.scanDirectories(input).map { tree =>
      tree.binaryInputs
        .collect { case BinaryInput(path: SegmentedPath, _, _, _) if path.depth > 1 =>
          (path.segments.head, SegmentedPath(NonEmptyChain.fromChainUnsafe(path.segments.tail), path.suffix, None))
        }
        .groupBy(_._1)
        .mapValuesStrict(_.map(_._2))
    }
  }

  implicit private val versionInfoDecoder: ConfigDecoder[(Path, Seq[String])] = ConfigDecoder.config.flatMap { config =>
    for {
      path      <- config.get[Path]("path")
      versions  <- config.get[Seq[String]]("versions")
    } yield {
      (path, versions)
    }
  }
  
  private def loadVersionInfo[F[_]: Concurrent] (existing: BinaryInput[F]): F[Map[String, Seq[Path]]] = {
    def asSync[T](res: ConfigResult[T]): F[T] = Concurrent[F].fromEither(res.leftMap(ConfigException.apply))
    
    existing.input
      .through(fs2.text.utf8.decode)
      .compile
      .string
      .flatMap(json => asSync(ConfigParser.parse(json).resolve()))
      .flatMap(config => asSync(config.get[Seq[(Path, Seq[String])]]("linkTargets")))
      .map { _
        .flatMap { case (path, versions) =>
          versions.map((_, path))
        }
        .groupBy(_._1)
        .mapValuesStrict(_.map(_._2))
      }
  }
  
  def gatherTargets[F[_]: Async] (versions: Versions, staticDocs: Seq[BinaryInput[F]]): F[Map[String, Seq[Path]]] =
    (staticDocs.find(_.path == VersionInfoGenerator.path), versions.scannerConfig) match {
      case (Some(info), _)   => loadVersionInfo(info)
      case (_, Some(config)) => scanTargetDirectory(versions, config)
      case _                 => Sync[F].pure(Map.empty)
    }

  def groupLinkTargets (versions: Versions,
                        currentVersion: Seq[Path],
                        existingVersions: Map[String, Seq[Path]]): Seq[VersionedDocument] = {

    val map = existingVersions + (versions.currentVersion.pathSegment -> currentVersion)
    map.toSeq
      .flatMap { case (version, paths) =>
        paths.map(path => (path, version))
      }
      .groupBy(_._1)
      .map { case (path, pairs) => VersionedDocument(path, pairs.map(_._2)) }
      .toSeq
  }

  case class VersionedDocument (path: Path, versions: Seq[String])

}
