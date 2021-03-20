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
import cats.effect.Sync
import cats.syntax.all._
import laika.collection.TransitionalCollectionOps._
import laika.ast.DocumentType.{Ignored, Static}
import laika.ast.Path.Root
import laika.ast.{DocumentType, Path, SegmentedPath}
import laika.io.model.{BinaryInput, DirectoryInput, DirectoryOutput}
import laika.rewrite.Versions

private[runtime] object VersionedLinkTargets {

  def scanExistingVersions[F[_]: Sync] (versions: Versions, output: DirectoryOutput): F[Map[String, Seq[Path]]] = {
    val existingVersions = (versions.newerVersions ++ versions.olderVersions).map(_.pathSegment).toSet
    val excluded = versions.excludeFromScanning.flatMap { exclude =>
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
    val input = DirectoryInput(Seq(output.directory), output.codec, docTypeMather)
    DirectoryScanner.scanDirectories(input).map { tree =>
      tree.binaryInputs
        .collect { case BinaryInput(path: SegmentedPath, _, _, _) => 
          (path.segments.head, SegmentedPath(NonEmptyChain.fromChainUnsafe(path.segments.tail), path.suffix, None)) 
        }
        .groupBy(_._1)
        .mapValuesStrict(_.map(_._2))
      }
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
