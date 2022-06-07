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

import java.nio.file.{FileSystems, WatchEvent, WatchKey, WatchService}
import java.nio.file.StandardWatchEventKinds._
import laika.ast.{DocumentType, Path}
import cats.syntax.all._

import scala.concurrent.duration._
import cats.effect.{Async, Ref, Resource, Temporal}
import fs2.io.file.Files
import laika.ast.DocumentType.Static
import laika.collection.TransitionalCollectionOps.{JIteratorWrapper, TransitionalMapOps}
import laika.io.model.{FileFilter, FilePath}
import laika.io.runtime.DirectoryScanner
import laika.preview.SourceChangeWatcher.{ObservedDirectory, ObservedFiles, ObservedTarget}

import scala.annotation.tailrec

private [preview] class SourceChangeWatcher[F[_]: Async] (service: WatchService,
                                                          targetMap: Ref[F, Map[WatchKey, ObservedTarget]],
                                                          inputs: List[FilePath],
                                                          update: F[Unit],
                                                          fileFilter: FileFilter,
                                                          docTypeMatcher: Path => DocumentType) {
  
  private def scanDirectory (dir: FilePath): F[List[FilePath]] = DirectoryScanner.scanDirectory[F, List[FilePath]](dir) {
    _.toList
      .traverse { childPath =>
        (Files[F].isDirectory(childPath.toFS2Path), fileFilter.filter(childPath)).tupled
          .flatMap { case (isDir, exclude) => 
            if (isDir && !exclude) scanDirectory(childPath) else List.empty[FilePath].pure[F]
          }
      }
      .map(_.flatten :+ dir)
  }
  
  def registerRoot (file: FilePath, children: List[FilePath] = Nil): F[List[ObservedTarget]] = {
    
    def registerDirectory (dir: FilePath, children: List[FilePath] = Nil): F[ObservedTarget] = {
      Async[F]
        .delay(dir.toNioPath.register(service, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY))
        .map { key =>
          if (children.isEmpty) ObservedDirectory(dir, key, fileFilter, docTypeMatcher)
          else ObservedFiles(dir, key, children.toSet, docTypeMatcher)
        }
    }
    
    if (children.isEmpty) scanDirectory(file).flatMap(_.traverse(registerDirectory(_)))
    else registerDirectory(file, children).map(List(_))
  }
  
  private def updateTargetMap (targets: List[List[ObservedTarget]]): F[Unit] = {
    val initTargetMap = targets.flatten.map(t => (t.key, t)).toMap
    targetMap.update(_ ++ initTargetMap)
  }
  
  val init: F[Unit] = {
    
    val groupedInputs = inputs
      .traverse { input =>
        Files[F].isDirectory(input.toFS2Path).map(
          if (_) (input, None) 
          else (FilePath.fromNioPath(input.toNioPath.getParent), Some(input))
        ) 
      }
      .map(_
        .groupBy(_._1)
        .mapValuesStrict(_.flatMap(_._2))
      )

    groupedInputs
      .flatMap {
        _.map {
          case (parent, children) => registerRoot(parent, children)
        }
        .toList
        .sequence
        .flatMap(updateTargetMap)
      }
  }

  @tailrec
  private def collectEvents (acc: List[(WatchEvent[_], WatchKey)]): List[(WatchEvent[_], WatchKey)] = {
    val key = service.poll()
    if (key == null) acc
    else {
      val events = JIteratorWrapper(key.pollEvents().iterator()).toList
      key.reset()
      collectEvents(acc ++ events.map((_, key)))
    }
  }
  
  private case class ProcessedEvent(triggersUpdate: Boolean, newDirectory: Option[FilePath])
  
  private def processEvent (event: WatchEvent[_], watchKey: WatchKey, targets: Map[WatchKey, ObservedTarget]): F[ProcessedEvent] = {
    
    val targetKindF: F[Option[Either[FilePath, DocumentType]]] = event.context() match {
      case p: java.nio.file.Path =>
        val file = FilePath.fromNioPath(p)
        val target = targets.get(watchKey)
        (fileFilter.filter(file), Files[F].isDirectory(file.toFS2Path)).tupled.flatMap { case (exclude, isDir) =>
          (exclude, isDir, target) match {
            case (false, true, Some(_))  => Async[F].pure(Some(Left(file)))
            case (false, false, Some(t)) => t.docTypeFor(file).map {
              case DocumentType.Ignored  => None
              case other                 => Some(Right(other))
            }
            case _                       => Async[F].pure(None)
          }
        }
      case _ => Async[F].pure(None)
    }
    
    targetKindF.map { targetKind =>
      (event.kind, targetKind) match {
        case (ENTRY_CREATE, Some(Left(path)))       => ProcessedEvent(triggersUpdate = true, Some(path))
        case (ENTRY_MODIFY, Some(Right(Static(_)))) => ProcessedEvent(triggersUpdate = false, None)
        case (ENTRY_CREATE, Some(_))                => ProcessedEvent(triggersUpdate = true, None)
        case (ENTRY_DELETE, Some(_))                => ProcessedEvent(triggersUpdate = true, None)
        case (ENTRY_MODIFY, Some(_))                => ProcessedEvent(triggersUpdate = true, None)
        case _                                      => ProcessedEvent(triggersUpdate = false, None)
      }
    }
  }
  
  val poll: F[Unit] = {
    val events = Async[F].delay(collectEvents(Nil))
    (events, targetMap.get)
      .mapN { case (events, targets) =>
        events
          .traverse { case (event, key) => processEvent(event, key, targets) }
          .flatMap { processedEvents =>
            val needsUpdate = processedEvents.find(_.triggersUpdate).nonEmpty
            val newDirectories = processedEvents.flatMap(_.newDirectory)
            val updateF = if (needsUpdate) update else Async[F].unit
            val registrations = newDirectories
              .traverse(registerRoot(_))
              .flatMap(updateTargetMap)
            
            updateF <* registrations
          }
        }
        .flatten
  }
  
}

private [preview] object SourceChangeWatcher {

  sealed trait ObservedTarget {
    def parent: FilePath
    def key: WatchKey
    def docTypeMatcher: Path => DocumentType
    def filter[F[_]: Async] (child: FilePath): F[Boolean]
    def docTypeFor[F[_]: Async] (child: FilePath): F[DocumentType] =
      filter(child).map(if (_) docTypeMatcher(Path.parse(child.toString)) else DocumentType.Ignored) 
  }
  case class ObservedDirectory (parent: FilePath, key: WatchKey, fileFilter: FileFilter, docTypeMatcher: Path => DocumentType) extends ObservedTarget {
    def filter[F[_]: Async] (child: FilePath): F[Boolean] = fileFilter.filter(child).map(!_)
  }
  case class ObservedFiles (parent: FilePath, key: WatchKey, children: Set[FilePath], docTypeMatcher: Path => DocumentType) extends ObservedTarget {
    def filter[F[_]: Async] (child: FilePath): F[Boolean] = Async[F].pure(children.contains(child))
  }

  def scheduledTaskResource[F[_]: Temporal] (task: F[Unit], interval: FiniteDuration): Resource[F, Unit] = {
    val bg = fs2.Stream.fixedRate[F](interval).evalMap(_ => task)
    fs2.Stream.emit(()).concurrently(bg).compile.resource.lastOrError
  }
  
  def create[F[_]: Async] (inputs: List[FilePath],
                           update: F[Unit],
                           pollInterval: FiniteDuration,
                           fileFilter: FileFilter,
                           docTypeMatcher: Path => DocumentType): Resource[F, Unit] = {
    if (inputs.isEmpty) Resource.unit
    else Resource
      .make(Async[F].delay(FileSystems.getDefault.newWatchService))(service => Async[F].delay(service.close()))
      .evalMap(s => Async[F].ref(Map.empty[WatchKey, ObservedTarget]).map((s,_)))
      .flatMap { case (service, targetMap) =>
        val watcher = new SourceChangeWatcher[F](service, targetMap, inputs, update, fileFilter, docTypeMatcher)
        Resource.eval(watcher.init) *>
          scheduledTaskResource(watcher.poll, pollInterval)
      }
  }
  
}