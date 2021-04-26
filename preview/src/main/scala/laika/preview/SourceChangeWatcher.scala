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

import java.io.File
import java.nio.file.{FileSystems, Files, WatchEvent, WatchService, Path => JPath}
import java.nio.file.StandardWatchEventKinds._

import laika.ast.{DocumentType, Path}
import cats.syntax.all._

import scala.concurrent.duration._
import cats.effect.{Async, Ref, Resource, Temporal}
import laika.ast.DocumentType.Static
import laika.collection.TransitionalCollectionOps.{JIteratorWrapper, TransitionalMapOps}
import laika.io.runtime.DirectoryScanner
import laika.preview.SourceChangeWatcher.{ObservedDirectory, ObservedFiles, ObservedTarget}

import scala.annotation.tailrec

class SourceChangeWatcher[F[_]: Async] (service: WatchService,
                                        targetMap: Ref[F, Map[JPath, ObservedTarget]],
                                        inputs: List[File],
                                        update: F[Unit],
                                        fileFilter: File => Boolean,
                                        docTypeMatcher: Path => DocumentType) {
  
  private def scanDirectory (dir: JPath): F[List[JPath]] = DirectoryScanner.scanDirectory[F, List[JPath]](dir) {
    _.toList
      .map { childPath =>
        if (Files.isDirectory(childPath) && !fileFilter(childPath.toFile)) scanDirectory(childPath)
        else Async[F].pure(List.empty[JPath])
      }
      .sequence.map(_.flatten :+ dir)
  }
  
  def registerRoot (file: JPath, children: List[JPath] = Nil): F[List[ObservedTarget]] = {
    
    def registerDirectory (dir: JPath, children: List[JPath] = Nil): F[ObservedTarget] = {
      val target = 
        if (children.isEmpty) ObservedDirectory(dir, fileFilter, docTypeMatcher)
        else ObservedFiles(dir, children.toSet, docTypeMatcher)
      Async[F]
        .delay(dir.register(service, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY))
        .as(target)
    }
    
    if (children.isEmpty) scanDirectory(file).flatMap(_.map(registerDirectory(_)).sequence)
    else registerDirectory(file, children).map(List(_))
  }
  
  private def updateTargetMap (targets: List[List[ObservedTarget]]): F[Unit] = {
    val initTargetMap = targets.flatten.map(t => (t.parent, t)).toMap
    targetMap.update(_ ++ initTargetMap)
  }
  
  val init: F[Unit] = {
    
    val groupedInputs = inputs.map { input =>
      if (input.isDirectory) (input.toPath, None)
      else (input.toPath.getParent, Some(input.toPath))
    }
      .groupBy(_._1)
      .mapValuesStrict(_.flatMap(_._2))

    groupedInputs
      .map {
        case (parent, children) => registerRoot(parent, children)
      }
      .toList
      .sequence
      .flatMap(updateTargetMap)
  }

  @tailrec
  private def collectEvents (acc: List[WatchEvent[_]]): List[WatchEvent[_]] = {
    val key = service.poll()
    if (key == null) acc
    else {
      val events = JIteratorWrapper(key.pollEvents().iterator()).toList
      key.reset()
      collectEvents(acc ++ events)
    }
  }
  
  val poll: F[Unit] = {
    val events = Async[F].delay(collectEvents(Nil))
    (events, targetMap.get).mapN { case (events, targets) =>
      val (needsUpdate, newDirectories) = events.foldLeft((false, List.empty[JPath])) { case ((needsUpdate, newDirectories), event) =>
        val targetKind = event.context() match {
          case p: JPath if !fileFilter(p.toFile) => targets.get(p.getParent).flatMap { t =>
            if (Files.isDirectory(p)) Some(Left(p))
            else t.docTypeFor(p) match {
              case DocumentType.Ignored  => None
              case other                 => Some(Right(other)) 
            }
          }
          case _ => None
        }
        (event.kind, targetKind) match {
          case (ENTRY_CREATE, Some(Left(path)))       => (true,        newDirectories :+ path)
          case (ENTRY_MODIFY, Some(Right(_: Static))) => (needsUpdate, newDirectories)
          case (ENTRY_CREATE | ENTRY_DELETE, Some(_)) => (true,        newDirectories)
          case _                                      => (needsUpdate, newDirectories)
        }
      }
      val updateF = if (needsUpdate) update else Async[F].unit
      val registrations = newDirectories
        .map(registerRoot(_))
        .sequence
        .flatMap(updateTargetMap)
      
      updateF <* registrations
    }
  }
  
}

object SourceChangeWatcher {

  sealed trait ObservedTarget {
    def parent: JPath
    def docTypeMatcher: Path => DocumentType
    def filter (child: JPath): Boolean
    def docTypeFor (child: JPath): DocumentType =
      if (filter(child)) docTypeMatcher(Path.parse(child.toString))
      else DocumentType.Ignored
  }
  case class ObservedDirectory (parent: JPath, fileFilter: File => Boolean, docTypeMatcher: Path => DocumentType) extends ObservedTarget {
    def filter (child: JPath): Boolean = !fileFilter(child.toFile)
  }
  case class ObservedFiles (parent: JPath, children: Set[JPath], docTypeMatcher: Path => DocumentType) extends ObservedTarget {
    def filter (child: JPath): Boolean = children.contains(child)
  }

  def scheduledTaskResource[F[_]: Temporal] (task: F[Unit], interval: FiniteDuration): Resource[F, Unit] = {
    val bg = fs2.Stream.fixedRate[F](interval).evalMap(_ => task)
    fs2.Stream.emit(()).concurrently(bg).compile.resource.lastOrError
  }
  
  def create[F[_]: Async] (inputs: List[File], 
                           update: F[Unit], 
                           pollInterval: FiniteDuration,
                           fileFilter: File => Boolean,
                           docTypeMatcher: Path => DocumentType): Resource[F, Unit] = {
    Resource
      .make(Async[F].delay(FileSystems.getDefault.newWatchService))(service => Async[F].delay(service.close()))
      .evalMap(s => Async[F].ref(Map.empty[JPath, ObservedTarget]).map((s,_)))
      .flatMap { case (service, targetMap) =>
        val watcher = new SourceChangeWatcher[F](service, targetMap, inputs, update, fileFilter, docTypeMatcher)
        scheduledTaskResource(watcher.poll, pollInterval)
      }
  }
  
}