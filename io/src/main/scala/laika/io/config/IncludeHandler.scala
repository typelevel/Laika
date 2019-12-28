/*
 * Copyright 2012-2019 the original author or authors.
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

package laika.io.config

import java.io.File
import java.net.URL

import cats.effect.Async
import cats.implicits._
import laika.config.Config.IncludeMap
import laika.config.{ConfigParser, ConfigResourceError}
import laika.io.runtime.Runtime
import laika.parse.hocon.{IncludeAny, IncludeClassPath, IncludeFile, IncludeResource, IncludeUrl, ValidStringValue}

/**
  * @author Jens Halm
  */
object IncludeHandler {
  
  case class RequestedInclude(resource: IncludeResource, parent: Option[IncludeResource])
  case class LoadedInclude(resource: IncludeResource, result: Either[ConfigResourceError, String])
  
  def load[F[_]: Async : Runtime] (includes: Seq[RequestedInclude]): F[IncludeMap] = 
    
    if (includes.isEmpty) Async[F].pure(Map.empty) else {
      
      def prepareFile(include: IncludeFile, parent: Option[IncludeResource]): F[(IncludeResource, IncludeResource)] = Async[F].pure {
        if (new File(include.resourceId.value).isAbsolute) (include, include)
        else parent.flatMap {
          case IncludeFile(id, _) => Option(new File(id.value).getParentFile)
          case _ => None
        } match {
          case Some(parentFile) => (IncludeFile(ValidStringValue(new File(parentFile, include.resourceId.value).getPath), include.isRequired), include)
          case None => (include, include)
        }
      }
      
      def prepareClasspath(include: IncludeClassPath, parent: Option[IncludeResource]): F[(IncludeResource, IncludeResource)] = Async[F].pure {
        if (include.resourceId.value.startsWith("/")) (include.copy(resourceId = ValidStringValue(include.resourceId.value.drop(1))), include)
        else parent match {
          case Some(p: IncludeClassPath) if p.resourceId.value.contains("/") => 
            val parentPath = p.resourceId.value.substring(0, p.resourceId.value.lastIndexOf("/"))
            val childPath = s"$parentPath/${include.resourceId.value}"
            (IncludeClassPath(ValidStringValue(childPath), include.isRequired), include)
          case _ => (include, include)
        }
      }
      
      def prepareUrl(include: IncludeUrl, parent: Option[IncludeResource]): F[(IncludeResource, IncludeResource)] = Async[F].delay {
        parent match {
          case Some(p: IncludeUrl) => 
            val parentUrl = new URL(p.resourceId.value)
            val childUrl = new URL(parentUrl, include.resourceId.value)
            (IncludeUrl(ValidStringValue(childUrl.toString), include.isRequired), include)
          case _ => (include, include)
        }
      }

      def prepareAny(include: IncludeAny, parent: Option[IncludeResource]): F[(IncludeResource, IncludeResource)] = 
        Async[F].delay(new URL(include.resourceId.value))
          .flatMap(_ => prepareUrl(IncludeUrl(include.resourceId, include.isRequired), parent))
          .handleErrorWith { _ =>
            parent match {
              case Some(_: IncludeClassPath) => prepareClasspath(IncludeClassPath(include.resourceId, include.isRequired), parent)
              case Some(_: IncludeUrl)       => prepareUrl(IncludeUrl(include.resourceId, include.isRequired), parent)
              case _                         => prepareFile(IncludeFile(include.resourceId, include.isRequired), parent)
            }
          }
      
      val preparedIncludes = includes.map {
        case RequestedInclude(i: IncludeFile, parent)      => prepareFile(i, parent)
        case RequestedInclude(i: IncludeClassPath, parent) => prepareClasspath(i, parent)
        case RequestedInclude(i: IncludeUrl, parent)       => prepareUrl(i, parent)
        case RequestedInclude(i: IncludeAny, parent)       => prepareAny(i, parent)
      }.toVector.sequence
    
      def result(resource: IncludeResource, result: F[Option[Either[ConfigResourceError, String]]]): F[Option[LoadedInclude]] =
        result.map(_.map(res => LoadedInclude(resource, res)))
      
      preparedIncludes.flatMap { includes =>
        Runtime[F].runParallel(
          includes.map {
            case (IncludeFile(resourceId, _), orig)      => result(orig, ResourceLoader.loadFile(resourceId.value))
            case (IncludeClassPath(resourceId, _), orig) => result(orig, ResourceLoader.loadClasspathResource(resourceId.value))
            case (IncludeUrl(resourceId, _), orig)       => result(orig, ResourceLoader.loadClasspathResource(resourceId.value))
            case _                                       => Async[F].pure(Option.empty[LoadedInclude])
          }
        )
      }.flatMap { loadedResources =>
        val configParsers = loadedResources.unite.map(loaded => (loaded.resource, loaded.result.map(ConfigParser.parse)))
        val includeMap = configParsers.map {
          case (resource, result) => (resource, result.flatMap(_.unresolved))
        }.toMap
        val recursiveIncludes = configParsers.flatMap {
          case (res, Right(parser)) => parser.includes.filterNot(includeMap.contains).map(RequestedInclude(_, Some(res)))
          case _ => Nil
        }
        load(recursiveIncludes).map(_ ++ includeMap)
      }
      
    }
  
}
