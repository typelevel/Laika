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

import cats.implicits._
import cats.effect.Async
import laika.config.Config.IncludeMap
import laika.config.{ConfigParser, ConfigResourceError}
import laika.io.runtime.Runtime
import laika.parse.hocon.{IncludeAny, IncludeClassPath, IncludeFile, IncludeResource, IncludeUrl}

/**
  * @author Jens Halm
  */
object IncludeHandler {
  
  case class RequestedInclude(resource: IncludeResource, parent: Option[IncludeResource])
  case class LoadedInclude(resource: IncludeResource, result: Either[ConfigResourceError, String])
  
  def load[F[_]: Async : Runtime] (includes: Seq[RequestedInclude]): F[IncludeMap] = 
    
    if (includes.isEmpty) Async[F].pure(Map.empty) else {
      
      def prepareFile(include: IncludeFile, parent: Option[IncludeResource]): F[(IncludeResource, IncludeResource)] = Async[F].pure((include, include))
      
      def prepareClasspath(include: IncludeClassPath, parent: Option[IncludeResource]): F[(IncludeResource, IncludeResource)] = Async[F].pure((include, include))
      
      def prepareUrl(include: IncludeUrl, parent: Option[IncludeResource]): F[(IncludeResource, IncludeResource)] = Async[F].pure((include, include))

      def prepareAny(include: IncludeAny, parent: Option[IncludeResource]): F[(IncludeResource, IncludeResource)] = Async[F].pure((include, include))
      
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
        val recursiveIncludes = configParsers.flatMap { 
          case (res, Right(parser)) => parser.includes.map(RequestedInclude(_, Some(res)))
          case _ => Nil
        }
        val includeMap = configParsers.map {
          case (resource, result) => (resource, result.flatMap(_.unresolved))
        }.toMap
        load(recursiveIncludes).map(_ ++ includeMap)
      }
      
    }
  
}
