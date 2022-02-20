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

package laika.io.config

import java.io.File
import java.net.URL

import cats.effect.Sync
import cats.implicits._
import laika.config.Config.IncludeMap
import laika.config.{ConfigParser, ConfigResourceError}
import laika.io.runtime.Batch
import laika.parse.hocon.{IncludeAny, IncludeClassPath, IncludeFile, IncludeResource, IncludeUrl, ValidStringValue}
import cats.effect.kernel.Async

/** Internal utility that provides configuration files requested by include statements in other
  * configuration instances.
  * 
  * @author Jens Halm
  */
object IncludeHandler {
  
  case class RequestedInclude(resource: IncludeResource, parent: Option[IncludeResource])
  case class LoadedInclude(requestedResource: IncludeResource, resolvedResource: IncludeResource, result: Either[ConfigResourceError, String])

  /** Loads the requested resources and maps them to the request instance for later lookup.
    * 
    * If a resource is not present (e.g. file does not exist in the file system or HTTP call
    * produced a 404) then the requested resource will not be present as a key in the result map.
    * 
    * If a resource is present, but fails to load or parse correctly, the error will
    * be mapped to the requested resource as a `Left`. Successfully loaded and parsed
    * resources appear in the result map as a `Right`.
    */
  def load[F[_]: Async : Batch] (includes: Seq[RequestedInclude]): F[IncludeMap] = 
    
    if (includes.isEmpty) Sync[F].pure(Map.empty) else {
      
      def prepareFile(include: IncludeFile, requested: IncludeResource, parent: Option[IncludeResource]): F[(IncludeResource, IncludeResource)] = Sync[F].pure {
        if (new File(include.resourceId.value).isAbsolute) (include, requested)
        else parent.flatMap {
          case IncludeFile(id, _) => Option(new File(id.value).getParentFile)
          case _ => None
        } match {
          case Some(parentFile) => (IncludeFile(ValidStringValue(new File(parentFile, include.resourceId.value).getPath), include.isRequired), requested)
          case None => (include, requested)
        }
      }
      
      def prepareClasspath(include: IncludeClassPath, requested: IncludeResource, parent: Option[IncludeResource]): F[(IncludeResource, IncludeResource)] = Sync[F].pure {
        if (include.resourceId.value.startsWith("/")) (include.copy(resourceId = ValidStringValue(include.resourceId.value.drop(1))), include)
        else parent match {
          case Some(p: IncludeClassPath) if p.resourceId.value.contains("/") => 
            val parentPath = p.resourceId.value.substring(0, p.resourceId.value.lastIndexOf("/"))
            val childPath = s"$parentPath/${include.resourceId.value}"
            (IncludeClassPath(ValidStringValue(childPath), include.isRequired), requested)
          case _ => (include, requested)
        }
      }
      
      def prepareUrl(include: IncludeUrl, requested: IncludeResource, parent: Option[IncludeResource]): F[(IncludeResource, IncludeResource)] = Sync[F].delay {
        parent match {
          case Some(p: IncludeUrl) => 
            val parentUrl = new URL(p.resourceId.value)
            val childUrl = new URL(parentUrl, include.resourceId.value)
            (IncludeUrl(ValidStringValue(childUrl.toString), include.isRequired), requested)
          case _ => (include, requested)
        }
      }

      def prepareAny(include: IncludeAny, parent: Option[IncludeResource]): F[(IncludeResource, IncludeResource)] = 
        Sync[F].delay(new URL(include.resourceId.value))
          .flatMap(_ => prepareUrl(IncludeUrl(include.resourceId, include.isRequired), include, parent))
          .handleErrorWith { _ =>
            parent match {
              case Some(_: IncludeClassPath) => prepareClasspath(IncludeClassPath(include.resourceId, include.isRequired), include, parent)
              case Some(_: IncludeUrl)       => prepareUrl(IncludeUrl(include.resourceId, include.isRequired), include, parent)
              case _                         => prepareFile(IncludeFile(include.resourceId, include.isRequired), include, parent)
            }
          }
      
      val preparedIncludes = includes.map {
        case RequestedInclude(i: IncludeFile, parent)      => prepareFile(i, i, parent)
        case RequestedInclude(i: IncludeClassPath, parent) => prepareClasspath(i, i, parent)
        case RequestedInclude(i: IncludeUrl, parent)       => prepareUrl(i, i, parent)
        case RequestedInclude(i: IncludeAny, parent)       => prepareAny(i, parent)
      }.toVector.sequence
    
      def result(requestedResource: IncludeResource, resolvedResource: IncludeResource, result: F[Option[Either[ConfigResourceError, String]]]): F[Option[LoadedInclude]] =
        result.map(_.map(res => LoadedInclude(requestedResource, resolvedResource, res)))
      
      preparedIncludes.flatMap { includes =>
        Batch[F].execute(
          includes.map {
            case (i@IncludeFile(resourceId, _), orig)      => result(orig, i, ResourceLoader.loadFile(resourceId.value))
            case (i@IncludeClassPath(resourceId, _), orig) => result(orig, i, ResourceLoader.loadClasspathResource(resourceId.value))
            case (i@IncludeUrl(resourceId, _), orig)       => Sync[F].delay(new URL(resourceId.value))
                                                              .flatMap(url => result(orig, i, ResourceLoader.loadUrl(url)))
            case _                                       => Sync[F].pure(Option.empty[LoadedInclude])
          }
        )
      }.flatMap { loadedResources =>
        val configParsers = loadedResources.unite.map(loaded => (loaded.requestedResource, loaded.resolvedResource, loaded.result.map(ConfigParser.parse)))
        val includeMap = configParsers.map {
          case (requested, _, result) => 
            (requested, result.flatMap(_.unresolved))
        }.toMap
        val recursiveIncludes = configParsers.flatMap {
          case (_, resolved, Right(parser)) => parser.includes.filterNot(includeMap.contains).map(RequestedInclude(_, Some(resolved)))
          case _ => Nil
        }
        load(recursiveIncludes).map(_ ++ includeMap)
      }
      
    }
  
}
