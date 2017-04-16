/*
 * Copyright 2013-2016 the original author or authors.
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

package laika.rewrite

import com.typesafe.config.Config
import laika.tree.Documents.Document
import laika.tree.Elements.SpanSequence
import scala.util.Try

/** A resolver for context references in templates or markup documents.
 *  
 *  @author Jens Halm
 */
case class ReferenceResolver (root: Any, parent: Option[ReferenceResolver] = None) {
  import java.util.{Map => JMap}
  def fromJavaMap (m: JMap[Any,Any], key: Any): Option[Any] = if (m.containsKey(key)) Some(m.get(key)) else None
  /* These are all dynamic, non-typesafe lookups for values where often both,
   * the path from the template and the actual target value (e.g. from a config
   * file) originate from text resources, so the dynamic lookup is justifiable here */
  def resolve (target: Any, path: List[String], root: Boolean = false): (Option[Any], List[String]) = {
    val result = target match {
      case m: JMap[_, _]=> (fromJavaMap(m.asInstanceOf[JMap[Any,Any]], path.head), path.tail)
      case m: Map[_, _] => (m.asInstanceOf[Map[Any,Any]].get(path.head), path.tail)
      case c: Config    => (Try { c.getAnyRef(path.mkString(".")) } toOption, Nil)
      case d: Document if path.head == "title" => (Some(SpanSequence(d.title)), path.tail)  
      case other        => (Try { target.getClass.getMethod(path.head).invoke(target) } toOption, path.tail)
    }
    result match {
      case (None, _) if root && parent.isDefined => parent.get.resolve(target, path, root)
      case (None, _)            => (None, Nil)
      case (Some(value), Nil)   => (Some(value), Nil)
      case (Some(value), path)  => resolve(value, path)
    }
  }
  
  def resolve (path: List[String]): Option[Any] = resolve(root, path, root = true)._1
  
}

/** Companion for constructing ReferenceResolvers for a particular
 *  target Document.
 */
object ReferenceResolver {
  
  /** Creates a new ReferenceResolver for the specified
   *  document and its parent and configuration.
   */
  def forDocument(document: Document, parent: TreeCursor, config: Config): ReferenceResolver =
    apply(Map[String,Any](
      "config" -> config,
      "document" -> document,
      "parent" -> parent.target,
      "root" -> parent.root.target
    ))
  
}
