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

package laika.config

import laika.ast.Path

/**
  * @author Jens Halm
  */
case class Key(segments: Seq[String]) {

  def child (segment: String): Key = Key(segments :+ segment)
  
  def isChild (other: Key): Boolean = this.segments.startsWith(other.segments)
  
  def parent: Key = if (segments.isEmpty) this else Key(segments.init)
  
  def local: Key = if (segments.isEmpty) this else Key(segments.last)
  
  def toPath: Path = Path(segments.toList)

  override def toString: String = if (segments.isEmpty) "<RootKey>" else segments.mkString(".")
}

object Key {
  
  def apply(segment: String, segments: String*): Key = Key(segment +: segments)
  
  def parse(key: String): Key = {
    val segments = key.split("\\.").toList
    Key(segments)
  }
  
  def fromPath(path: Path): Key = Key(path.components)
  
  val root: Key = Key(Nil)
}
