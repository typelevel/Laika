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

package laika.config

import laika.ast.{Path, SegmentedPath}
import laika.ast.Path.Root

/**
  * @author Jens Halm
  */
case class Key(segments: Seq[String]) {

  def child (segment: String): Key = Key(segments :+ segment)
  
  def child (childKey: Key): Key = Key(segments ++ childKey.segments)
  
  def isChild (other: Key): Boolean = this.segments.startsWith(other.segments)
  
  def parent: Key = if (segments.isEmpty) this else Key(segments.init)
  
  def local: Key = if (segments.isEmpty) this else Key(segments.last)
  
  @deprecated("key to path conversion is no longer necessary in any Laika API", "0.18.0")
  def toPath: Path = Path(segments.toList)

  override def toString: String = if (segments.isEmpty) "<RootKey>" else segments.mkString(".")
}

object Key {
  
  def apply(segment: String, segments: String*): Key = Key(segment +: segments.toList)
  
  def parse(key: String): Key = if (key.isEmpty) root else {
    val segments = key.split("\\.").toList
    Key(segments)
  }
  
  val root: Key = Key(Nil)
}
