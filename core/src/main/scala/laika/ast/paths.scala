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

package laika.ast

import cats.implicits._
import cats.data.NonEmptyList
import laika.ast.Path.Root
import laika.ast.RelativePath.{Current, Parent}

import scala.annotation.tailrec

sealed trait PathBase // TODO - promote shared methods

object PathBase {
  
  def decode(path: String): PathBase = if (path.startsWith("/")) Path(path) else RelativePath(path)
  
}

/** Represents a path inside a virtual tree of documents.
 */
sealed trait Path extends PathBase {

  /** The parent of this path.
   *  Will return this if this path represents a root node.
   */
  def parent: Path

  /** The local name of this path.
   */
  def name: String

  /** The depth of this path from the virtual root.
    */
  def depth: Int

  /** Creates a new path with the specified name
   *  as an immediate child of this path.
   */
  def / (name: String): Path

  /** Combines this path with the specified relative path.
   */
  def / (path: RelativePath): Path

  /** Interprets this path relative to some other path.
   */
  def relativeTo (path: Path): RelativePath
  
  def suffix: String = ""
  def basename: String = name
  
  def withSuffix (suffix: String): Path = this
  
  def isSubPath (other: Path): Boolean
}

// TODO - needs new / extractor

case class SegmentedPath (segments: NonEmptyList[String]) extends Path {
  
  val depth: Int = segments.size
  
  lazy val name: String = segments.last
  
  lazy val parent: Path = NonEmptyList.fromList(segments.init).fold[Path](Root)(SegmentedPath)

  def / (name: String): Path = SegmentedPath(segments :+ name)

  /** Combines this path with the specified relative path.
    */
  def / (path: RelativePath): Path =
    NonEmptyList.fromList(segments.toList.dropRight(path.parentLevels) ++ path.components).fold[Path](Root)(SegmentedPath)
  
  def relativeTo (path: Path): RelativePath = {
    // TODO - needs separate methods for relativeToTree + relativeToDocument (+ relativeToOrigin?)
    def buildRelativePath = {
      def removeCommonParts (a: List[String], b: List[String]): (List[String],List[String]) = (a,b) match {
        case (p1 :: rest1, p2 :: rest2) if p1 == p2 => removeCommonParts(rest1,rest2)
        case _ => (a,b)
      }
      val (a, b) = path match {
        case Root => (Nil, segments.toList)
        case SegmentedPath(otherSegments) => removeCommonParts(otherSegments.toList, segments.toList)
      } 
      val base = if (a.isEmpty) Current else Parent(a.length)
      if (b.nonEmpty) base / SegmentedRelativePath(b, 0)
      else base
    }
    buildRelativePath
  }

  def isSubPath (other: Path): Boolean = other match {
    case Root => true
    case SegmentedPath(segments) => this.segments.toList.startsWith(segments.toList)
  } 
  
  override lazy val basename: String = if (name.contains('.')) name.take(name.lastIndexOf(".")) else name
  
  override lazy val suffix: String = if (name.contains('.')) name.drop(name.lastIndexOf(".")+1) else ""
  
  override def withSuffix (newSuffix: String): Path = 
    if (name.endsWith(newSuffix) || segments.isEmpty) this 
    else SegmentedPath(NonEmptyList.ofInitLast(segments.init, basename + "." + newSuffix))
  
  override lazy val toString: String = "/" + (segments.toList mkString "/")
}

/** Factory methods for creating path instances.
  */
object Path {

  /** The root of an absolute path.
    */
  case object Root extends Path {
    val depth: Int = 0
    val parent: Path = this
    val name: String = "/"
    def / (name: String): Path = SegmentedPath(NonEmptyList.of(name))
    def / (path: RelativePath): Path = path match {
      case SegmentedRelativePath(segments, _) => Path(segments)
      case _ => this
    }
    def relativeTo (path: Path): RelativePath = path match {
      case Root => Current
      case SegmentedPath(segments) => Parent(segments.size)
    }
    def isSubPath (other: Path): Boolean = other == Root
    override val toString: String = "/"
  }

  /** Creates path from interpreting the specified string representation.
    * The path must be absolute (start with a `/`).
    */
  def apply(str: String): Path = {
    require(str.headOption.contains('/'), "path must be absolute")
    str match {
      case "/"   => Root
      case other => apply(other.drop(1).stripSuffix("/").split("/").toList)
    }
  }

  def apply (segments: List[String]): Path = NonEmptyList.fromList(segments).fold[Path](Root)(SegmentedPath)

}

sealed trait RelativePath extends PathBase {

  /** The parent of this path.
    * Will return this if this path represents the current node.
    */
  def parent: RelativePath

  /** The local name of this path.
    */
  def name: String = components.lastOption.getOrElse("")

  /** The number of levels this relative path points above the current level.
    */
  def parentLevels: Int

  /** All components after the prefix as a list of strings.
    */
  def components: List[String]

  /** Creates a new path with the specified name
    *  as an immediate child of this path.
    */
  def / (name: String): RelativePath = SegmentedRelativePath(components :+ name, parentLevels)

  /** Combines this path with the specified relative path.
    */
  def / (path: RelativePath): RelativePath =
    SegmentedRelativePath(components.dropRight(path.parentLevels) ++ path.components, parentLevels)

  def suffix: String = ""
  def basename: String = name
  def withSuffix (newSuffix: String): RelativePath
}

case class SegmentedRelativePath(components: List[String], parentLevels: Int) extends RelativePath {

  lazy val parent: RelativePath =
    if (components.isEmpty) this
    else if (components.tail.isEmpty) RelativePath.Current
    else SegmentedRelativePath(components.init, parentLevels)

  override lazy val toString: String = ("../" * parentLevels) + (components mkString "/")

  override lazy val basename: String = if (name.contains('.')) name.take(name.lastIndexOf(".")) else name

  override lazy val suffix: String = if (name.contains('.')) name.drop(name.lastIndexOf(".")+1) else ""
  
  override def withSuffix (newSuffix: String): RelativePath = 
    if (name.endsWith(newSuffix) || components.isEmpty) this
    else SegmentedRelativePath(components.init :+ basename + "." + newSuffix, parentLevels)
  
}

object RelativePath {

  /** The root of an absolute path.
    */
  case object Current extends RelativePath {
    val components: List[String] = Nil
    val parent: RelativePath = Parent(1)
    val parentLevels: Int = 0
    def withSuffix (newSuffix: String): RelativePath = this
    override val toString: String = "."
  }

  case class Parent(parentLevels: Int) extends RelativePath {
    val components: List[String] = Nil
    lazy val parent: RelativePath = Parent(parentLevels + 1)
    def withSuffix (newSuffix: String): RelativePath = this
    override val toString: String = "../" * parentLevels
  }

  /** Creates a relative path from interpreting the specified string representation.
    * If it starts with `/` it will be interpreted as an absolute path,
    * if it starts with `../` as a relative path pointing to some parent
    * path. Otherwise it will be interpreted as a relative path.
    */
  def apply (str: String): RelativePath = {
    require(!str.headOption.contains('/'), "path must not be absolute")
    str.stripSuffix("/") match {
      case ""    => Current
      case other =>
        @tailrec def countParents(current: Int, path: String): (Int, String) = 
          if (path.startsWith("..")) countParents(current + 1, path.drop(2).stripPrefix("/"))
          else (current, path)
        val (levels, rest) = countParents(0, other)
        if (rest.isEmpty) Parent(levels)
        else SegmentedRelativePath(rest.split("/").toList, levels)
    }
  }
  
}

/** Extractors for pattern matching against absolute and relative paths.
  */
object / {
  def unapply(p: Path): Option[(Path, String)] = p match {
    case Root => None
    case _ => Some((p.parent, p.name))
  }

  def unapply(p: RelativePath): Option[(RelativePath, String)] = p match {
    case Current => None
    case Parent(_) => None
    case _ => Some((p.parent, p.name))
  }
}
