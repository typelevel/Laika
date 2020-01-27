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

sealed trait PathBase extends Product with Serializable // TODO - promote shared methods

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

case class SegmentedPath (segments: NonEmptyList[String]) extends Path {
  
  val depth: Int = segments.size
  
  lazy val name: String = segments.last
  
  lazy val parent: Path = NonEmptyList.fromList(segments.init).fold[Path](Root)(SegmentedPath)

  def / (name: String): Path = SegmentedPath(segments :+ name)

  def / (path: RelativePath): Path = {
    val otherSegments = path match {
      case SegmentedRelativePath(s, _) => s.toList
      case _ => Nil
    }
    NonEmptyList.fromList(segments.toList.dropRight(path.parentLevels) ++ otherSegments).fold[Path](Root)(SegmentedPath)
  }
  
  def relativeTo (path: Path): RelativePath = {
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
      NonEmptyList.fromList(b).fold[RelativePath](base)(seg => base / SegmentedRelativePath(seg))
    }
    buildRelativePath
  }

  def isSubPath (other: Path): Boolean = other match {
    case Root => true
    case SegmentedPath(otherSegments) => segments.toList.startsWith(otherSegments.toList)
  } 
  
  override lazy val basename: String = if (name.contains('.')) name.take(name.lastIndexOf(".")) else name
  
  override lazy val suffix: String = if (name.contains('.')) name.drop(name.lastIndexOf(".")+1) else ""
  
  override def withSuffix (newSuffix: String): Path = 
    if (name.endsWith(newSuffix)) this 
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
      case SegmentedRelativePath(segments, _) => SegmentedPath(segments)
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
  def name: String

  /** The number of levels this relative path points above the current level.
    */
  def parentLevels: Int

  /** Creates a new path with the specified name
    *  as an immediate child of this path.
    */
  def / (name: String): RelativePath

  /** Combines this path with the specified relative path.
    */
  def / (path: RelativePath): RelativePath

  def suffix: String = ""
  def basename: String = name
  def withSuffix (newSuffix: String): RelativePath
}

case class SegmentedRelativePath(segments: NonEmptyList[String], parentLevels: Int = 0) extends RelativePath {

  lazy val parent: RelativePath = {
    def noSegments = if (parentLevels == 0) Current else Parent(parentLevels)
    NonEmptyList.fromList(segments.init)
      .fold[RelativePath](noSegments)(SegmentedRelativePath(_, parentLevels))
  }

  val name: String = segments.last

  def / (name: String): RelativePath = SegmentedRelativePath(segments :+ name, parentLevels)

  def / (path: RelativePath): RelativePath = path match {
    case Current => this
    case Parent(otherLevels) => copy(parentLevels = parentLevels + otherLevels)
    case SegmentedRelativePath(otherSegments, otherLevels) => SegmentedRelativePath(
      // cats Nel lacks a prepend(List[A]), but we know it will be non-empty
      NonEmptyList.fromListUnsafe(segments.toList.dropRight(path.parentLevels) ++ otherSegments.toList),
      parentLevels + Math.max(0, otherLevels - segments.size)
    )
  }

  override lazy val toString: String = ("../" * parentLevels) + (segments.toList mkString "/")

  override lazy val basename: String = if (name.contains('.')) name.take(name.lastIndexOf(".")) else name

  override lazy val suffix: String = if (name.contains('.')) name.drop(name.lastIndexOf(".")+1) else ""
  
  override def withSuffix (newSuffix: String): RelativePath = 
    if (name.endsWith(newSuffix)) this
    else SegmentedRelativePath(NonEmptyList.ofInitLast(segments.init, basename + "." + newSuffix), parentLevels)
  
}

object RelativePath {

  /** The root of an absolute path.
    */
  case object Current extends RelativePath {
    val name = "."
    val parent: RelativePath = Parent(1)
    val parentLevels: Int = 0
    def / (name: String): RelativePath = SegmentedRelativePath(NonEmptyList.of(name))
    def / (path: RelativePath): RelativePath = path
    def withSuffix (newSuffix: String): RelativePath = this
    override val toString: String = name
  }

  case class Parent(parentLevels: Int) extends RelativePath {
    val name: String = "../" * parentLevels
    lazy val parent: RelativePath = Parent(parentLevels + 1)
    def / (name: String): RelativePath = SegmentedRelativePath(NonEmptyList.of(name), parentLevels)
    def / (path: RelativePath): RelativePath = path match {
      case Current => this
      case Parent(otherLevels) => Parent(parentLevels + otherLevels)
      case SegmentedRelativePath(segments, otherLevels) => SegmentedRelativePath(segments, parentLevels + otherLevels)
    }
    def withSuffix (newSuffix: String): RelativePath = this
    override val toString: String = name
  }

  /** Creates a relative path from interpreting the specified string representation.
    * If it starts with `/` it will be interpreted as an absolute path,
    * if it starts with `../` as a relative path pointing to some parent
    * path. Otherwise it will be interpreted as a relative path.
    */
  def apply (str: String): RelativePath = {
    require(!str.headOption.contains('/'), "path must not be absolute")
    str.stripSuffix("/") match {
      case "" | "." => Current
      case other =>
        @tailrec def countParents(current: Int, path: String): (Int, String) = 
          if (path.startsWith("..")) countParents(current + 1, path.drop(2).stripPrefix("/"))
          else (current, path)
        val (levels, rest) = countParents(0, other)
        val segments = if (rest.isEmpty) Nil else rest.split("/").toList
        NonEmptyList.fromList(segments)
          .fold[RelativePath](Parent(levels))(SegmentedRelativePath(_, levels))
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
