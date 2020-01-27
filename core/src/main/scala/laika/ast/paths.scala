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

import cats.data.NonEmptyList
import laika.ast.Path.Root
import laika.ast.RelativePath.{Current, Parent}

import scala.annotation.tailrec

/** The abstract base for absolute and relative paths. */
sealed trait PathBase extends Product with Serializable {

  /** The local name of this path.
    */
  def name: String

  /** The base name of this path, without the suffix (if present).
    */
  def basename: String = name

  /** The suffix of `None` if this path name does not have a file suffix
    * separated by a `.`.
    */
  def suffix: Option[String] = None
  
}

/** The common base for absolute and relative paths that contain one or more path segments. */
sealed trait SegmentedPathBase extends PathBase {

  /** The segments representing this path instance. */
  def segments: NonEmptyList[String]

  lazy val name: String = segments.last

  override lazy val basename: String = if (name.contains('.')) name.take(name.lastIndexOf(".")) else name

  override lazy val suffix: Option[String] = if (name.contains('.')) Some(name.drop(name.lastIndexOf(".")+1)) else None
  
}

object PathBase {
  
  def parse (path: String): PathBase = if (path.startsWith("/")) Path.parse(path) else RelativePath.parse(path)
  
}

/** Represents a path inside a virtual tree of documents.
 */
sealed trait Path extends PathBase {

  /** The parent of this path.
   *  Will return this if this path represents a root node.
   */
  def parent: Path

  /** The depth of this path from the virtual root.
    */
  def depth: Int

  /** Returns a new path that either replaces the existing suffix
    * with the specified one or appends it if this path does not have a suffix yet.
    */
  def withSuffix (suffix: String): Path = this

  /** Creates a new path with the specified name
   *  as an immediate child of this path.
   */
  def / (name: String): Path

  /** Combines this path with the specified relative path.
   */
  def / (path: RelativePath): Path

  /** Interprets this path as a relative path - a shortcut
    * for `relativeTo(Root)`.
    */
  def relative: RelativePath = relativeTo(Root)

  /** Interprets this path relative to some other path.
   */
  def relativeTo (path: Path): RelativePath
  
  def isSubPath (other: Path): Boolean
  
}

case class SegmentedPath (segments: NonEmptyList[String]) extends Path with SegmentedPathBase {
  
  val depth: Int = segments.size
  
  lazy val parent: Path = NonEmptyList.fromList(segments.init).fold[Path](Root)(SegmentedPath)

  def / (name: String): Path = SegmentedPath(segments :+ name)

  def / (path: RelativePath): Path = {
    val otherSegments = path match {
      case SegmentedRelativePath(s, _) => s.toList
      case _ => Nil
    }
    val combinedSegments = segments.toList.dropRight(path.parentLevels) ++ otherSegments
    Path(combinedSegments)
  }
  
  def relativeTo (path: Path): RelativePath = {
    
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

  def isSubPath (other: Path): Boolean = other match {
    case Root => true
    case SegmentedPath(otherSegments) => segments.toList.startsWith(otherSegments.toList)
  } 
  
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
  def parse (str: String): Path = {
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

  def withSuffix (newSuffix: String): RelativePath = this
}

case class SegmentedRelativePath(segments: NonEmptyList[String], parentLevels: Int = 0) extends RelativePath with SegmentedPathBase {

  lazy val parent: RelativePath = {
    def noSegments = if (parentLevels == 0) Current else Parent(parentLevels)
    NonEmptyList.fromList(segments.init)
      .fold[RelativePath](noSegments)(SegmentedRelativePath(_, parentLevels))
  }

  def / (name: String): RelativePath = SegmentedRelativePath(segments :+ name, parentLevels)

  def / (path: RelativePath): RelativePath = {

    def construct(otherSegments: List[String], otherLevels: Int): RelativePath = {
      
      val newParentLevels = parentLevels + Math.max(0, otherLevels - segments.size)
      
      def noSegments = if (newParentLevels == 0) Current else Parent(newParentLevels)
      
      NonEmptyList.fromList(segments.toList.dropRight(otherLevels) ++ otherSegments).fold(noSegments){ newSegments =>
        SegmentedRelativePath(newSegments, newParentLevels)
      }
    }
    
    path match {
      case Current => this
      case Parent(otherLevels) => construct(Nil, otherLevels)
      case SegmentedRelativePath(otherSegments, otherLevels) => construct(otherSegments.toList, otherLevels)
    }
  }

  override lazy val basename: String = if (name.contains('.')) name.take(name.lastIndexOf(".")) else name

  override lazy val suffix: Option[String] = if (name.contains('.')) Some(name.drop(name.lastIndexOf(".")+1)) else None
  
  override def withSuffix (newSuffix: String): RelativePath = 
    if (name.endsWith(newSuffix)) this
    else SegmentedRelativePath(NonEmptyList.ofInitLast(segments.init, basename + "." + newSuffix), parentLevels)

  override lazy val toString: String = ("../" * parentLevels) + (segments.toList mkString "/")
}

object RelativePath {

  /** Represent the current path.
    */
  case object Current extends RelativePath {
    val name = "."
    val parent: RelativePath = Parent(1)
    val parentLevels: Int = 0
    def / (name: String): RelativePath = SegmentedRelativePath(NonEmptyList.of(name))
    def / (path: RelativePath): RelativePath = path
    override val toString: String = name
  }

  /** Represent a parent path that is the specified number of levels above the current path.
    */
  case class Parent(parentLevels: Int) extends RelativePath {
    val name: String = "../" * parentLevels
    lazy val parent: RelativePath = Parent(parentLevels + 1)
    def / (name: String): RelativePath = SegmentedRelativePath(NonEmptyList.of(name), parentLevels)
    def / (path: RelativePath): RelativePath = path match {
      case Current => this
      case Parent(otherLevels) => Parent(parentLevels + otherLevels)
      case SegmentedRelativePath(segments, otherLevels) => SegmentedRelativePath(segments, parentLevels + otherLevels)
    }
    override val toString: String = name
  }

  /** Creates a relative path from interpreting the specified string representation.
    * If it starts with `/` it will be interpreted as an absolute path,
    * if it starts with `../` as a relative path pointing to some parent
    * path. Otherwise it will be interpreted as a relative path.
    */
  def parse (str: String): RelativePath = {
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
