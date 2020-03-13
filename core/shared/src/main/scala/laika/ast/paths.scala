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

import cats.data.NonEmptyChain
import cats.implicits._
import laika.ast.Path.Root
import laika.ast.RelativePath.{Current, Parent}

import scala.annotation.tailrec

/** The abstract base for absolute and relative paths.
  * 
  * A path in Laika is always virtual and not pointing
  * to a path in the file system, even if the data was obtained
  * by scanning a directory. This is because in Laika transformation
  * input can come from different sources, e.g. from two different
  * directories merged into a single virtual tree in memory with
  * some additional documents added programmatically without any
  * file system reference.
  */
sealed trait PathBase extends Product with Serializable {

  /** The local name of this path, without the optional fragment part, but including the suffix if present.
    */
  def name: String

  /** The base name of this path, without the suffix (if present).
    */
  def basename: String = name

  /** The suffix of `None` if this path name does not have a file suffix
    * separated by a `.`.
    */
  def suffix: Option[String] = None

  /** The fragment part of the path (after a `#` in the last segment),
    * or `None` if this path does not have a fragment component.
    */
  def fragment: Option[String] = None
  
}

/** The common base for absolute and relative paths that contain one or more path segments. */
sealed trait SegmentedPathBase extends PathBase {

  /** The segments representing this path instance. */
  def segments: NonEmptyChain[String]

  lazy val name: String = fragment.fold(segments.last){ fr => 
    segments.last.dropRight(fr.length + 1) 
  }

  override lazy val basename: String = suffix.fold(name){ sf =>
    name.dropRight(sf.length + 1)
  }

  override lazy val suffix: Option[String] = name.split('.').tail.lastOption

  override lazy val fragment: Option[String] = segments.last.split('#').tail.lastOption
  
}

object PathBase {

  /** Creates path from interpreting the specified string representation.
    *
    * A path with a slash prefix will be interpreted as absolute, all other input is
    * interpreted as a relative path.
    *
    * Empty path segments are allowed, but should usually be avoided for usability reasons.
    */
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

  /** Returns a new path that either replaces the existing fragment component
    * with the specified one or appends it if this path does not have a component yet.
    */
  def withFragment (fragment: String): Path = this

  /** Returns a new path that discards this path's suffix, if present.
    */
  def withoutSuffix: Path = this

  /** Returns a new path that discards this path's fragment, if present.
    */
  def withoutFragment: Path = this

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

case class SegmentedPath (segments: NonEmptyChain[String]) extends Path with SegmentedPathBase {
  
  val depth: Int = segments.length.toInt
  
  lazy val parent: Path = NonEmptyChain.fromChain(segments.init).fold[Path](Root)(SegmentedPath)

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
      case SegmentedPath(otherSegments) => removeCommonParts(otherSegments.toList, segments.init.toList :+ name)
    } 
    val base = if (a.isEmpty) Current else Parent(a.length)
    val pathWithoutFragment = NonEmptyChain.fromSeq(b).fold[RelativePath](base)(seg => base / SegmentedRelativePath(seg))
    fragment.fold[RelativePath](pathWithoutFragment){ fm =>
      pathWithoutFragment match {
        case Current => Current / s"#$fm" // TODO - Current.withFragment should work
        case p => p.withFragment(fm)
      }
    }
  }

  def isSubPath (other: Path): Boolean = other match {
    case Root => true
    case SegmentedPath(otherSegments) => segments.toList.startsWith(otherSegments.toList)
  } 
  
  override def withSuffix (newSuffix: String): Path = 
    if (suffix.contains(newSuffix)) this 
    else SegmentedPath(NonEmptyChain.fromChainAppend(segments.init, s"$basename.$newSuffix" + fragment.fold("")("#"+_)))

  override def withFragment (newFragment: String): Path =
    if (fragment.contains(newFragment)) this
    else SegmentedPath(NonEmptyChain.fromChainAppend(segments.init, s"$name#$newFragment"))

  override def withoutSuffix: Path = suffix.fold(this)(_ =>
    SegmentedPath(NonEmptyChain.fromChainAppend(segments.init, s"$basename${fragment.fold("")("#"+_)}"))
  )

  override def withoutFragment: Path = fragment.fold(this)(_ =>
    SegmentedPath(NonEmptyChain.fromChainAppend(segments.init, name))
  )
  
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
    def / (name: String): Path = SegmentedPath(NonEmptyChain(name))
    def / (path: RelativePath): Path = path match {
      case SegmentedRelativePath(segments, _) => SegmentedPath(segments)
      case _ => this
    }
    def relativeTo (path: Path): RelativePath = path match {
      case Root => Current
      case SegmentedPath(segments) => Parent(segments.length.toInt)
    }
    def isSubPath (other: Path): Boolean = other == Root
    override val toString: String = "/"
  }

  /** Creates an absolute path from interpreting the specified string representation.
    * 
    * A slash prefix, even if not present, will be assumed. 
    * 
    * If you need to parse a string that can potentially represent both, an absolute or a relative
    * path, use the `PathBase.parse` method instead.
    * 
    * Empty path segments are allowed, but should usually be avoided for usability reasons.
    */
  def parse (str: String): Path = {
    str match {
      case "/"   => Root
      case other => apply(other.stripPrefix("/").stripSuffix("/").split("/").toList)
    }
  }

  def apply (segments: List[String]): Path = NonEmptyChain.fromSeq(segments).fold[Path](Root)(SegmentedPath)

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

  /** Returns a new path that either replaces the existing suffix
    * with the specified one or appends it if this path does not have a suffix yet.
    */
  def withSuffix (newSuffix: String): RelativePath = this

  /** Returns a new path that either replaces the existing fragment component
    * with the specified one or appends it if this path does not have a component yet.
    */
  def withFragment (fragment: String): RelativePath = this

  /** Returns a new path that discards this path's suffix, if present.
    */
  def withoutSuffix: RelativePath = this

  /** Returns a new path that discards this path's fragment, if present.
    */
  def withoutFragment: RelativePath = this
}

case class SegmentedRelativePath(segments: NonEmptyChain[String], parentLevels: Int = 0) extends RelativePath with SegmentedPathBase {

  lazy val parent: RelativePath = {
    def noSegments = if (parentLevels == 0) Current else Parent(parentLevels)
    NonEmptyChain.fromSeq(segments.toList.init)
      .fold[RelativePath](noSegments)(SegmentedRelativePath(_, parentLevels))
  }

  def / (name: String): RelativePath = SegmentedRelativePath(segments :+ name, parentLevels)

  def / (path: RelativePath): RelativePath = {

    def construct(otherSegments: List[String], otherLevels: Int): RelativePath = {
      
      val newParentLevels = parentLevels + Math.max(0, otherLevels - segments.size.toInt)
      
      def noSegments = if (newParentLevels == 0) Current else Parent(newParentLevels)
      
      NonEmptyChain.fromSeq(segments.toList.dropRight(otherLevels) ++ otherSegments).fold(noSegments){ newSegments =>
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
    if (suffix.contains(newSuffix)) this
    else SegmentedRelativePath(NonEmptyChain.fromChainAppend(segments.init, s"$basename.$newSuffix" + fragment.fold("")("#"+_)), parentLevels)

  override def withFragment (newFragment: String): RelativePath =
    if (fragment.contains(newFragment)) this
    else SegmentedRelativePath(NonEmptyChain.fromChainAppend(segments.init, s"$name#$newFragment"), parentLevels)

  override def withoutSuffix: RelativePath = suffix.fold(this)(_ =>
    SegmentedRelativePath(NonEmptyChain.fromChainAppend(segments.init, s"$basename${fragment.fold("")("#"+_)}"), parentLevels)
  )

  override def withoutFragment: RelativePath = fragment.fold(this)(_ =>
    SegmentedRelativePath(NonEmptyChain.fromChainAppend(segments.init, name), parentLevels)
  )
  
  override lazy val toString: String = ("../" * parentLevels) + (segments.toList mkString "/")
}

object RelativePath {

  /** Represent the current path.
    */
  case object Current extends RelativePath {
    val name = "."
    val parent: RelativePath = Parent(1)
    val parentLevels: Int = 0
    def / (name: String): RelativePath = SegmentedRelativePath(NonEmptyChain(name))
    def / (path: RelativePath): RelativePath = path
    override def withFragment (fragment: String): RelativePath = SegmentedRelativePath(NonEmptyChain(s"#fragment"))
    override val toString: String = name
  }

  /** Represent a parent path that is the specified number of levels above the current path.
    */
  case class Parent(parentLevels: Int) extends RelativePath {
    val name: String = "../" * parentLevels
    lazy val parent: RelativePath = Parent(parentLevels + 1)
    def / (name: String): RelativePath = SegmentedRelativePath(NonEmptyChain(name), parentLevels)
    def / (path: RelativePath): RelativePath = path match {
      case Current => this
      case Parent(otherLevels) => Parent(parentLevels + otherLevels)
      case SegmentedRelativePath(segments, otherLevels) => SegmentedRelativePath(segments, parentLevels + otherLevels)
    }
    override val toString: String = name
  }

  /** Creates a relative path from interpreting the specified string representation.
    * 
    * A slash prefix, if present, will be discarded. Paths starting with one or more `../`
    * path prefixes will be interpreted as expected, pointing to parent trees. 
    * 
    * If you need to parse a string that can potentially represent both, an absolute or a relative
    * path, use the `PathBase.parse` method instead.
    * 
    * Empty path segments are allowed, but should usually be avoided for usability reasons.
    */
  def parse (str: String): RelativePath = {
    str.stripPrefix("/").stripSuffix("/") match {
      case "" | "." => Current
      case other =>
        @tailrec def countParents(current: Int, path: String): (Int, String) = 
          if (path.startsWith("..")) countParents(current + 1, path.drop(2).stripPrefix("/"))
          else (current, path)
        val (levels, rest) = countParents(0, other)
        val segments = if (rest.isEmpty) Nil else rest.split("/").toList
        NonEmptyChain.fromSeq(segments)
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
