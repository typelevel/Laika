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

import cats.data.{Chain, NonEmptyChain}
import cats.implicits._
import laika.ast.Path.Root
import laika.ast.RelativePath.{CurrentDocument, CurrentTree, Parent}

import scala.annotation.tailrec

/** Generic base trait for all path abstractions in Laika.
  * 
  * The most commonly used sub-type is `VirtualPath` which is used to assign paths to trees and documents
  * within a Laika transformation.
  * 
  * This trait is the only one within the Path API that is not sealed,
  * to allow for implementations in other modules (e.g. `FilePath` in `laika-io`).
  */
trait GenericPath {

  type Self <: GenericPath

  /** The local name of this path, without the optional fragment part, but including the suffix if present.
    */
  def name: String

  /** The base name of this path, without the suffix (if present).
    */
  def basename: String = name

  /** The suffix of `None` if this path name does not have a file suffix
    * separated by a `.`.
    */
  def suffix: Option[String]

  /** The fragment part of the path (after a `#` in the last segment),
    * or `None` if this path does not have a fragment component.
    */
  def fragment: Option[String]

  /** Returns a new path that replaces the base name with the specified
    * new name while keeping both, suffix and fragment, in case they are present.
    */
  def withBasename (name: String): Self = copyWith(basename = name)

  /** Returns a new path that either replaces the existing suffix
    * with the specified one or appends it if this path does not have a suffix yet.
    */
  def withSuffix (suffix: String): Self = copyWith(suffix = Some(suffix))

  /** Returns a new path that either replaces the existing fragment component
    * with the specified one or appends it if this path does not have a component yet.
    */
  def withFragment (fragment: String): Self = copyWith(fragment = Some(fragment))

  /** Returns a new path that discards this path's suffix, if present.
    */
  def withoutSuffix: Self = copyWith(suffix = None)

  /** Returns a new path that discards this path's fragment, if present.
    */
  def withoutFragment: Self = copyWith(fragment = None)

  protected def copyWith (basename: String = basename,
                          suffix: Option[String] = suffix,
                          fragment: Option[String] = fragment) : Self

  /** Creates a new path with the specified name
    *  as an immediate child of this path.
    */
  def / (name: String): Self = this / RelativePath.parse(name)

  /** Combines this path with the specified relative path.
    */
  def / (path: RelativePath): Self
  
}

/** The abstract base for absolute and relative paths within Laika's virtual path abstraction.
  * 
  * A path in Laika is always virtual and not pointing to a path in the file system, 
  * even if the data was obtained by scanning a directory. 
  * This is because in Laika transformation input can come from different sources,
  * e.g. from two different directories merged into a single virtual tree in memory
  * with some additional documents added programmatically without any file system reference.
  */
sealed trait VirtualPath extends GenericPath with Product with Serializable {
  
  type Self <: VirtualPath

}

/** The common base for absolute and relative paths that contain one or more path segments. */
sealed trait SegmentedVirtualPath extends VirtualPath {

  /** The segments representing this path instance. The last segment does not include the suffix or fragment parts */
  def segments: NonEmptyChain[String]

  lazy val name: String = suffix.fold(segments.last)(suf => s"${segments.last}.$suf")

  override lazy val basename: String = segments.last
  
  protected def pathPrefix: String
  
  override def toString: String = 
    pathPrefix + (segments.toList mkString "/") + suffix.fold("")("." + _) + fragment.fold("")("#" + _)

}

object SegmentedVirtualPath {

  private[laika] def parseLastSegment (segments: List[String]): (Option[NonEmptyChain[String]], Option[String], Option[String]) = 
    segments.lastOption.fold[(Option[NonEmptyChain[String]], Option[String], Option[String])]((None, None, None)) { lastSegment =>

      def splitAtLast(in: String, char: Char): (String, Option[String]) =
        in.split(char).toSeq match {
          case Seq(single)  => (single, None)
          case init :+ last => (init.mkString(char.toString), Some(last))
        }

      def splitAtFirst(in: String, char: Char): (String, Option[String]) =
        in.split(char).toSeq match {
          case Seq(single)  => (single, None)
          case first +: rest => (first, Some(rest.mkString(char.toString)))
        }
  
      val (name, fragment)   = splitAtLast(lastSegment, '#')
      val (basename, suffix) = splitAtFirst(name, '.')
  
      (Some(NonEmptyChain.fromChainAppend(Chain.fromSeq(segments.init), basename)), suffix, fragment)
    }
  
}

object VirtualPath {

  /** Creates path from interpreting the specified string representation.
    *
    * A path with a slash prefix will be interpreted as absolute, all other input is
    * interpreted as a relative path.
    *
    * Empty path segments are allowed, but should usually be avoided for usability reasons.
    */
  def parse (path: String): VirtualPath = if (path.startsWith("/")) Path.parse(path) else RelativePath.parse(path)
  
}

/** Represents an absolute path inside a virtual tree of documents.
  * 
  * Since this is the most commonly used path abstraction in Laika it received a conveniently short type name.
  * The full, accurate name of this type would be `AbsoluteVirtualPath`.
  */
sealed trait Path extends VirtualPath {
  
  type Self = Path

  /** The parent of this path.
   *  Will return this if this path represents a root node.
   */
  def parent: Path

  /** The depth of this path from the virtual root.
    */
  def depth: Int

  /** Interprets this path as a relative path - a shortcut
    * for `relativeTo(Root)`.
    */
  def relative: RelativePath = relativeTo(Root)

  /** Interprets this path relative to some other path.
    * 
    * The reference path is interpreted as a document path,
    * as relative paths are most commonly used from the perspective of a document, not a tree node.
    */
  def relativeTo (refPath: Path): RelativePath
  
  /** Indicates whether this path is a sub-path of (or identical to) the specified other path. 
    */
  def isSubPath (other: Path): Boolean
  
}

case class SegmentedPath (segments: NonEmptyChain[String], suffix: Option[String] = None, fragment: Option[String] = None) extends Path with SegmentedVirtualPath {
  
  val depth: Int = segments.length.toInt
  
  lazy val parent: Path = NonEmptyChain.fromChain(segments.init).fold[Path](Root)(SegmentedPath(_))

  def / (path: RelativePath): Path = {
    val (otherSegments, otherSuffix, otherFragment, thisSuffix) = path match {
      case SegmentedRelativePath(s, suf, frag, _) => (s.toList, suf, frag, suffix)
      case CurrentDocument(frag) => (Nil, suffix, frag, None)
      case _ => (Nil, None, None, None)
    }
    val thisSegments = segments.toList.dropRight(1) :+ (segments.last + thisSuffix.fold("")("." + _))
    val combinedSegments = thisSegments.dropRight(path.parentLevels) ++ otherSegments
    NonEmptyChain.fromSeq(combinedSegments).fold[Path](Root)(SegmentedPath(_, otherSuffix, otherFragment))
  }
  
  def relativeTo (path: Path): RelativePath = {

    val refPath = if (path.isSubPath(withoutFragment)) path else path.parent
    
    @tailrec
    def removeCommonParts (a: List[String], b: List[String]): (List[String],List[String]) = (a,b) match {
      case (p1 :: rest1, p2 :: rest2) if p1 == p2 => removeCommonParts(rest1,rest2)
      case _ => (a,b)
    }
    val (a, b) = refPath match {
      case Root => (Nil, segments.init.toList :+ name)
      case other: SegmentedPath => removeCommonParts(other.segments.init.toList :+ other.name, segments.init.toList :+ name)
    } 
    val segmentRest = segments.toList.drop(segments.size.toInt - b.size)
    NonEmptyChain.fromSeq(segmentRest).fold[RelativePath] {
      val base = if (a.isEmpty) CurrentDocument() else Parent(a.length)
      fragment.fold[RelativePath](base)(base.withFragment)
    } { seg =>
      val base = if (a.isEmpty) CurrentTree else Parent(a.length)
      base / SegmentedRelativePath(seg, suffix, fragment)
    }
  }

  def isSubPath (other: Path): Boolean = other match {
    case Root => true
    case SegmentedPath(otherSegments, otherSuffix, otherFragment) => 
      if (segments.length == otherSegments.length) 
        segments == otherSegments && 
        suffix == otherSuffix && 
        (fragment == otherFragment || otherFragment.isEmpty)
      else 
        segments.toList.startsWith(otherSegments.toList) && 
        otherSuffix.isEmpty && 
        otherFragment.isEmpty
  }
  
  protected def copyWith (basename: String = basename,
                          suffix: Option[String] = suffix,
                          fragment: Option[String] = fragment): Path = {
    copy(
      segments = NonEmptyChain.fromChainAppend(segments.init, basename),
      suffix = suffix,
      fragment = fragment
    )
  }
  
  protected val pathPrefix: String = "/"
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
    val suffix: Option[String] = None
    val fragment: Option[String] = None

    protected def copyWith (basename: String = basename,
                            suffix: Option[String] = suffix,
                            fragment: Option[String] = fragment): Path = this
    
    def / (path: RelativePath): Path = path match {
      case SegmentedRelativePath(segments, suf, frag, _) => SegmentedPath(segments, suf, frag)
      case _ => this
    }
    def relativeTo (path: Path): RelativePath = path match {
      case Root => CurrentTree
      case SegmentedPath(segments, _, _) => Parent(segments.length.toInt)
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
  def parse (str: String): Path =
    if (str == "/") Root
    else {
      val segments = str.stripPrefix("/").stripSuffix("/").split("/").toList        
      val parts = 
        if (str.endsWith("/")) (NonEmptyChain.fromSeq(segments), None, None)
        else SegmentedVirtualPath.parseLastSegment(segments)
      parts match {
        case (Some(seg), suf, frag) => SegmentedPath(seg, suf, frag)
        case _ => Root
      }  
    }
  
  def apply (segments: List[String]): Path = SegmentedVirtualPath.parseLastSegment(segments) match {
    case (Some(seg), suf, frag) => SegmentedPath(seg, suf, frag)
    case _ => Root
  }

}

sealed trait RelativePath extends VirtualPath {

  type Self = RelativePath
  
  /** The parent of this path.
    */
  def parent: RelativePath

  /** The number of levels this relative path points above the current level.
    */
  def parentLevels: Int

  protected def copyWith (basename: String = basename,
                          suffix: Option[String] = suffix,
                          fragment: Option[String] = fragment): RelativePath = this
}

case class SegmentedRelativePath(segments: NonEmptyChain[String], 
                                 suffix: Option[String] = None, 
                                 fragment: Option[String] = None, 
                                 parentLevels: Int = 0) extends RelativePath with SegmentedVirtualPath {

  lazy val parent: RelativePath = {
    def noSegments = if (parentLevels == 0) CurrentTree else Parent(parentLevels)
    NonEmptyChain.fromSeq(segments.toList.init)
      .fold[RelativePath](noSegments)(seg => copy(segments = seg, suffix = None, fragment = None))
  }

  def / (path: RelativePath): RelativePath = {

    def construct(otherSegments: List[String], otherSuffix: Option[String], otherFragment: Option[String], otherLevels: Int): RelativePath = {
      
      val newParentLevels = parentLevels + Math.max(0, otherLevels - segments.size.toInt)
      
      def noSegments: RelativePath = if (newParentLevels == 0) CurrentTree else Parent(newParentLevels)
      
      NonEmptyChain.fromSeq(segments.toList.dropRight(otherLevels) ++ otherSegments).fold(noSegments){ newSegments =>
        SegmentedRelativePath(newSegments, otherSuffix, otherFragment, newParentLevels)
      }
    }
    
    path match {
      case CurrentTree | CurrentDocument(None) => this
      case CurrentDocument(Some(fr))           => withFragment(fr)
      case Parent(otherLevels)                 => construct(Nil, None, None, otherLevels)
      case p: SegmentedRelativePath            => construct(p.segments.toList, p.suffix, p.fragment, p.parentLevels)
    }
  }

  override protected def copyWith (basename: String = basename,
                                   suffix: Option[String] = suffix,
                                   fragment: Option[String] = fragment): RelativePath = copy(
    segments = NonEmptyChain.fromChainAppend(segments.init, basename),
    suffix = suffix,
    fragment = fragment
  )
  
  protected val pathPrefix: String = "../" * parentLevels
}

object RelativePath {

  /** Represent the current tree node.
    */
  case object CurrentTree extends RelativePath {
    val name = "."
    val parent: RelativePath = Parent(1)
    val parentLevels: Int = 0
    val suffix: Option[String] = None
    val fragment: Option[String] = None
    def / (path: RelativePath): RelativePath = path
    override val toString: String = name
  }

  /** Represent the current document.
    */
  case class CurrentDocument (fragment: Option[String] = None) extends RelativePath {
    val name = ""
    val parent: RelativePath = CurrentTree
    val parentLevels: Int = 0
    val suffix: Option[String] = None
    def / (path: RelativePath): RelativePath = this
    override def withFragment (fragment: String): RelativePath = copy(Some(fragment))
    override val toString: String = s"#${fragment.getOrElse("")}"
  }
  object CurrentDocument {
    def apply (fragment: String): CurrentDocument = apply(Some(fragment))
  }

  /** Represent a parent path that is the specified number of levels above the current path.
    */
  case class Parent(parentLevels: Int) extends RelativePath {
    val name: String = "../" * parentLevels
    val suffix: Option[String] = None
    val fragment: Option[String] = None
    lazy val parent: RelativePath = Parent(parentLevels + 1)
    def / (path: RelativePath): RelativePath = path match {
      case Parent(otherLevels) => Parent(parentLevels + otherLevels)
      case p: SegmentedRelativePath => SegmentedRelativePath(p.segments, p.suffix, p.fragment, parentLevels + p.parentLevels)
      case _ => this
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
      case "" | "#" => CurrentDocument()
      case "." => CurrentTree
      case other if other.startsWith("#") => CurrentDocument(other.drop(1))
      case other =>
        @tailrec def countParents(current: Int, path: String): (Int, String) = 
          if (path.startsWith("..")) countParents(current + 1, path.drop(2).stripPrefix("/"))
          else (current, path)
        val (levels, rest) = countParents(0, other)

        val segments = rest.split("/").toList
        val parts = {
          if (rest.isEmpty) (None, None, None)
          else if (str.endsWith("/")) (NonEmptyChain.fromSeq(segments), None, None)
          else SegmentedVirtualPath.parseLastSegment(segments)
        }
        parts match {
          case (Some(seg), suf, frag) => SegmentedRelativePath(seg, suf, frag, levels)
          case _ => Parent(levels)
        }
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
    case _: SegmentedRelativePath => Some((p.parent, p.name))
    case _ => None
  }
}
