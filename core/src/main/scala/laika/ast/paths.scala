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

package laika.ast

import scala.annotation.tailrec

/** Represents a path inside a virtual tree of
 *  documents.
 */
sealed abstract class Path {

  import Path._

  /** The parent of this path.
   *  Will return this if this path represents a root node.
   */
  def parent: Path

  /** The local name of this path.
   */
  def name: String

  /** The first element of this path.
   */
  def prefix: PathPrefix

  /** All components after the prefix as a list of strings.
   */
  def components: List[String]

  /** The depth of this path from the virtual root.
    */
  def depth: Int = components.size

  def isAbsolute: Boolean = prefix == Root

  /** Creates a new path with the specified name
   *  as an immediate child of this path.
   */
  def / (name: String): / = new /(this, name)

  /** Combines this path with the specified path.
   *  If the specified path is a relative path it
   *  will be interpreted relative to this path, if it is absolute
   *  it will be returned unchanged.
   */
  def / (path: Path): Path = path.prefix match {
    case Root => path
    case Current => Path(prefix, components ::: path.components)
    case Parent(1) => parent / Path(Current, path.components)
    case Parent(i) => parent / Path(Parent(i-1), path.components)
  }

  /** Interprets this path relative to some other path.
   */
  def relativeTo (path: Path): Path = {
    def buildRelativePath = {
      def removeCommonParts (a: List[String], b: List[String]): (List[String],List[String]) = (a,b) match {
        case (p1 :: rest1, p2 :: rest2) if p1 == p2 => removeCommonParts(rest1,rest2)
        case _ => (a,b)
      }
      val (a,b) = removeCommonParts(path.components, components)
      val pref = a match {
        case Nil => Current
        case list => Parent(list.length)
      }
      Path(pref, b)
    }
    (isAbsolute, path.isAbsolute) match {
      case (true, true) | (false, false) => buildRelativePath
      case (true, false) => this // there is no other sensible result for this case
      case (false, true) => path / this
    }
  }
  def suffix: String = ""
  def basename: String = name
  
  def withSuffix (suffix: String): Path = this
  
  def isSubPath (other: Path): Boolean = this.prefix == other.prefix && this.components.startsWith(other.components)
}

case class / (parent: Path, name: String) extends Path {
  lazy val components: List[String] = parent.components ++ List(name)
  lazy val prefix: Path.PathPrefix = parent.prefix
  override lazy val basename: String = if (name.contains('.')) name.take(name.lastIndexOf(".")) else name
  override lazy val suffix: String = if (name.contains('.')) name.drop(name.lastIndexOf(".")+1) else ""
  override def withSuffix (newSuffix: String): Path = copy(name = basename + "." + newSuffix)
  override lazy val toString: String = prefix.toString + (components mkString "/")
}

/** Factory methods for creating path instances.
 */
object Path {

  abstract class PathPrefix (val name: String) extends Path {
    val components: List[String] = Nil
    val parent: Path = this
    val prefix: PathPrefix = this
    override val toString: String = name
  }

  /** The root of an absolute path.
    */
  case object Root extends PathPrefix ("/")

  /** The root of a relative path.
    */
  case object Current extends PathPrefix ("")

  /** The prefix of a relative path pointing
    *  to a parent the specified number of levels
    *  above this path.
    */
  case class Parent (levels: Int) extends PathPrefix("../" * levels) {
    require(levels > 0)
  }

  /** Creates path from interpreting the specified string representation.
   *  If it starts with `/` it will be interpreted as an absolute path,
   *  if it starts with `../` as a relative path pointing to some parent
   *  path. Otherwise it will be interpreted as a relative path.
   */
  def apply(str: String): Path = {
    val trimmed = str.trim match {
      case "/" | "../" => str.trim
      case other => other.stripSuffix("/")
    }
    val (parent, rest) =
      if (trimmed.startsWith("/")) (Root, trimmed.drop(1))
      else if (trimmed.startsWith("../")) (Parent(1), trimmed.drop(3))
      else if (trimmed == "..") (Parent(1), "")
      else (Current, trimmed)
     if (rest.isEmpty) parent else apply(parent, rest.split("/").toList)
  }

  /** Creates a path from the specified prefix element and
   *  string components which each of them representing one path element.
   */
  @tailrec def apply (prefix: Path, components: List[String]): Path = (prefix, components) match {
    case (Parent(level), ".." :: rest) => apply(Parent(level+1), rest)
    case (parent, rest) => rest.foldLeft(parent)(_ / _)
  }

  /** Creates an absolute path from the specified path segments.
    */
  def apply (segments: List[String]): Path = apply(Root, segments)
  
}
