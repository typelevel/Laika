/*
* Copyright 2012-2022 the original author or authors.
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

package laika.io.model

import cats.data.NonEmptyChain
import laika.ast.Path.Root
import laika.ast.{GenericPath, Path, RelativePath, SegmentedPath}
import laika.collection.TransitionalCollectionOps.JIteratorWrapper

import java.nio.file.Paths

/** Represents a path on the file system, pointing to a file or directory that may or may not exist.
  * 
  * This type has a lot of shared API with the `VirtualPath` abstraction in `laika-core` via their
  * common super-trait `GenericPath`.
  * 
  * However, it differs in two ways from the virtual path abstraction: semantically, as the latter
  * is never intended to represent an actual file and instead just describes a tree structure
  * of inputs (and AST documents after parsing). 
  * 
  * Secondly, it comes with additional APIs to convert to and from other path representations,
  * namely `java.io.File`, `java.nio.file.Path` and `fs2.io.file.Path`.
  * 
  * @author Jens Halm
  */
class FilePath private (private val underlying: Path) extends GenericPath {

  type Self = FilePath
  
  def name: String = underlying.name

  def suffix: Option[String] = underlying.suffix

  def fragment: Option[String] = underlying.fragment

  protected def copyWith (basename: String, suffix: Option[String], fragment: Option[String]) =
    underlying match {
      case Root => new FilePath(Root)
      case sp: SegmentedPath => new FilePath(sp.copy(
        segments = NonEmptyChain.fromChainAppend(sp.segments.init, basename),
        suffix = suffix,
        fragment = fragment
      ))
    }

  def / (path: RelativePath): FilePath = new FilePath(underlying / path)

  /** Converts this `FilePath` to a `java.nio.file.Path`.
    */
  def toNioPath: java.nio.file.Path = underlying match {
    case Root => Paths.get("/")
    case sp: SegmentedPath => Paths.get(sp.segments.head, sp.segments.tail.toList:_*)
  }

  /** Converts this `FilePath` to an `fs2.io.file.Path`.
    */
  def toFS2Path: fs2.io.file.Path = fs2.io.file.Path.fromNioPath(toNioPath)

  /** Converts this `FilePath` to an `java.io.File` instance.
    */
  def toJavaFile: java.io.File = toNioPath.toFile
  
  override def toString: String = underlying.toString

  override def equals (other: Any): Boolean = other match {
    case fp: FilePath => fp.underlying == underlying
    case _ => false
  }

  override def hashCode (): Int = underlying.hashCode()
  
}

/** Companion for parsing path strings or creating `FilePath` instances from other
  * path representations.
  */
object FilePath {

  /** Creates a new `FilePath` from the specified NIO path after normalizing it.
    */
  def fromNioPath (path: java.nio.file.Path): FilePath = {
    val segments = JIteratorWrapper(path.normalize().iterator()).toList.map(_.toString)
    new FilePath(Path.apply(segments))
  }

  /** Creates a new `FilePath` from the specified fs2 path after normalizing it.
    */
  def fromFS2Path (path: fs2.io.file.Path): FilePath = fromNioPath(path.toNioPath)

  /** Creates a new `FilePath` from the specified File instance after normalizing its path.
    */
  def fromJavaFile (file: java.io.File): FilePath = fromNioPath(file.toPath)

  /** Creates a new `FilePath` by parsing the specified path string in a platform-specific manner.
    */
  def parse (path: String): FilePath = fromNioPath(Paths.get(path))
  
}