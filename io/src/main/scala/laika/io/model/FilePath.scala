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

import java.nio.file.{InvalidPathException, Paths}

/** Represents an absolute path on the file system, pointing to a file or directory that may or may not exist.
  * 
  * This type has a lot of shared API with the `VirtualPath` abstraction in `laika-core` via their
  * common super-trait `GenericPath`.
  * Like the virtual path API it comes with convenience methods for querying and modifying
  * suffix and fragment components, a common requirement in processing internal links for example.
  * 
  * However, it differs in three ways from the virtual path abstraction: first, semantically,
  * as the latter is never intended to represent an actual file
  * and instead just describes a tree structure of inputs (and AST documents after parsing).
  * 
  * Secondly, this type comes with additional APIs to convert to and from other path representations,
  * namely `java.io.File`, `java.nio.file.Path` and `fs2.io.file.Path`.
  *
  * And finally, the `toString` method for this type returns a platform-dependent string representation
  * by using the file separator of the underlying file system.
  * Laika's virtual path abstractions on the other hand always uses a forward slash `/` as the separator.
  * 
  * Having a dedicated file path abstraction also helps with type signatures for methods in Laika's APIs 
  * that accept two path arguments: a file path and a virtual path indicating the mount point 
  * (at which the specified file is supposed to be inserted into the virtual tree).
  * 
  * @author Jens Halm
  */
class FilePath private (private val root: String, private val underlying: Path) extends GenericPath {

  type Self = FilePath
  
  override val basename: String = underlying.basename
  
  def name: String = underlying.name

  def suffix: Option[String] = underlying.suffix

  def fragment: Option[String] = underlying.fragment

  protected def copyWith (basename: String, suffix: Option[String], fragment: Option[String]): FilePath =
    underlying match {
      case Root => this
      case sp: SegmentedPath => new FilePath(root, sp.copy(
        segments = NonEmptyChain.fromChainAppend(sp.segments.init, basename),
        suffix = suffix,
        fragment = fragment
      ))
    }

  def / (path: RelativePath): FilePath = new FilePath(root, underlying / path)

  /** Converts this `FilePath` to a `java.nio.file.Path`.
    */
  def toNioPath: java.nio.file.Path = underlying match {
    case Root => Paths.get(root)
    case sp: SegmentedPath => 
      val last = sp.name + fragment.fold("")("#" + _)
      val segments = sp.segments.init.append(last).toList
      Paths.get(root, segments:_*)
  }

  /** Converts this `FilePath` to an `fs2.io.file.Path`.
    */
  def toFS2Path: fs2.io.file.Path = fs2.io.file.Path.fromNioPath(toNioPath)

  /** Converts this `FilePath` to an `java.io.File` instance.
    */
  def toJavaFile: java.io.File = toNioPath.toFile
  
  override def toString: String = toNioPath.toString

  override def equals (other: Any): Boolean = other match {
    case fp: FilePath => fp.root == root && fp.underlying == underlying
    case _ => false
  }

  override def hashCode (): Int = underlying.hashCode()
  
}

/** Companion for parsing path strings or creating `FilePath` instances from other
  * path representations.
  */
object FilePath {

  /** Creates a new `FilePath` from the specified NIO path after normalizing it.
    * The provided path needs to be absolute, for relative paths use `laika.ast.RelativePath`.
    */
  def fromNioPath (path: java.nio.file.Path): FilePath = {
    if (!path.isAbsolute) throw new InvalidPathException(path.toString, "File paths must be absolute")
    val root = Option(path.getRoot).fold("")(_.toString)
    val segments = JIteratorWrapper(path.normalize().iterator()).toList.map(_.toString)
    new FilePath(root, Path.apply(segments))
  }

  /** Creates a new `FilePath` from the specified fs2 path after normalizing it.
    * The provided path needs to be absolute, for relative paths use `laika.ast.RelativePath`.
    */
  def fromFS2Path (path: fs2.io.file.Path): FilePath = fromNioPath(path.toNioPath)

  /** Creates a new `FilePath` from the specified File instance after normalizing its path.
    * The provided file needs to be absolute, for relative paths use `laika.ast.RelativePath`.
    */
  def fromJavaFile (file: java.io.File): FilePath = fromNioPath(file.toPath)

  /** Creates a new `FilePath` by parsing the specified path string in a platform-specific manner.
    * The provided path string needs to be absolute, for relative paths use `laika.ast.RelativePath`.
    */
  def parse (path: String): FilePath = fromNioPath(Paths.get(path))
  
}