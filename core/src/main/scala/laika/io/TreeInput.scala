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

package laika.io

import java.io.File

import laika.ast.Path.Root
import laika.ast.{DocumentType, Path, TextDocumentType}

import scala.io.Codec

/** Represents a tree structure of Inputs, abstracting over various types of IO resources. 
 *  
 *  While the default implementations wrap the structure of directories in the file system,
 *  other implementations may build an entirely virtual input tree structure. 
 * 
 *  @author Jens Halm
 */
trait TreeInput {

  // TODO - move and extend Input
  
  /** The local name of the tree represented by this tree.
   */
  lazy val name: String = path.name

  /** The full path of the tree represented by this tree.
   *  This path is always an absolute path
   *  from the root of the (virtual) input tree,
   *  therefore does not represent the filesystem
   *  path in case of file I/O.
   */
  def path: Path = Root

  /** All textual inputs (recursively) .
   */
  def textInputs: Seq[TextInput]

  /** All binary inputs (recursively) .
    */
  def binaryInputs: Seq[BinaryInput]
  
  /** The paths this tree has been created from
   *  or an empty list if this input tree does
   *  not originate from the file system.
   */
  def sourcePaths: Seq[String]

}


/** Factory methods for creating `InputTree` instances.
 */
object TreeInput {

  type FileFilter = File => Boolean

  case class Directory (path: Path, file: File)

  private class DirectoryInputTree(rootDirs: Seq[File], exclude: FileFilter, docTypeMatcher: Path => DocumentType, codec: Codec) extends TreeInput {

    import DocumentType._
    
    private lazy val allDirs: Seq[Directory] = {
      
      def collectSubDirectories (dir: Directory): Seq[Directory] = {
        val subDirs = dir.file.listFiles filter (f => f.isDirectory && !exclude(f) && docTypeMatcher(dir.path / f.getName) != Ignored) map (d => Directory(dir.path / d.getName, d))
        dir +: subDirs.flatMap(collectSubDirectories)
      }
      
      rootDirs.map(d => Directory(Root, d)).flatMap(collectSubDirectories)
    }

    private lazy val allFiles: Seq[(DocumentType, Path, File)] = {
      def filesInDir (dir: Directory): Array[(DocumentType, Path, File)] = dir.file.listFiles collect {
        case file if file.isFile && !exclude(file) => (docTypeMatcher(dir.path / file.getName), dir.path / file.getName, file)
      }
      allDirs.flatMap(filesInDir)
    }
    
    lazy val textInputs: Seq[TextInput] = allFiles.collect {
      case (docType: TextDocumentType, filePath, file) => TextFileInput(file, docType, filePath, codec)
    }
    
    lazy val binaryInputs: Seq[BinaryInput] = allFiles.collect {
      case (Static, filePath, file) => BinaryFileInput(file, filePath)
    }

    lazy val sourcePaths: Seq[String] = rootDirs map (_.getAbsolutePath)

  }

  /**  Creates an instance based on the current working directory, including
    *  all subdirectories.
    *
    *  @param docTypeMatcher a function determining the document type based on the path of the input
    *  @param exclude the files to exclude from processing
    *  @param codec the character encoding of the files, if not specified the platform default will be used
    */
  def forWorkingDirectory (docTypeMatcher: Path => DocumentType, exclude: FileFilter)(implicit codec: Codec): TreeInput =
    forRootDirectories(Seq(new File(System.getProperty("user.dir"))), docTypeMatcher, exclude)

  /** Creates an instance based on the specified directories, including
   *  all subdirectories. The directories will be merged into a tree with a single
   *  root. If any of the specified root directories contain sub-directories with
   *  the same name, these sub-directories will be merged, too.
   *
   *  @param roots the root directories of the input tree
   *  @param docTypeMatcher a function determining the document type based on the path of the input
   *  @param exclude the files to exclude from processing
   *  @param codec the character encoding of the files, if not specified the platform default will be used
   */
  def forRootDirectories (roots: Seq[File], docTypeMatcher: Path => DocumentType, exclude: FileFilter)(implicit codec: Codec): TreeInput = {
    require(roots.nonEmpty, "The specified roots sequence must contain at least one directory")
    for (root <- roots) {
      require(root.exists, s"Directory ${root.getAbsolutePath} does not exist")
      require(root.isDirectory, s"File ${root.getAbsolutePath} is not a directory")
    }

    new DirectoryInputTree(roots, exclude, docTypeMatcher, codec)
  }

  /** Responsible for building new InputTrees based
   *  on the specified document type matcher and codec.
   */
  trait InputTreeBuilder {
    def build (docTypeMatcher: Path => DocumentType): TreeInput
  }
  
  /** A filter that selects files that are hidden according to `java.io.File.isHidden`.
   */
  val hiddenFileFilter: FileFilter = file => file.isHidden && file.getName != "."

}
