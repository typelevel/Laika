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
 
import java.io._

import laika.ast.{DocumentType, Path, TextDocumentType}
import laika.ast.Path.Root
import laika.bundle.DocumentTypeMatcher

import scala.io.Codec

/** Represents the input for a parser, abstracting over various types of IO resources. 
 *  
 *  @author Jens Halm
 */
sealed trait Input {

  /** The full virtual path of this input.
   *  This path is always an absolute path
   *  from the root of the (virtual) input tree,
   *  therefore does not represent the filesystem
   *  path in case of file I/O.
   */
  def path: Path
  
  /** The local name of this input.
   */
  lazy val name: String = path.name

}

/** A marker trait for binary input.
  */
sealed trait BinaryInput extends Input

/** A marker trait for textual input.
  */
sealed trait TextInput extends Input {

  /** The type of the document.
    */
  def docType: TextDocumentType
  
}

sealed trait TreeInput extends Input {

  val path: Path = Root

  /** The paths this tree has been created from
    *  or an empty list if this input tree does
    *  not originate from the file system.
    */
  def sourcePaths: Seq[String]

}

case class StringInput (source: String, docType: TextDocumentType, path: Path = Root) extends TextInput

case class TextFileInput (file: File, docType: TextDocumentType, path: Path, codec: Codec) extends TextInput

case class BinaryFileInput (file: File, path: Path) extends BinaryInput

case class ByteInput (bytes: Array[Byte], path: Path) extends BinaryInput

object ByteInput {
  def apply (input: String, path: Path)(implicit codec: Codec): ByteInput = ByteInput(input.getBytes(codec.charSet), path)
}

case class DirectoryInput (directories: Seq[File],
                           codec: Codec,
                           docTypeMatcher: Path => DocumentType = DocumentTypeMatcher.base,
                           fileFilter: File => Boolean = DirectoryInput.hiddenFileFilter) extends TreeInput {
  lazy val sourcePaths: Seq[String] = directories map (_.getAbsolutePath)
}

object DirectoryInput {
  
  /** A filter that selects files that are hidden according to `java.io.File.isHidden`.
    */
  val hiddenFileFilter: File => Boolean = file => file.isHidden && file.getName != "."
  
  def apply (directory: File)(implicit codec: Codec): DirectoryInput = DirectoryInput(Seq(directory), codec)
}

case class InputCollection (textInputs: Seq[TextInput], binaryInputs: Seq[BinaryInput] = Nil) extends TreeInput {
  val sourcePaths: Seq[String] = Nil
}

object InputCollection {
  def apply (textInput: TextInput): InputCollection = InputCollection(Seq(textInput))
}
