/*
 * Copyright 2013-2018 the original author or authors.
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

import java.io.{File, OutputStream}

import laika.io.Output.Binary

import scala.io.Codec

/** Base trait for various collections of output operations.
  *
  * @author Jens Halm
  */
trait OutputOps

/** A target for a render operation that accepts only binary output.
  */
trait BinaryOutputOps extends OutputOps {

  /** Renders the model to the file with the specified name.
    *
    *  @param name the name of the file to parse
    *  @param codec the character encoding of the file, if not specified the platform default will be used.
    */
  def toFile (name: String)(implicit codec: Codec): Unit = toBinaryOutput(Output.toFile(name)(codec))

  /** Renders the model to the specified file.
    *
    *  @param file the file to write to
    *  @param codec the character encoding of the file, if not specified the platform default will be used.
    */
  def toFile (file: File)(implicit codec: Codec): Unit = toBinaryOutput(Output.toFile(file)(codec))

  /** Renders the model to the specified output stream.
    *
    *  @param stream the stream to render to
    *  @param codec the character encoding of the stream, if not specified the platform default will be used.
    */
  def toStream (stream: OutputStream)(implicit codec: Codec): Unit = toBinaryOutput(Output.toStream(stream)(codec))

  /** Renders the model to the specified output.
    *
    *  This is a generic method based on Laika's IO abstraction layer that concrete
    *  methods delegate to. Usually not used directly in application code, but
    *  might come in handy for very special requirements.
    */
  def toBinaryOutput (out: Output with Binary): Unit

}

/** Represents a single destination for a render operation.
  *  Various types of output can be
  *  specified to trigger the actual rendering.
  */
trait TextOuputOps extends BinaryOutputOps {

  /** Renders the model to the console.
    */
  def toConsole: Unit = toStream(System.out)

  /** Renders the model to the specified writer.
    */
  def toWriter (writer: java.io.Writer): Unit = toOutput(Output.toWriter(writer))

  /** Renders the model to the specified `StringBuilder`.
    */
  def toBuilder (builder: StringBuilder): Unit = toOutput(Output.toBuilder(builder))

  /** Renders the model to a String and returns it.
    */
  override def toString = {
    val builder = new StringBuilder
    toBuilder(builder)
    builder.toString
  }

  /** Renders the model to the specified output.
    *
    *  This is a generic method based on Laika's IO abstraction layer that concrete
    *  methods delegate to. Usually not used directly in application code, but
    *  might come in handy for very special requirements.
    */
  def toOutput (out: Output): Unit

  /** Renders the model to the specified binary output.
    */
  def toBinaryOutput (out: Output with Binary): Unit = toOutput(out)

}

/** Represents a tree of output destinations for recursive render operations.
  *  Various types of output can be specified to trigger the actual rendering.
  */
trait OutputTreeOps extends OutputOps {

  /** Renders the document tree to the
    *  specified directory and its subdirectories.
    *  Required subdirectories which do not exist yet will be created.
    *
    *  @param name the name of the directory to write to
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def toDirectory (name: String)(implicit codec: Codec): Unit = toDirectory(new File(name))

  /** Renders the document tree to the
    *  specified directory and its subdirectories.
    *  Required subdirectories which do not exist yet will be created.
    *
    *  @param dir the directory to write to
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def toDirectory (dir: File)(implicit codec: Codec): Unit = toOutputTree(OutputTree.forRootDirectory(dir))

  /** Renders the document tree to the
    *  current working directory and its subdirectories.
    *  Required subdirectories which do not exist yet will be created.
    *
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def toDefaultDirectory (implicit codec: Codec): Unit = toOutputTree(OutputTree.forWorkingDirectory)

  /** Renders the document tree to the specified output tree.
    */
  def toOutputTree (tree: OutputTree): Unit

}
