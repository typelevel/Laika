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

import java.io.File

import laika.api.{Render, Transform}
import laika.ast.Path
import laika.ast.Path.Root

import scala.io.Codec

/** Base trait for various collections of output operations.
  *
  * @author Jens Halm
  */
trait OutputOps

/** A target for a render operation that renders to a single output.
  */
trait SingleOutputOps[Writer] extends OutputOps {
  
  type Result

  /** Renders the model to the file with the specified name.
    *
    *  @param name the name of the file to parse
    *  @param codec the character encoding of the file, if not specified the platform default will be used.
    */
  def toFile (name: String)(implicit codec: Codec): Result = toFile(new File(name))

  /** Renders the model to the specified file.
    *
    *  @param file the file to write to
    *  @param codec the character encoding of the file, if not specified the platform default will be used.
    */
  def toFile (file: File)(implicit codec: Codec): Result

}

trait BinaryOutputOps[Writer] extends SingleOutputOps[Writer] {

  def toFile (file: File)(implicit codec: Codec): Result = toBinaryOutput(BinaryFileOutput(file, Path(file.getName)))

  /** Renders the model to the specified output.
    *
    *  This is a generic method based on Laika's IO abstraction layer that concrete
    *  methods delegate to. Usually not used directly in application code, but
    *  might come in handy for very special requirements.
    */
  def toBinaryOutput (out: BinaryOutput): Result
  
}

trait BinaryRenderOutputOps[FMT] extends BinaryOutputOps[FMT] {

  type Result = Render.MergeOp2[FMT]
  
}

trait BinaryTransformOutputOps[FMT] extends BinaryOutputOps[FMT] {

  type Result = Transform.MergeOp2[FMT]

}

/**  Represents a single destination for a render operation.
  *  Various types of output can be
  *  specified to trigger the actual rendering.
  */
trait TextOutputOps[FMT] extends SingleOutputOps[FMT] {
  
  type Result

  def toFile (file: File)(implicit codec: Codec): Result = toTextOutput(TextFileOutput(file, Path(file.getName), codec))

  /** Renders the model to the specified output.
    *
    *  This is a generic method based on Laika's IO abstraction layer that concrete
    *  methods delegate to. Usually not used directly in application code, but
    *  might come in handy for very special requirements.
    */
  def toTextOutput (out: TextOutput): Result

}

trait TextRenderOutputOps[FMT] extends TextOutputOps[FMT] {

  type Result = Render.Op2[FMT]
  
  /** Renders the model to a String and returns it.
    */
  override def toString = { // TODO - 0.12 - toString needs new name (and has a different return type)
    val builder = new StringBuilder
    toTextOutput(StringOutput(builder, Root)).execute
    //toBuilder(builder).execute
    //builder.toString
  }

}

trait TextTransformOutputOps[Writer] extends TextOutputOps[Writer] {

  type Result = Transform.Op2[Writer]

  /** Renders the model to a String and returns it.
    */
  override def toString = { // TODO - 0.12 - toString needs new name (and has a different return type)
    val builder = new StringBuilder
    toTextOutput(StringOutput(builder, Root)).execute
//    toBuilder(builder).execute
//    builder.toString
  }

}

/** Represents a tree of output destinations for recursive render operations.
  *  Various types of output can be specified to trigger the actual rendering.
  */
trait OutputTreeOps[Writer] extends OutputOps {
  
  type Result

  /** Renders the document tree to the
    *  specified directory and its subdirectories.
    *  Required subdirectories which do not exist yet will be created.
    *
    *  @param name the name of the directory to write to
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def toDirectory (name: String)(implicit codec: Codec): Result = toDirectory(new File(name))

  /** Renders the document tree to the
    *  specified directory and its subdirectories.
    *  Required subdirectories which do not exist yet will be created.
    *
    *  @param dir the directory to write to
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def toDirectory (dir: File)(implicit codec: Codec): Result = toOutputTree(DirectoryOutput(dir, codec))

  /** Renders the document tree to the
    *  current working directory and its subdirectories.
    *  Required subdirectories which do not exist yet will be created.
    *
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def toDefaultDirectory (implicit codec: Codec): Result = toOutputTree(DirectoryOutput(new File(System.getProperty("user.dir")), codec))

  /** Renders the document tree to the specified output tree.
    */
  def toOutputTree (tree: TreeOutput): Result // TODO - rename

}

// TODO - 0.12 - unclutter trait hierarchies
trait RenderOutputTreeOps[Writer] extends OutputTreeOps[Writer] {
  
  type Result = Render.TreeOp2[Writer]
  
}

trait TransformOutputTreeOps[Writer] extends OutputTreeOps[Writer] {

  type Result = Transform.TreeOp2[Writer]

}
