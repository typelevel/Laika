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

import laika.ast.Path
import laika.io.IO.FileBased

import scala.collection.mutable.StringBuilder
import scala.io.Codec

/** Represents the output of a renderer, abstracting over various types of IO resources. 
 *  
 *  For renderers that only need a simple way to pass Strings to the output, a simple
 *  funtion (`String => Unit`) is provided. Alternatively the full `java.io.Writer` API
 *  may be used.
 *  
 *  The API provided by this trait is only meant to be used internally by a renderer
 *  implementation. For providing hooks for user customization, the renderer should
 *  wrap it in a convenient API appropriate for the corresponding output format, like
 *  the `HTMLWriter` API for HTML renderers for example. 
 * 
 *  @author Jens Halm
 */
trait Output { 

  /** The output as a `java.io.Writer`.
   */
  def asWriter: Writer
  
  /** The output as a simple function taking Strings
   *  to append to the output.
   */
  def asFunction: String => Unit
  
  /** Flushes this output, forcing all buffered output
   *  to be written, without closing the underlying writer or stream.
   */
  def flush (): Unit = ()
  
  /** The full path of this output.
   *  This path is always an absolute path
   *  from the root of the (virtual) output tree,
   *  therefore does not represent the filesystem
   *  path in case of file I/O.
   */
  def path: Path
  
  /** The local name of this output.
   */
  lazy val name: String = path.name
  
}


/** Factory methods for creating Output instances from different types of IO resources.
 * 
 *  @author Jens Halm
 */
object Output {
  
  /** Represents any output that can be used as binary output.
   */
  trait Binary {
    def asBinaryOutput: BinaryOutput
  }
  
  /** A sub-trait for binary output which can only be created
   *  from a stream or a file, not from a `java.io.Writer` instance
   *  or from a `StringBuilder`.
   * 
   *  Most renderers write character data, but formats like PDF
   *  would require a binary stream to write to.
   */
  trait BinaryOutput {
  
    /** The output as a `java.io.OutputStream`. Should only be used
     *  by renderers that do not produce character data.
     */
    def asStream: OutputStream
    
    /** Flushes this output, forcing all buffered output
     *  to be written, without closing the underlying writer or stream.
     */
    def flush (): Unit = ()
    
  }
  
  case class StringBuilderOutput (builder: StringBuilder, path: Path) extends Output {
    
    def asWriter: Writer = new StringBuilderWriter(builder)
  
    def asFunction: String => Unit = builder.append(_:String)
    
  }
  
  private class StreamOutput (stream: OutputStream, val path: Path, codec: Codec) extends Output with Binary {
   
    def asBinaryOutput: BinaryOutput = new BinaryOutput {
      val asStream = stream
      override def flush = stream flush
    }
    
    lazy val asWriter: Writer = new BufferedWriter(new OutputStreamWriter(stream, codec.encoder))
    
    val asFunction: String => Unit = asWriter.write(_:String)
    
    override def flush (): Unit = asWriter flush
    
  }
  
  private class AutocloseStreamOutput (stream: OutputStream, p: Path, codec: Codec) extends StreamOutput(stream,p,codec) with Closeable {

    override def asBinaryOutput: BinaryOutput = new BinaryOutput with Closeable {
      val asStream = stream
      override def flush = stream flush
      def close = stream close
    }
    
    def close (): Unit = asWriter close
    
  }
  
  private class LazyFileOutput (val file: File, val path: Path, codec: Codec) extends Output with Binary with FileBased with Closeable {
    
    lazy val delegate = new AutocloseStreamOutput(new BufferedOutputStream(new FileOutputStream(file)), path, codec)
    
    def asWriter: Writer = delegate.asWriter
    def asFunction: String => Unit = delegate.asFunction
    override def flush (): Unit = delegate.flush()
    def close (): Unit = delegate.close()
    def asBinaryOutput: BinaryOutput = delegate.asBinaryOutput
  }
  
  /** Creates a new Output instance for the file with the specified name.
   *  
   *  @param name the name of the file
   *  @param codec the character encoding of the file, if not specified the platform default will be used.
   */
  def toFile (name: String)(implicit codec: Codec): Output with Binary with Closeable = new LazyFileOutput(new File(name), Path(name), codec)
  
  /** Creates a new Output instance for the specified file.
   *  
   *  @param file the file to use as output
   *  @param codec the character encoding of the file, if not specified the platform default will be used.
   */
  def toFile (file: File)(implicit codec: Codec): Output with Binary with Closeable = new LazyFileOutput(file, Path(file.getName), codec)
  
  def toFile (file: File, virtualPath: Path)(implicit codec: Codec): Output with Binary with Closeable = 
    new LazyFileOutput(file, virtualPath / file.getName, codec)

  /** Creates a new Output instance for the specified StringBuilder.
   */
  def toBuilder (builder: StringBuilder, path: Path = Path.Root): Output = new StringBuilderOutput(builder, path)
  
  
  
  private class StringBuilderWriter (builder: StringBuilder) extends Writer {

    def write(c: Char): StringBuilder = builder += c

    def write (buf: Array[Char], offset: Int, len: Int): Unit = {
      if ((offset < 0) || (offset > buf.length) || (len < 0) ||
          ((offset + len) > buf.length) || ((offset + len) < 0)) throw new IndexOutOfBoundsException()
      if (len > 0) builder.appendAll(buf, offset, len);
    }

    override def write(str: String): Unit = builder ++= str

    override def write(str: String, offset: Int, len: Int): Unit = {
      builder ++= str.substring(offset, offset + len)
    }
    
    override def append(seq: CharSequence): StringBuilderWriter = {
      write(if (seq == null) "null" else seq.toString)
      this
    }

    override def append(seq: CharSequence, start: Int, end: Int): StringBuilderWriter = {
      val res = if (seq == null) "null" else seq
      write(res.subSequence(start, end).toString)
      this
    }

    override def append(c: Char): StringBuilderWriter = { write(c); this }

    override def toString = builder.toString

    def flush (): Unit = ()
     
    def close (): Unit = ()
    
  }
  
}

