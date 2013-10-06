/*
 * Copyright 2013 the original author or authors.
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

import java.io.BufferedWriter
import java.io.Closeable
import java.io.File
import java.io.FileOutputStream
import java.io.OutputStream
import java.io.OutputStreamWriter
import java.io.Writer

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
  
}


/** Factory methods for creating Output instances from different types of IO resources.
 * 
 *  @author Jens Halm
 */
object Output {
  
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
  
  private class StringBuilderOutput (builder: StringBuilder) extends Output {
    
    def asWriter: Writer = new StringBuilderWriter(builder)
  
    def asFunction = builder.append(_:String)
    
  }
  
  private class WriterOutput (val asWriter: Writer) extends Output {
   
    def asFunction = asWriter.write(_:String)
    
    override def flush = asWriter flush
  }
  
  private class StreamOutput (stream: OutputStream, codec: Codec) extends Output with Binary {
   
    def asBinaryOutput: BinaryOutput = new BinaryOutput {
      val asStream = stream
      override def flush = stream flush
    }
    
    lazy val asWriter = new BufferedWriter(new OutputStreamWriter(stream, codec.encoder))
    
    val asFunction = asWriter.write(_:String)
    
    override def flush = asWriter flush
    
  }
  
  private class AutocloseStreamOutput (stream: OutputStream, codec: Codec) extends StreamOutput(stream,codec) with Closeable {

    override def asBinaryOutput: BinaryOutput = new BinaryOutput with Closeable {
      val asStream = stream
      override def flush = stream flush
      def close = stream close
    }
    
    def close = asWriter close
    
  }
  
  /** Creates a new Output instance for the file with the specified name.
   *  
   *  @param name the name of the file
   *  @param codec the character encoding of the file, if not specified the platform default will be used.
   */
  def toFile (name: String)(implicit codec: Codec): Output with Binary with Closeable = new AutocloseStreamOutput(new FileOutputStream(name), codec)
  
  /** Creates a new Output instance for the specified file.
   *  
   *  @param file the file to use as output
   *  @param codec the character encoding of the file, if not specified the platform default will be used.
   */
  def toFile (file: File)(implicit codec: Codec): Output with Binary with Closeable = new AutocloseStreamOutput(new FileOutputStream(file), codec)

  /** Creates a new Output instance for the specified OutputStream.
   *  
   *  @param stream the stream to write to
   *  @param codec the character encoding to use for producing the bytes, if not specified the platform default will be used.
   */
  def toStream (stream: OutputStream)(implicit codec: Codec): Output with Binary = new StreamOutput(stream, codec)

  /** Creates a new Output instance for the specified Writer.
   */
  def toWriter (writer: Writer): Output = new WriterOutput(writer)

  /** Creates a new Output instance for the specified StringBuilder.
   */
  def toBuilder (builder: StringBuilder): Output = new StringBuilderOutput(builder)
  
  
  
  private class StringBuilderWriter (builder: StringBuilder) extends Writer {

    def write(c: Char) = builder += c

    def write (buf: Array[Char], offset: Int, len: Int) {
        if ((offset < 0) || (offset > buf.length) || (len < 0) ||
            ((offset + len) > buf.length) || ((offset + len) < 0)) {
            throw new IndexOutOfBoundsException();
        } 
        if (len > 0) builder.appendAll(buf, offset, len);
    }

    override def write(str: String) = builder ++= str

    override def write(str: String, offset: Int, len: Int) = {
      builder ++= str.substring(offset, offset + len)
    }
    
    override def append(seq: CharSequence) = {
      write(if (seq == null) "null" else seq.toString)
      this
    }

    override def append(seq: CharSequence, start: Int, end: Int) = {
      val res = if (seq == null) "null" else seq
      write(res.subSequence(start, end).toString)
      this
    }

    override def append(c: Char) = { write(c); this }

    override def toString = builder.toString

    def flush = ()
     
    def close = ()
    
  }
  
}

