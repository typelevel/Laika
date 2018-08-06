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
import laika.parse.ParserContext

import scala.io.Codec

/** Represents the input for a parser, abstracting over various types of IO resources. 
 *  
 *  For parsers that use the parser combinators of this library,
 *  this trait offers a convenient `asParserInput` method. Other types of parsers
 *  may use the `java.io.Reader` provided by the `asReader` method. 
 * 
 *  @author Jens Halm
 */
trait Input {

  /** The input as a `java.io.Reader`.
   */
  def asReader: java.io.Reader

  /** The input as a `ParserContext` instance for Laika's parser combinators.
   */
  def asParserInput: ParserContext
  
  /** The full path of this input.
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

/** Factory methods for creating Input instances from different types of IO resources.
 * 
 *  @author Jens Halm
 */
object Input {
  
  /** Represents any input that can be used as binary input.
   */
  trait Binary {
    def asBinaryInput: BinaryInput
  }
  
  /** A sub-trait for binary input which can only be created
   *  from a stream or a file, not from a `java.io.Reader` instance
   *  or from a `String`.
   */
  trait BinaryInput {
  
    def asStream: InputStream
    
  }
  
  private class StringInput (source: String, val path: Path) extends Input {
    
    def asReader: java.io.Reader = new StringReader(source)
  
    def asParserInput: ParserContext = ParserContext(source)
    
  }
  
  private class ReaderInput (val asReader: java.io.Reader, val path: Path) extends Input {
   
    def asParserInput: ParserContext = ParserContext(asReader)

  }
  
  private class StreamInput (stream: InputStream, val path: Path, codec: Codec) extends Input with Binary {
   
    def asBinaryInput: BinaryInput = new BinaryInput {
      val asStream = stream
    }
    
    def asParserInput: ParserContext = ParserContext(asReader)
    
    lazy val asReader: java.io.Reader = new BufferedReader(new InputStreamReader(stream, codec.decoder))
    
  }
  
  private class AutocloseStreamInput (stream: InputStream, p: Path, codec: Codec) extends StreamInput(stream,p,codec) with Closeable {
    
    override def asBinaryInput: BinaryInput = new BinaryInput with Closeable {
      val asStream = stream
      def close = stream close
    }
    
    def close: Unit = asReader.close
    
  }
  
  class LazyFileInput (val file: File, val path: Path, codec: Codec) extends Input with Binary with FileBased with Closeable {
    
    private lazy val delegate = new AutocloseStreamInput(new FileInputStream(file), path, codec)
    
    def asReader: java.io.Reader = delegate.asReader
    def asParserInput: ParserContext = ParserContext(delegate.asReader, file.length.toInt)
    def close: Unit = delegate.close
    def asBinaryInput: BinaryInput = delegate.asBinaryInput
  }
  
  class LazyClasspathInput (val resource: String, val path: Path, codec: Codec) extends Input with Binary with Closeable {
    
    private lazy val delegate = new AutocloseStreamInput(getClass.getResourceAsStream(resource), path, codec)
    
    def asReader: java.io.Reader = delegate.asReader
    def asParserInput: ParserContext = delegate.asParserInput
    def close: Unit = delegate.close
    def asBinaryInput: BinaryInput = delegate.asBinaryInput
  }
  
  /** Creates a new Input instance from the specified source string.
   *  
   *  @param source the string to parse
   *  @param path the path of the document in the virtual tree 
   */
  def fromString (source: String, path: Path = Path.Root): Input = new StringInput(source, path)

  /** Creates a new Input instance for the file with the specified name.
   *  
   *  @param name the name of the file
   *  @param codec the character encoding of the file, if not specified the platform default will be used.
   */
  def fromFile (name: String)(implicit codec: Codec): Input with Binary with Closeable 
    = new LazyFileInput(new File(name), Path(name), codec)
  
  /** Creates a new Input instance for the specified file.
   *  
   *  @param file the file to use as input
   *  @param codec the character encoding of the file, if not specified the platform default will be used.
   */
  def fromFile (file: File)(implicit codec: Codec): Input with Binary with Closeable 
    = new LazyFileInput(file, Path(file.getName), codec)
  
  /** Creates a new Input instance for the specified file.
   *  
   *  @param file the file to use as input
   *  @param virtualPath the path of the document in the virtual tree
   *  @param codec the character encoding of the file, if not specified the platform default will be used.
   */
  def fromFile (file: File, virtualPath: Path)(implicit codec: Codec): Input with Binary with Closeable 
    = new LazyFileInput(file, virtualPath / file.getName, codec)
  
  /** Creates a new Input instance for the specified classpath resource.
   *  
   *  @param resource the name of the resource on the classpath
   *  @param virtualPath the path of the document in the virtual tree 
   *  @param codec the character encoding of the input, if not specified the platform default will be used.
   */
  def fromClasspath (resource: String, virtualPath: Path)(implicit codec: Codec): Input with Binary with Closeable
    = new LazyClasspathInput(resource, virtualPath, codec)
  
  /** Creates a new Input instance for the specified InputStream.
   *  
   *  @param stream the stream to read character data from
   *  @param path the (potentially virtual) path of the input source
   *  @param codec the character encoding used by the text input, if not specified the platform default will be used.
   */
  def fromStream (stream: InputStream, path: Path = Path.Root)(implicit codec: Codec): Input with Binary = new StreamInput(stream, path, codec)
  
  /** Creates a new Input instance for the specified Reader.
   *  
   *  @param reader the reader to read character data from
   *  @param path the (potentially virtual) path of the input source
   */
  def fromReader (reader: java.io.Reader, path: Path = Path.Root): Input = new ReaderInput(reader, path)
  
  
}