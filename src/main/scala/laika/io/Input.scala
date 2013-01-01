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
 
import java.io.BufferedReader
import java.io.Closeable
import java.io.FileInputStream
import java.io.InputStream
import java.io.InputStreamReader
import java.io.StringReader
import scala.collection.immutable.PagedSeq
import scala.io.Codec
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.input.PagedSeqReader
import scala.util.parsing.input.Reader
import java.io.File

/** Represents the input for a parser, abstracting over various types of IO resources. 
 *  
 *  For parsers that use the parser combinators from the Scala SDK, 
 *  this trait offers a convenient `asParserInput` method. Other types of parsers
 *  may use the `java.io.Reader` provided by the `asReader` method. 
 * 
 *  @author Jens Halm
 */
trait Input {

  /** The input as a `java.io.Reader`.
   */
  def asReader: java.io.Reader

  /** The input as a Reader instance for the parser combinators of the Scala SDK.
   */
  def asParserInput: Reader[Char]
  
}

/** Factory methods for creating Input instances from different types of IO resources.
 * 
 *  @author Jens Halm
 */
object Input {
  
  private class StringInput (source: String) extends Input with Closeable {
    
    def asReader = new StringReader(source)
  
    def asParserInput = new CharSequenceReader(source)
    
    def close = ()
  }
  
  private class ReaderInput (val asReader: java.io.Reader) extends Input with Closeable {
   
    def asParserInput = new PagedSeqReader(PagedSeq.fromReader(asReader))

    def close = asReader.close
  }
  
  /** Creates a new Input instance from the specified string.
   */
  def fromString (source: String): Input with Closeable = new StringInput(source)

  /** Creates a new Input instance for the file with the specified name.
   *  
   *  @param name the name of the file
   *  @param codec the character encoding of the file, if not specified the platform default will be used.
   */
  def fromFile (name: String)(implicit codec: Codec): Input with Closeable = fromStream(new FileInputStream(name))(codec)
  
  /** Creates a new Input instance for the specified file.
   *  
   *  @param file the file to use as input
   *  @param codec the character encoding of the file, if not specified the platform default will be used.
   */
  def fromFile (file: File)(implicit codec: Codec): Input with Closeable = fromStream(new FileInputStream(file))(codec)
  
  /** Creates a new Input instance for the specified InputStream.
   *  
   *  @param stream the stream to read character data from
   *  @param codec the character encoding used by the text input, if not specified the platform default will be used.
   */
  def fromStream (stream: InputStream)(implicit codec: Codec): Input with Closeable = 
    fromReader(new BufferedReader(new InputStreamReader(stream, codec.decoder)))
  
  /** Creates a new Input instance for the specified Reader.
   */
  def fromReader (reader: java.io.Reader): Input with Closeable = new ReaderInput(reader)
  
}