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
  
import java.io._

/** Collection of I/O utilities.
 * 
 * @author Jens Halm
 */
object IO {

  /** Calls the specified function, closes the IO resource if the resource
   *  mixes in `java.io.Closeable` and returns the result of the function call.
   *  
   *  @param resource the IO resource to manage
   *  @param f the function to invoke, with the managed resource getting passed into it
   *  @return the result of the function call
   */
  def apply [R, T] (resource: R)(f: R => T) = resource match {
    case c: Closeable => try f(resource) finally c.close
    case _ => f(resource)
  }
  
  
  /** Copies all bytes from the specified InputStream to the
   *  OutputStream. Rethrows all Exceptions and does not
   *  close the streams afterwards.
   */ 
  def copy (input: InputStream, output: OutputStream): Unit = (input, output) match {
    case (in: FileInputStream, out: FileOutputStream) =>
      in.getChannel().transferTo(0, Integer.MAX_VALUE, out.getChannel());
    case _ => {
      val buffer = new Array[Byte](8192)
      Iterator.continually(input.read(buffer))
        .takeWhile(_ != -1)
        .foreach { output.write(buffer, 0 , _) }
    }
  }

  /** Copies all characters from the specified Reader to the
   *  Writer. Rethrows all Exceptions and does not
   *  close the Reader or Writer afterwards.
   */ 
  def copy (input: Reader, output: Writer): Unit = {
    val buffer = new Array[Char](8192)
    Iterator.continually(input.read(buffer))
      .takeWhile(_ != -1)
      .foreach { output.write(buffer, 0 , _) }
  }
  
  /** Copies all bytes or characters (depending on Input type) 
   *  from the specified Input to the
   *  Output. Rethrows all Exceptions and does not
   *  close the Input or Output afterwards.
   */ 
  def copy (input: Input, output: Output): Unit = {
    (input, output) match {
      case (in: Input.Binary, out: Output.Binary) => {
        val binaryIn = in.asBinaryInput
        val binaryOut = out.asBinaryOutput
        apply(binaryIn) { in => apply(binaryOut) { out => copy(in.asStream, out.asStream) } }
      }
      case _ => apply(input) { in => apply(output) { out => copy(in.asReader, out.asWriter) } }
    }
  }
  
  
}