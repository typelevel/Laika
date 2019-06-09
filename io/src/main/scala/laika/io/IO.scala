package laika.io

import java.io._

import laika.runtime.OutputRuntime

/** Collection of I/O utilities.
  * 
  * // TODO - 0.12 - remove/replace this class
 * 
 * @author Jens Halm
 */
object IOX {

  /** Calls the specified function, closes the IO resource if the resource
   *  mixes in `java.io.Closeable` and returns the result of the function call.
   *  
   *  @param resource the IO resource to manage
   *  @param f the function to invoke, with the managed resource getting passed into it
   *  @return the result of the function call
   */
  def apply [R, T] (resource: R)(f: R => T): T = resource match {
    case c: Closeable => try f(resource) finally c.close
    case _ => f(resource)
  }
  
  
  /** Copies all bytes from the specified InputStream to the
   *  OutputStream. Rethrows all Exceptions and does not
   *  close the streams afterwards.
   */ 
  def copy (input: InputStream, output: OutputStream): Unit = (input, output) match {
    case (in: FileInputStream, out: FileOutputStream) =>
      in.getChannel.transferTo(0, Integer.MAX_VALUE, out.getChannel);
    case _ =>
      val buffer = new Array[Byte](8192)
      Iterator.continually(input.read(buffer))
        .takeWhile(_ != -1)
        .foreach { output.write(buffer, 0 , _) }
  }

  /** Copies all bytes or characters (depending on Input type) 
   *  from the specified Input to the
   *  Output. Rethrows all Exceptions and does not
   *  close the Input or Output afterwards.
   */ 
  def copy (input: BinaryInput, output: BinaryOutput): Unit = {

    val sameFile = (input, output) match {
      case (a: BinaryFileInput, b: BinaryFileOutput) => a.file == b.file
      case _ => false
    }

    if (!sameFile) {
      val inputStream = input match {
        case BinaryFileInput(file, _) => new BufferedInputStream(new FileInputStream(file)) // TODO - 0.12 - avoid duplication
        case ByteInput(bytes, _)      => new ByteArrayInputStream(bytes)
      }
      val outputStream = OutputRuntime.asStream(output)
      apply(inputStream) { in => apply(outputStream) { out => copy(in, out) } }
    }
  }

}
