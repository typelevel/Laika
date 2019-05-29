package laika.io

import java.io._

import laika.execute.OutputExecutor

/** Collection of I/O utilities.
 * 
 * @author Jens Halm
 */
object IOX {

  /** Common trait for all `Input` and `Output` instances
    * which map directly to a File.
    */
  trait FileBased {

    /** The file this instance is mapped to.
      */
    def file: File

  }

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
  def copy (input: BinaryInput, output: Output): Unit = {

    val sameFile = (input, output) match { // TODO - 0.12 - resurrect this check
      case (a: FileBased, b: FileBased) => a.file == b.file
      case _ => false
    }

    if (!sameFile) (input, output) match {
      case (in, out: BinaryOutput) =>
        val binaryIn = in match {
          case BinaryFileInput(file, _) => new BufferedInputStream(new FileInputStream(file)) // TODO - 0.12 - avoid duplication
          case ByteInput(bytes, _)      => new ByteArrayInputStream(bytes)
        }
        val binaryOut = OutputExecutor.asStream(out)
        apply(binaryIn) { in => apply(binaryOut) { out => copy(in, out) } }
      case (in, StringOutput(builder, _)) =>
        // TODO - 0.12 - temporary just to keep some more of the tests green during migration
        val binaryIn = in match {
          case BinaryFileInput(file, _) => new BufferedInputStream(new FileInputStream(file)) // TODO - 0.12 - avoid duplication
          case ByteInput(bytes, _)      => new ByteArrayInputStream(bytes)
        }
        val binaryOut = new ByteArrayOutputStream(8196)
        apply(binaryIn) { in => apply(binaryOut) { out => copy(in, out) } }
        builder.append(new String(binaryOut.toByteArray, "UTF-8"))
      case _ =>
        throw new RuntimeException("case not supported during 0.12 migration")
    }
  }

}
