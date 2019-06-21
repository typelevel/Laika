package laika.io.ops

import java.io.{File, OutputStream}

import cats.effect.Async
import laika.ast.Path
import laika.ast.Path.Root
import laika.io.model.{BinaryFileOutput, BinaryOutput, BinaryStreamOutput}

/** API for specifying the output for a binary format like EPUB or PDF.
  *
  * It allows any class merging in this trait to define all output related operations
  * in terms of the only abstract method `toOutput`.
  *
  * @author Jens Halm
  */
trait BinaryOutputOps[F[_]] {

  type Result

  def F: Async[F]

  /** Builder step that instructs the runtime to render
    * to the file with the specified name.
    *
    *  @param name the name of the file to write to
    */
  def toFile (name: String): Result = toFile(new File(name))

  /** Builder step that instructs the runtime to render
    * to the specified file.
    *
    *  @param file the file to write to
    */
  def toFile (file: File): Result =
    toOutput(F.pure(BinaryFileOutput(file, Path(file.getName))))

  /** Builder step that instructs the runtime to render
    * to the specified output stream.
    * 
    * @param stream the binary stream to render to
    * @param autoClose indicates whether the stream should be closed after all output had been written                 
    */
  def toStream (stream: F[OutputStream], autoClose: Boolean = true): Result =
    toOutput(F.map(stream)(BinaryStreamOutput(_, Root, autoClose)))

  /** Builder step that instructs the runtime to render
    * to the specified output.
    *
    * This is a generic method based on Laika's IO model that concrete
    * methods delegate to.
    */
  def toOutput (output: F[BinaryOutput]): Result

}
