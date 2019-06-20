package laika.io.ops

import java.io.{File, OutputStream}

import cats.effect.Async
import laika.ast.Path
import laika.ast.Path.Root
import laika.io.model.{BinaryFileOutput, BinaryOutput, BinaryStreamOutput}

/**
  * @author Jens Halm
  */
trait BinaryOutputOps[F[_]] {

  type Result

  def F: Async[F]

  /** Renders to the file with the specified name.
    *
    *  @param name the name of the file to parse
    */
  def toFile (name: String): Result = toFile(new File(name))

  /** Renders to the specified file.
    *
    *  @param file the file to write to
    */
  def toFile (file: File): Result =
    toOutput(F.pure(BinaryFileOutput(file, Path(file.getName))))

  /** Renders to the specified output stream.
    * 
    * @param stream the binary stream to render to
    * @param autoClose whether the stream should be closed after all output had been written                 
    */
  def toStream (stream: F[OutputStream], autoClose: Boolean = true): Result =
    toOutput(F.map(stream)(BinaryStreamOutput(_, Root, autoClose)))

  /** Renders the model to the specified output.
    *
    *  This is a generic method based on Laika's IO abstraction layer that concrete
    *  methods delegate to. Usually not used directly in application code, but
    *  might come in handy for very special requirements.
    */
  def toOutput (output: F[BinaryOutput]): Result

}
