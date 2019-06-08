package laika.runtime

import java.io._

import cats.effect.{Async, Resource}
import laika.io._

import scala.io.Codec

/**
  * @author Jens Halm
  */
object OutputRuntime {
  
  def write[F[_]: Async] (result: String, output: TextOutput): F[Unit] = {
    output match {
      case StringOutput(builder, path) => Async[F].unit
      case TextFileOutput(file, _, codec) => fileWriter(file, codec).use { writer =>
        Async[F].delay(writer.write(result))
      }  
    }
  }

  def fileWriter[F[_]: Async] (file: File, codec: Codec): Resource[F, Writer] = Resource.fromAutoCloseable(Async[F].delay {
    new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), codec.charSet))
  })

  def asStream (output: BinaryOutput): OutputStream = output match {
    case BinaryFileOutput(file, _) => new BufferedOutputStream(new FileOutputStream(file))
    case ByteOutput(out, _) => out
  }

}
