package laika.runtime

import java.io.{BufferedOutputStream, BufferedWriter, FileOutputStream, OutputStream, OutputStreamWriter}

import laika.io._

/**
  * @author Jens Halm
  */
object OutputRuntime {
  
  case class RenderFunction (render: String => Unit, close: () => Unit = () => ()) 
  
  def asRenderFunction (output: TextOutput): RenderFunction = output match {
    case StringOutput(builder, _) => RenderFunction(builder.append(_:String))
    case TextFileOutput(file, _, codec) => 
      val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), codec.encoder))
      RenderFunction(writer.write(_:String), () => writer.close())
  }
  
  def asStream (output: BinaryOutput): OutputStream = output match {
    case BinaryFileOutput(file, _) => new BufferedOutputStream(new FileOutputStream(file))
    case ByteOutput(out, _) => out
  }

}
