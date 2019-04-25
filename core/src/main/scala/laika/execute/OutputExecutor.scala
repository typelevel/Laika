/*
 * Copyright 2012-2019 the original author or authors.
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

package laika.execute

import java.io.{BufferedOutputStream, BufferedWriter, FileOutputStream, OutputStream, OutputStreamWriter}

import laika.io._

/**
  * @author Jens Halm
  */
object OutputExecutor {
  
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
