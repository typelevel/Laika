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

import java.io.{BufferedReader, FileInputStream, InputStreamReader, Reader}

import laika.ast.Path
import laika.io.{TextInput, IO, StringInput, TextFileInput}
import laika.parse.ParserContext
import laika.parse.markup.DocumentParser.ParserInput

import scala.io.Codec

/** 
  * @author Jens Halm
  */
object InputExecutor {
  
  /** Builds a new instance for the specified input reader.
    */
  def readAll (reader: Reader): String = readAll(reader, 8 * 1024)

  /** Builds a new instance for the specified input reader, providing a hint
    * for the expected size of the input string.
    */
  def readAll (reader: Reader, sizeHint: Int): String = {

    val arr = new Array[Char](sizeHint)
    val buffer = new StringBuilder
    var numCharsRead: Int = 0

    while ({numCharsRead = reader.read(arr, 0, arr.length); numCharsRead != -1}) {
      buffer.appendAll(arr, 0, numCharsRead)
    }

    buffer.toString
  }
  
  def asParserInput (input: TextInput): ParserInput = input match {
    case StringInput(source, path) => ParserInput(path, ParserContext(source))
    case TextFileInput(file, path, codec) => 
      val source = IO(new BufferedReader(new InputStreamReader(new FileInputStream(file), codec.decoder)))(readAll)
      ParserInput(path, ParserContext(source))
  }
  
  // TODO - 0.12 - temporary solution
  def classPathParserInput (resourcePath: String, virtualPath: Path)(implicit codec: Codec): ParserInput = {
    val stream = getClass.getResourceAsStream(resourcePath)
    IO(stream){ in =>
      val reader = new BufferedReader(new InputStreamReader(in, codec.decoder))
      val content = InputExecutor.readAll(reader)
      ParserInput(virtualPath, ParserContext(content))
    }
  }
  
}
