/*
 * Copyright 2014 the original author or authors.
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

package laika.parse.css

import java.io.File
import java.io.InputStream
import java.io.Reader
import scala.io.Codec
import laika.io.IO
import laika.io.Input
import laika.parse.css.Styles.StyleDeclarationSet

/** API for performing a parse operation for a style sheet. Usually not called
 *  directly by application code unless a custom CSS parser needs
 *  to be installed. 
 *  
 *  @author Jens Halm
 */
class ParseStyleSheet private (parse: Input => StyleDeclarationSet) {

  /** Returns a style set obtained from parsing the specified string.
   *  Any kind of input is valid, including an empty string. 
   */
  def fromString (str: String) = fromInput(Input.fromString(str))
  
  /** Returns a style set obtained from parsing the input from the specified reader.
   */
  def fromReader (reader: Reader) = fromInput(Input.fromReader(reader))

  /** Returns a style set obtained from parsing the file with the specified name.
   *  Any kind of character input is valid, including empty files.
   * 
   *  @param name the name of the file to parse
   *  @param codec the character encoding of the file, if not specified the platform default will be used.
   */
  def fromFile (name: String)(implicit codec: Codec) = fromInput(Input.fromFile(name)(codec))
  
  /** Returns a style set obtained from parsing the specified file.
   *  Any kind of character input is valid, including empty files.
   * 
   *  @param file the file to use as input
   *  @param codec the character encoding of the file, if not specified the platform default will be used.
   */
  def fromFile (file: File)(implicit codec: Codec) = fromInput(Input.fromFile(file)(codec))
  
  /** Returns a style set obtained from parsing the input from the specified stream.
   * 
   *  @param stream the stream to use as input for the parser
   *  @param codec the character encoding of the stream, if not specified the platform default will be used.
   */
  def fromStream (stream: InputStream)(implicit codec: Codec) = fromInput(Input.fromStream(stream)(codec))
  
  def fromInput (input: Input) = IO(input)(parse)
  
}

/** Serves as an entry point to the ParseTemplate API.
 * 
 *  @author Jens Halm
 */
object ParseStyleSheet extends ParseStyleSheet(CSSParsers.default) {
  
  /** Returns a new ParseStyleSheet instance for the specified parser function.
   * 
   *  @param parser the parser function to use for all subsequent operations
   */
  def as (parser: Input => StyleDeclarationSet) = new ParseStyleSheet(parser) 
  
}
