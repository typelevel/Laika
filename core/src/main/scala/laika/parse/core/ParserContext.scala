/*
 * Copyright 2013-2017 the original author or authors.
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

package laika.parse.core

/**
  * @author Jens Halm
  */
class ParserContext {

}

object ParserContext {

  def apply (input: String): Reader = new CharSequenceReader(input)

  def apply (input: java.io.Reader): Reader = apply(input, 8 * 1024)

  def apply (input: java.io.Reader, sizeHint: Int): Reader = {

    val arr = new Array[Char](sizeHint)
    val buffer = new StringBuilder
    var numCharsRead: Int = 0

    while ({numCharsRead = input.read(arr, 0, arr.length); numCharsRead != -1}) {
      buffer.appendAll(arr, 0, numCharsRead)
    }

    new CharSequenceReader(buffer.toString)
  }

}
