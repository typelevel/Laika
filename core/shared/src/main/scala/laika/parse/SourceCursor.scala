/*
 * Copyright 2012-2020 the original author or authors.
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

package laika.parse

import scala.collection.mutable.ArrayBuffer

/** Represents the state and context of a parsing operation,
  * containing the input string as well as positional information.
  *
  * @author Jens Halm
  */
trait SourceCursor {

  /** The full input string, containing the string portions before and after the current offset.
    */
  def input: String

  /** The offset of this cursor from the start of the source.
    */
  def offset: Int
  
  /** Indicates whether this contexts offset is behind the last character of the input string
    */
  def atEnd: Boolean

  /** Indicates the number of characters remaining in the input string after the current offset.
    */
  def remaining: Int

  /** The character at the current offset.
    */
  def char: Char = charAt(0)

  /** The character at the specified offset, relative from the current offset.
    */
  def charAt (relativeOffset: Int): Char

  /** Captures a string containing the specified number of characters from the current offset.
    * If the number of remaining characters is less than the specified number, all remaining
    * characters will be returned.
    */
  def capture (numChars: Int): String

  /** Consumes the specified number of characters, returning a new `ParserContext` with the new offset.
    */
  def consume (numChars: Int): SourceCursor

  /** The current position in the input string.
    */
  def position: Position

  /** The nest level of this cursor in case of recursive parsing. */
  def nestLevel: Int

  /** Returns a new `SourceCursor` with the input string being reversed,
    * but pointing to the same character as this context.
    *
    * This is a low-level optimization for parsers that look for strings like email addresses where the first character 
    * is not significant, so that parsing backwards from any `@` encountered in the input provided better performance.
    */
  def reverse: SourceCursor
  
}

/** A root source represents the full input string of a parsing operation.
  * 
  * In a single-pass parser like those for HOCON or CSS, only `RootCursor` instances will be used for the entire
  * parsing operation.
  * 
  * In a multi-pass parser like those for text markup, a `RootCursor` is only used for the first pass, 
  * whereas the subsequent passes on parts of the input are performed with the other `SourceCursor` implementations.
  */
class RootSource (inputRef: InputString, val offset: Int, val nestLevel: Int) extends SourceCursor {

  val input: String = inputRef.value

  def atEnd: Boolean = offset >= input.length

  def remaining: Int = input.length - offset

  def charAt (relativeOffset: Int): Char = input.charAt(offset + relativeOffset)

  def capture (numChars: Int): String = {
    require(numChars >= 0, "numChars cannot be negative")
    
    val endIndex = Math.min(input.length, offset + numChars)
    input.substring(offset, endIndex)
  }

  def consume (numChars: Int): SourceCursor =
    if (numChars != 0) new RootSource(inputRef, offset + numChars, nestLevel)
    else this

  def position: Position = Position(inputRef, offset)

  def reverse: SourceCursor = new RootSource(inputRef.reverse, remaining, nestLevel)

}

/** Companion for creating new `SourceCursor` instances.
  */
object SourceCursor {

  /** Builds a new instance for the specified input string.
    */
  def apply (input: String): SourceCursor = new RootSource(InputString(input), 0, 0)

  /** Builds a new instance for the specified input string and nesting level.
    *
    * Keeping track of the nesting level allows to protect against malicious
    * input that would otherwise cause endless recursion triggering stack
    * overflows or ultra-slow performance.
    */
  def apply (input: String, nestLevel: Int): SourceCursor = new RootSource(InputString(input), 0, nestLevel)

}

/** Represents the input string for a parsing operation.
  */
case class InputString (value: String) {

  /** An index that contains all line starts, including first line, and eof.
    */
  lazy val lineStarts: Array[Int] = {
    val lineStarts = new ArrayBuffer[Int]
    lineStarts += 0
    var pos = 0
    val len = value.length
    while (pos < len) {
      if (value(pos) == '\n') lineStarts += pos + 1
      pos += 1
    }
    lineStarts += len
    lineStarts.toArray
  }

  /** Builds a new `Source` instance with the input string reversed.
    */
  lazy val reverse = InputString(value.reverse)

}

/**  Represents an offset into a source string. Its main purpose
  *  is error reporting, e.g. printing a visual representation of the line
  *  containing the error.
  *
  *  @param s the source for this position
  *  @param offset the offset into the source string
  *
  *  @author Jens Halm
  */
case class Position(s: InputString, offset: Int) {

  val source: String = s.value

  /** The line number referred to by this position, starting at 1.
    */
  lazy val line: Int = {
    val result = java.util.Arrays.binarySearch(s.lineStarts, offset)
    if (result == s.lineStarts.length - 1) result // EOF position is not on a new line
    else if (result < 0) Math.abs(result) - 1 // see javadoc for binarySearch
    else result + 1 // line is 1-based
  }

  /** The column number referred to by this position, starting at 1.
    */
  lazy val column: Int = offset - s.lineStarts(line - 1) + 1

  /** The contents of the line at the current offset (not including a newline).
    */
  lazy val lineContent: String = {
    val startIndex = s.lineStarts(line - 1)
    val endIndex = s.lineStarts(line)

    val result = source.substring(startIndex, endIndex)
    if (result.endsWith("\n")) result.dropRight(1) else result
  }

  /** The contents of the line at the current offset, decorated with
    * a caret indicating the column. Example:
    * {{{
    *   The content of the current line with a caret under the c.
    *       ^
    * }}}
    */
  def lineContentWithCaret: String = lineContent + "\n" + " " * (column-1) + "^"

  /** A string representation of this Position of the form `line.column`.
    */
  override lazy val toString = s"$line.$column"

}

