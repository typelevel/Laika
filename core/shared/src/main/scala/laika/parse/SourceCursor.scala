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

import cats.data.NonEmptyChain
import laika.ast.Path

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable.ArrayBuffer

/** Represents the state and context of a parsing operation,
  * containing the input string as well as positional information.
  *
  * @author Jens Halm
  */
trait SourceCursor {
  
  type Self <: SourceCursor

  /** The full input string, containing the string portions before and after the current offset.
    */
  def input: String

  /** The length of the input of this cursor.
    */
  def length: Int = input.length
  
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
  def charAt (relativeOffset: Int): Char = input.charAt(offset + relativeOffset)

  /** Captures a string containing the specified number of characters from the current offset.
    * If the number of remaining characters is less than the specified number, all remaining
    * characters will be returned.
    */
  def capture (numChars: Int): String

  /** Consumes the specified number of characters, returning a new `SourceCursor` with the new offset.
    */
  def consume (numChars: Int): Self

  /** The source for the root input, positioned to match the offset of this (potentially nested) source.
    */
  def root: RootSource
  
  /** The current position in the input string.
    */
  def position: Position

  /** The (virtual) path of the document this input originates from; may be empty in case of generated sources.
    */
  def path: Option[Path] = root.path
  
  /** The nest level of this cursor in case of recursive parsing. */
  def nestLevel: Int
  
  /** Create a new instance of this cursor with the nestLevel incremented. */
  def nextNestLevel: Self

  /** Returns a new `SourceCursor` with the input string being reversed,
    * but pointing to the same character as this context.
    *
    * This is a low-level optimization for parsers that look for strings like email addresses where the first character 
    * is not significant, so that parsing backwards from any `@` encountered in the input provided better performance.
    */
  def reverse: Self
  
  protected def canConsume (maxChars: Int): Int =
    if (maxChars > 0) Math.min(remaining, maxChars)
    else Math.max(offset * -1, maxChars)
  
}

/** Represents any source cursor other than the root cursor and it is mandated by some APIs that
  * solely deal with recursive parsing where the root input will never be used as the source for the parser.
  */
trait SourceFragment extends SourceCursor {

  type Self <: SourceFragment
  
}

/** A root source represents the full input string of a parsing operation.
  * 
  * In a single-pass parser like those for HOCON or CSS, only `RootCursor` instances will be used for the entire
  * parsing operation.
  * 
  * In a multi-pass parser like those for text markup, a `RootCursor` is only used for the first pass, 
  * whereas the subsequent passes on parts of the input are performed with the other `SourceCursor` implementations.
  * 
  * For this reason this type of cursor is only meant to be used for creating a root cursor for the input holding
  * the whole document (e.g. the entire markup document or the full template).
  * 
  * For creating a cursor for a fragment of the input, either `BlockSource` or `LineSource` must be used
  * to preserve position tracking in relation to the root input.
  */
class RootSource (inputRef: InputString, val offset: Int, val nestLevel: Int) extends SourceCursor {

  type Self = RootSource
  
  val input: String = inputRef.value

  def atEnd: Boolean = offset >= input.length

  def remaining: Int = input.length - offset

  def capture (numChars: Int): String = {
    require(numChars >= 0, "numChars cannot be negative")
    
    val endIndex = Math.min(input.length, offset + numChars)
    input.substring(offset, endIndex)
  }

  def consume (numChars: Int): RootSource = {
    val toConsume = canConsume(numChars)
    if (toConsume != 0) new RootSource(inputRef, offset + numChars, nestLevel)
    else this
  }

  val root: RootSource = this
  
  lazy val position: Position = if (inputRef.isReverse) reverse.position else new Position(inputRef, offset)

  def nextNestLevel: RootSource = new RootSource(inputRef, offset, nestLevel + 1)
  
  def reverse: RootSource = new RootSource(inputRef.reverse, remaining, nestLevel)

  override def hashCode(): Int = (input, offset, nestLevel).hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case rs: RootSource => rs.input == input &&
      rs.offset == offset &&
      rs.nestLevel == nestLevel
    case _ => false
  }

  override def toString: String = s"RootSource(offset $offset - length ${input.length})"
}

/** A line source represents all or part of a single line from the root input source.
  *
  * Such a source will be used in multi-pass parsers, particularly block-level parsers,
  * where the root parser might strip some markup decoration from each line and then pass the result
  * down to the next recursion. 
  * In such a case each line might have a different x-offset from the root input.
  * The use of this instance ensures that the correct position can still be tracked.
  * 
  * A `LineSource` is usually used as part of a `BlockSource` and less frequently on its own.
  */
class LineSource private (val input: String, private val parentRef: SourceCursor, val offset: Int, val nestLevel: Int) extends SourceFragment {

  /*
  Implementation Note: 
  The (private) parentRef property always holds a source with an offset that corresponds to the 0 offset 
  of this line source, creating the instance for the public parent property with a matching offset lazily when needed.
  */
  
  type Self = LineSource
  
  def atEnd: Boolean = offset >= input.length

  def remaining: Int = input.length - offset

  def capture (numChars: Int): String = {
    require(numChars >= 0, "numChars cannot be negative")

    val endIndex = Math.min(input.length, offset + numChars)
    input.substring(offset, endIndex)
  }

  def consume (numChars: Int): LineSource = {
    val toConsume = canConsume(numChars)
    if (toConsume != 0) new LineSource(input, parentRef, offset + toConsume, nestLevel)
    else this
  }

  lazy val parent: SourceCursor = parentRef.consume(offset)
  
  lazy val root: RootSource = parent.root
  
  lazy val position: Position = root.position

  def nextNestLevel: LineSource = new LineSource(input, parentRef, offset, nestLevel + 1)
  
  def reverse: LineSource = new LineSource(input.reverse, parentRef.consume(input.length).reverse, remaining, nestLevel)

  @nowarn("cat=deprecation")
  def trim: LineSource = {
    val spacesRemoved = input.prefixLength(_ == ' ')
    val moveOffset = Math.min(offset, spacesRemoved)
    new LineSource(input.trim, parentRef.consume(spacesRemoved), offset - moveOffset, nestLevel)
  }
  
  override def hashCode(): Int = (input, root.offset, offset, nestLevel).hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case ls: LineSource => 
      ls.input == input && 
      ls.root.offset == root.offset &&
      ls.offset == offset &&
      ls.nestLevel == nestLevel
    case _ => false
  }

  override def toString: String = s"LineSource(offset $offset - length ${input.length} - root offset ${root.offset})"
}

/** Companion for creating LineSource instances.
  */
object LineSource {

  /** Creates a new instance for the specified input and parent source. 
    * The input is used by parsers whereas the parent source is only used for error messages.
    * 
    * There are some expectations which are not enforced to allow for the necessary freedom to handle edge cases:
    * 
    * - The parent source is expected to be at an offset that corresponds to the beginning of the input
    *   of this new instance.
    * - The input of the parent is expected to contain the full input of this source at its current offset,
    *   to ensure position tracking is accurate in case of errors.
    *   
    * Some minor deviations from these rules can be allowed, if they do not diminish the accuracy of position tracking.
    * For example, if this input has all line breaks replaced by spaces, but the parent does not, the positions would
    * still align.
    */
  def apply (input: String, parent: SourceCursor): LineSource = new LineSource(input, parent, 0, parent.nestLevel)
}

/** A block source represents the source for a block level element where each individual line might
  * have a different x-offset to the root source.
  *
  * This type of source is essential to preserve position tracking in recursive block parsing.
  * For block level markup the first passes have to identify the type of block and remove their markup decoration.
  * The remaining inline markup is then passed to inline parsers, but the input to those parsers is no longer
  * a consecutive substring of the root input, making it hard to provide exact positions in error messages.
  * 
  * This type of source cursor solves this issue by providing a view to parsers that looks like a consecutive
  * string of inline markup without the stripped decoration, while maintaining the x- and y-offsets of each line
  * in relation to the root source.
  * 
  * Such a source will be used in multi-pass parsers, where the root parser might strip some markup decoration 
  * from each line and then pass the result down to the next recursion. 
  * In such a case each line might have a different x-offset from the root input.
  * The use of this instance ensures that the correct position can still be tracked.
  */
class BlockSource (inputRef: InputString, val lines: NonEmptyChain[LineSource], val offset: Int, val nestLevel: Int) extends SourceFragment {

  type Self = BlockSource
  
  lazy val input: String = inputRef.value 
  
  def atEnd: Boolean = offset >= input.length

  def remaining: Int = input.length - offset

  def capture (numChars: Int): String = {
    require(numChars >= 0, "numChars cannot be negative")

    val endIndex = Math.min(input.length, offset + numChars)
    input.substring(offset, endIndex)
  }

  def consume (numChars: Int): BlockSource = {
    val toConsume = canConsume(numChars)
    if (toConsume != 0) new BlockSource(inputRef, lines, offset + toConsume, nestLevel)
    else this
  }

  private lazy val activeLine: LineSource = {
    @tailrec def posFromLine (remainingLines: List[LineSource], remainingOffset: Int): (LineSource, Int) = {
      val lineLength = remainingLines.head.length
      if (lineLength >= remainingOffset) (remainingLines.head, remainingOffset)
      else if (remainingLines.tail.isEmpty) (remainingLines.head, lineLength)
      else posFromLine(remainingLines.tail, remainingOffset - (lineLength + 1))
    }
    val (lineSource, lineOffset) = posFromLine(lines.toChain.toList, offset)
    lineSource.consume(lineOffset - lineSource.offset)
  }
  
  lazy val root: RootSource = activeLine.root
  
  lazy val position: Position = activeLine.position
  
  override val path: Option[Path] = inputRef.path

  def nextNestLevel: BlockSource = new BlockSource(inputRef, lines, offset, nestLevel + 1)
  
  def reverse: BlockSource = new BlockSource(inputRef.reverse, lines.reverse.map(_.reverse), remaining, nestLevel)

  override def hashCode(): Int = (input, lines.iterator, offset, nestLevel).hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case bs: BlockSource => bs.input == input &&
      bs.lines.iterator.sameElements(lines.iterator) &&
      bs.offset == offset &&
      bs.nestLevel == nestLevel
    case _ => false
  }

  override def toString: String = s"BlockSource(offset $offset - length ${input.length} - root offset ${root.offset})"
}

/** Companion for creating BlockSource instances.
  */
object BlockSource {
  
  import cats.syntax.all._

  /** Creates a new block source for the specified lines.
    * Each line can have a different x- and y-offset from the root input as they may have been obtained
    * by a previously applied block parser that stripped some decoration.
    */
  def apply (lines: NonEmptyChain[LineSource]): BlockSource = {
    val trimmedLines = lines.map { line =>
      if (line.input.endsWith("\n")) LineSource(line.input.dropRight(1), line.parent) else line
    }
    val input = new InputString(trimmedLines.map(_.input).mkString_("\n"))
    new BlockSource(input, trimmedLines, 0, lines.head.nestLevel)
  }

  /** Creates a new block source for the specified lines.
    * Each line can have a different x- and y-offset from the root input as they may have been obtained
    * by a previously applied block parser that stripped some decoration.
    */
  def apply (firstLine: LineSource, rest: LineSource*): BlockSource = {
    apply(NonEmptyChain(firstLine, rest:_*))
  }
}

/** Represents a generated source, where an AST node has been created programmatically and cannot be
  * traced back to the corresponding input source.
  */
object GeneratedSource extends SourceFragment {
  type Self = this.type
  def input: String = ""
  def offset: Int = 0
  def remaining: Int = 0
  def atEnd: Boolean = true
  def capture(numChars: Int): String = ""
  def consume(numChars: Int): this.type = this
  def root: RootSource = new RootSource(InputString.empty, 0, 0)
  def position: Position = new Position(InputString.empty, 0)
  def nestLevel: Int = 0
  def nextNestLevel: this.type = this
  def reverse: this.type = this
}

/** Companion for creating new `SourceCursor` instances.
  * This type of constructor is only meant to be used for creating a root cursor for the input holding
  * the whole document (e.g. the entire markup document or the full template).
  * 
  * For creating a cursor for a fragment of the input, either `BlockSource` or `LineSource` must be used
  * to preserve position tracking in relation to the root input.
  */
object SourceCursor {

  /** Builds a new instance for the specified input string.
    */
  def apply (input: String): SourceCursor = new RootSource(new InputString(input), 0, 0)

  /** Builds a new instance for the specified input string and source path.
    */
  def apply (input: String, path: Path): SourceCursor = new RootSource(new InputString(input, Some(path)), 0, 0)

}

/** Represents the input string for a parsing operation.
  */
private[parse] class InputString (val value: String, val path: Option[Path] = None, val isReverse: Boolean = false) {

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
  lazy val reverse = new InputString(value.reverse, path, !isReverse)

}

private[parse] object InputString {
  val empty: InputString = new InputString("")
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
class Position(s: InputString, offset: Int) {

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

