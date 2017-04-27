/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package laika.parse.core

/** An object encapsulating basic character constants.
 *
 * @author Martin Odersky, Adriaan Moors
 */
object CharSequenceReader {
  final val EofCh = '\u001a'
}

/** A character array reader reads a stream of characters (keeping track of their positions)
 * from an array.
 *
 * @param s the source sequence
 * @param offset  starting offset.
 * @author Martin Odersky
 */
class CharSequenceReader(s: Source,
                         override val offset: Int) extends Reader {

  val source = s.value

  import CharSequenceReader._

  /** Construct a `CharSequenceReader` with its first element at
   *  `source(0)` and position `(1,1)`.
   */
  def this(source: String) = this(Source(source), 0)

  /** Returns the first element of the reader, or EofCh if reader is at its end.
   */
  def first =
    if (offset < source.length) source.charAt(offset) else EofCh

  /** Returns a CharSequenceReader consisting of all elements except the first.
   *
   * @return If `atEnd` is `true`, the result will be `this`;
   *         otherwise, it's a `CharSequenceReader` containing the rest of input.
   */
  def rest: CharSequenceReader =
    if (offset < source.length) new CharSequenceReader(s, offset + 1)
    else this

  /** The position of the first element in the reader.
   */
  def pos: Position = new Position(s, offset)

  /** true iff there are no more elements in this reader (except for trailing
   *  EofCh's)
   */
  def atEnd = offset >= source.length

  /** Returns an abstract reader consisting of all elements except the first
   *  `n` elements.
   */
  override def drop(n: Int): CharSequenceReader =
    new CharSequenceReader(s, offset + n)
}
