/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package laika.parse.core

/** An interface for streams of values that have positions.
 *
 * @author Martin Odersky
 * @author Adriaan Moors
 */
abstract class Reader {

  /** If this is a reader over character sequences, the underlying char sequence.
   *  If not, throws a `NoSuchMethodError` exception.
   */
  def source: java.lang.CharSequence

  def offset: Int

   /** Returns the first element of the reader
    */
  def first: Char

  /** Returns an abstract reader consisting of all elements except the first
   *
   * @return If `atEnd` is `true`, the result will be `this';
   *         otherwise, it's a `Reader` containing more elements.
   */
  def rest: Reader

  /** Returns an abstract reader consisting of all elements except the first `n` elements.
   */
  def drop(n: Int): Reader = {
    var r: Reader = this
    var cnt = n
    while (cnt > 0) {
      r = r.rest; cnt -= 1
    }
    r
  }

  /** The position of the first element in the reader.
   */
  def pos: Position

  /** `true` iff there are no more elements in this reader.
   */
  def atEnd: Boolean
}
