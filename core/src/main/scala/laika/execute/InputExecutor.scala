package laika.execute

import java.io.Reader

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
  
}
