package laika.render

import java.io.{BufferedReader, Closeable, InputStreamReader, Reader}

import laika.ast.Path
import laika.parse.ParserContext
import laika.parse.markup.DocumentParser.ParserInput

import scala.io.Codec

// TODO - 0.12 - classpath resources will be embedded
object TempResourceProvider {

  def autoClose [R, T] (resource: R)(f: R => T): T = resource match {
    case c: Closeable => try f(resource) finally c.close
    case _ => f(resource)
  }
  
  def classPathParserInput (resourcePath: String, virtualPath: Path)(implicit codec: Codec): ParserInput = {
    val stream = getClass.getResourceAsStream(resourcePath)
    autoClose(stream){ in =>
      val reader = new BufferedReader(new InputStreamReader(in, codec.decoder))
      val content = readAll(reader)
      ParserInput(virtualPath, ParserContext(content))
    }
  }

  def readAll (reader: Reader): String = {

    val arr = new Array[Char](8 * 1024)
    val buffer = new StringBuilder
    var numCharsRead: Int = 0

    while ({numCharsRead = reader.read(arr, 0, arr.length); numCharsRead != -1}) {
      buffer.appendAll(arr, 0, numCharsRead)
    }

    buffer.toString
  }
  
}
