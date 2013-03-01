package laika.parse.rst

import scala.annotation.tailrec

trait WhitespacePreprocessor {

  
  val tabStops = 4
  
  
  def processWhitespace (input: String) = {
    
    val end = input.length
    val buf = new StringBuilder

    var i = 0
    var col = 0
    var cur = 0
    
    def cut (add: String) = {
      buf ++= input.substring(cur, i) ++= add
      cur = i + 1
    }
    
    while (i < end) {
      input.charAt(i) match {
        case '\f' | '\u000b' => 
          cut(" ")
          col += 1
        case '\r' =>
          cut("")
        case '\n' =>
          col = 0
        case '\t' => 
          val spaces = tabStops - (col % tabStops)
          cut(" " * spaces)
          col += spaces
        case _    => 
          col += 1
      }
      i += 1
    }
    
    buf ++= input.substring(cur)
    buf.toString
  }
  
  
}