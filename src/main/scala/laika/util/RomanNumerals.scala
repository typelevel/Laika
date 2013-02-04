package laika.util

import scala.annotation.tailrec
import com.sun.xml.internal.txw2.IllegalAnnotationException

/** Converts Roman numerals to integers and vice versa.
 *  Since there never have been universally accepted rules for Roman numerals,
 *  the conversion functions do not apply strict error checking, so some unusual or 
 *  illegal constructs may be supported, too. They do not prevent using the same
 *  symbol more than three times in a row for example. 
 */
object RomanNumerals {

  private case class Symbol (roman: String, value: Int, repeatable: Boolean = false)
  
  private val symbols = List(
    Symbol("M", 1000, true), 
    Symbol("CM", 900), 
    Symbol("D", 500), 
    Symbol("CD", 400), 
    Symbol("C", 100, true), 
    Symbol("XC", 90), 
    Symbol("L", 50), 
    Symbol("XL", 40), 
    Symbol("X", 10, true), 
    Symbol("IX", 9), 
    Symbol("V", 5), 
    Symbol("IV", 4), 
    Symbol("I", 1, true)
  ) 
  
  /** Converts from an integer to Roman numerals.
   *  The integer has to be between 1 and 4999.
   */
  def intToRoman (value: Int): String = {
    require(value > 0 && value < 5000, "Number must be between 1 and 4999") 
    
    ((value, "") /: symbols) {
      case ((remaining, result), symbol) => 
        val occurrences = remaining / symbol.value
        (remaining - occurrences * symbol.value, result + symbol.roman * occurrences)
    }._2 
  }
  
  /** Converts from Roman numerals to integer.
   */
  def romanToInt (roman: String): Int = {
    
    def convert (roman: String, lastSymbol: Symbol): Int = {
      symbols.filter(roman startsWith _.roman).sortBy(-_.roman.length) match {
        case (s @ Symbol(romanSymbol, value, repeatable)) :: _ =>
          if (s == lastSymbol && !repeatable) 
            throw new IllegalArgumentException("Symbol " + romanSymbol + " cannot be repeated")
          else if (value > lastSymbol.value) 
            throw new IllegalArgumentException("Illegal ordering of symbols: " + lastSymbol.roman + romanSymbol)
          value + convert(roman.substring(romanSymbol.length), s)
        case Nil if roman.isEmpty => 0
        case Nil => throw new IllegalArgumentException("Illegal Roman Numeral: " + roman)
      }
    }
    
    convert(roman, symbols(0))
  }        
  
}
