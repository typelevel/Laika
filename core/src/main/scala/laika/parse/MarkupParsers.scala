/*
 * Copyright 2013-2016 the original author or authors.
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

import scala.annotation.tailrec
import laika.parse.core._
import laika.parse.core.text.Characters

/** Base parsers that provide optimized low-level renderers for typical requirements
 *  of text markup parsers. In particular they are meant as an efficient replacement
 *  for scenarios where usually regex parsers are used. In cases where different parsers
 *  need to be tried for relatively short input sequences, regex parsers tend to be less
 *  efficient. Furthermore, these base parsers may also improve readability, as it
 *  allows to combine simple low-level parsers to higher-level parsers based on the
 *  Scala SDK combinator API, instead of producing long regexes which may be hard to read.  
 * 
 *  @author Jens Halm
 */
trait MarkupParsers extends BaseParsers {

  
  /** Implicit conversion that allows to pass a single
   *  character to the range-based `anyIn` parser. 
   */
  implicit def charToTraversable (char: Char): Traversable[Char] = Set(char)
  

  /** Succeeds at the end of a line, including the end of the input.
   *  Produces an empty string as a result and consumes any new line characters.
   */
  def eol: Parser[String] = Parser { in =>
      if (in.atEnd) Success("", in) 
      else if (in.first == '\n') Success("", in.rest)
      else if (in.first == '\r' && in.source.length > in.offset + 1 && in.source.charAt(in.offset + 1) == '\n') Success("", in.drop(2))
      else Failure(Message.ExpectedEOL, in)
  }  
  
  /** Succeeds at the end of the input.
   */
  def eof: Parser[String] = Parser { in =>
      if (in.atEnd) Success("", in) 
      else Failure(Message.ExpectedEOF, in)
  }  
  
  /** Succeeds at the start of the input.
   */
  val atStart: Parser[Unit] = Parser { in =>
    if (in.offset == 0) Success(success(()), in) 
    else Failure(Message.ExpectedStart, in)
  }
  
  /** Parses horizontal whitespace (space and tab).
   *  Always succeeds, consuming all whitespace found.
   */
  def ws: Characters = anyOf(' ','\t')
  
  /** Parses a simple reference name that only allows alphanumerical characters
   *  and the punctuation characters `-`, `_`, `.`, `:`, `+`.
   */
  val refName: Parser[String] = {
    val alphanum = anyWhile(c => Character.isDigit(c) || Character.isLetter(c)) min 1
    val symbol = anyOf('-', '_', '.', ':', '+') take 1
    
    alphanum ~ ((symbol ~ alphanum)*) ^^ { 
      case start ~ rest => start + (rest map { case a~b => a+b }).mkString
    }
  }

  
  /** API for specifying further constraints on the parsers provided by this base trait.
   * 
   *  For reading 3 or more `'*'` or `'+'`characters for example the constraint could
   *  be specified as follows:
   *  
   *  {{{
   *  anyOf('*','+') min 3
   *  }}}
   */
  class TextParser private[MarkupParsers] (newParser:           (Int, Int, Boolean, Boolean, Char => Boolean) => Parser[(String,Boolean)],
                                           minChar:             Int = 0,
                                           maxChar:             Int = 0,
                                           mustFailAtEOF:       Boolean = false,
                                           mustConsumeLastChar: Boolean = false,
                                           isStopChar:          Char => Boolean = c => false) extends Parser[String] {
    
    private val parser = newParser(minChar, maxChar, mustFailAtEOF, mustConsumeLastChar, isStopChar)
    
    /** Creates and returns a new parser that fails if it does not consume the specified minimum number
     *  of characters. It may still consume more characters in case of further matches. 
     */
    def min (count: Int): TextParser = new TextParser(newParser, count, maxChar, mustFailAtEOF, mustConsumeLastChar, isStopChar)
    
    /** Creates and returns a new parser that consumes at most the specified maximum number of characters. 
     *  Always succeeds, unless a minimum number of matches is also specified.
     */
    def max (count: Int): TextParser = new TextParser(newParser, minChar, count, mustFailAtEOF, mustConsumeLastChar, isStopChar)
    
    /** Creates and returns a new parser that consumes exactly the specified number of characters.
     *  Fails if there are less matches, but succeeds in case there are more matches, simply ignoring them.
     *  Calling `take 3` for example is equivalent to calling `min 3 max 3`.
     */
    def take (count: Int): TextParser = new TextParser(newParser, count, count, mustFailAtEOF, mustConsumeLastChar, isStopChar)
    
    private[parse] def failAtEOF = new TextParser(newParser, minChar, maxChar, true, mustConsumeLastChar, isStopChar)
  
    private[parse] def stopChars (chars: Char*) = new TextParser(newParser, minChar, maxChar, mustFailAtEOF, mustConsumeLastChar, charLookupFor(chars:_*))
    
    private[parse] def consumeLastChar = new TextParser(newParser, minChar, maxChar, mustFailAtEOF, true, isStopChar)
    
    private[parse] def applyInternal (in: Reader) = parser(in)
    
    def apply (in: Reader): ParseResult[String] = parser(in) match {
      case Success((result,_), next) => Success(result, next)
      case f: Failure => f
    }
  }
  
  
  /** Returns an optimized, Array-based lookup function 
   *  for the specified characters.
   */
  protected def optimizedCharLookup (chars: Char*): Char => Boolean = {
    val max = chars.max
    val lookup = new Array[Int](max + 1)
    
    for (c <- chars) lookup(c) = 1
    
    c:Char => c <= max && lookup(c) == 1
  }
  
  /** Returns an optimized, Array-based lookup function 
   *  for the specified ranges of characters.
   */
  protected def optimizedRangeLookup (ranges: Traversable[Char]*): Char => Boolean = {
    val max = ranges map (_.max) max
    val lookup = new Array[Int](max + 1)
    
    for (r <- ranges; c <- r) lookup(c) = 1
    
    c:Char => c <= max && lookup(c) == 1
  }
  
  private def charLookupFor (chars: Char*): Char => Boolean = {
    chars.length match {
      case 0 => c => false
      case 1 => val c = chars(0); _ == c
      case 2 => val c1 = chars(0); val c2 = chars(1); c => c == c1 || c == c2
      case _ => optimizedCharLookup(chars:_*)
    } 
  }
  
  /** Consumes any kind of input, always succeeds.
   *  This parser would consume the entire input unless a `max` constraint
   *  is specified.
   */
  val any: TextParser = anyWhileLegacy(c => true)
  
  /** Consumes any number of consecutive occurrences of the specified characters.
   *  Always succeeds unless a minimum number of required matches is specified.
   */
  def anyOf (chars: Char*): Characters = new Characters(charLookupFor(chars:_*))
  
  /** Consumes any number of consecutive characters that are not one of the specified characters.
   *  Always succeeds unless a minimum number of required matches is specified.
   */
  def anyBut (chars: Char*): TextParser = {
    val p: Char => Boolean = chars.length match {
      case 0 => c => true
      case 1 => val c = chars(0); _ != c
      case 2 => val c1 = chars(0); val c2 = chars(1); c => c != c1 && c != c2
      case _ => val lookup = optimizedCharLookup(chars:_*); !lookup(_)
    }
    anyWhileLegacy(p)
  }
  
  /** Consumes any number of consecutive characters that are not one of the specified characters.
   *  
   *  This parser is identical to the `anyBut` parser except for two differences: this parser fails
   *  if it reaches the end of the input without seeing any of the specified
   *  characters and it also consumes this final character, without adding it
   *  to the result. This parser is usually used when a construct like a span
   *  enclosed between two characters needs to be parsed.
   */
  def anyUntil (chars: Char*): TextParser = anyBut(chars:_*).failAtEOF.consumeLastChar
  
  /** Consumes any number of consecutive characters that are in one of the specified character ranges.
   *  Always succeeds unless a minimum number of required matches is specified.
   */
  def anyIn (ranges: Traversable[Char]*): Characters = {
    val p: Char => Boolean = {
      if (ranges.isEmpty) c => false
      else optimizedRangeLookup(ranges:_*)
    }
    new Characters(p)
  }

  private class MessageProviderFactory (minExpected: Int) {

    val msgFunction: Int => String = actual => s"expected at least $minExpected characters, got only $actual"

    def newProvider (actual: Int): MessageProvider = new MessageFunction(actual, msgFunction)

  }

  /** Consumes any number of consecutive characters which satisfy the specified predicate.
    *  Always succeeds unless a minimum number of required matches is specified.
    */
  def anyWhile (p: Char => Boolean): Characters = new Characters(p)


  private def anyWhileLegacy (p: Char => Boolean): TextParser = {
    
    def newParser (min: Int, max: Int, failAtEof: Boolean, consumeLastChar: Boolean, isStopChar: Char => Boolean) = {

      val msgProviderFactory = new MessageProviderFactory(min)

      Parser { in =>
        val source = in.source
        val end = source.length
        val maxOffset = if (max > 0) in.offset + max else end

        def result (offset: Int, consumeLast: Boolean = false, onStopChar: Boolean = false) = {
          if (offset - in.offset >= min) {
            val consumeTo = if (consumeLast) offset + 1 else offset
            Success((source.subSequence(in.offset, offset).toString, onStopChar), in.drop(consumeTo - in.offset))
          }
          else
            Failure(msgProviderFactory.newProvider(offset - in.offset), in)
        }

        @tailrec
        def parse (offset: Int): ParseResult[(String,Boolean)] = {
          if (offset == end) { if (failAtEof) Failure(Message.UnexpectedEOF, in) else result(offset) }
          else {
            val c = source.charAt(offset)
            if (!p(c)) result(offset, consumeLastChar)
            else if (offset == maxOffset) result(offset)
            else if (isStopChar(c)) result(offset, onStopChar = true)
            else parse(offset + 1)
          }
        }

        parse(in.offset)
      }
    }
    
    new TextParser(newParser)
  }
  
  /** Consumes any number of characters for which the specified parser fails on the corresponding offset.
   *  This parser fails if the end of input is reached without the specified parser ever succeeding or
   *  if the parser causes an Error result instead of a plain Failure or Success. 
   *  Further constraints like minimum or maximum number of required matching characters can be specified
   *  through the API of the returned `TextParser` instance.
   */
  def anyUntil (until: => Parser[Any]): TextParser = {
    
    def newParser (min: Int, max: Int, failAtEof: Boolean, consumeLastChar: Boolean, isStopChar: Char => Boolean) = {

      val msgProviderFactory = new MessageProviderFactory(min)

      Parser { in =>

        lazy val parser = until
        val maxOffset = if (max > 0) in.offset + max else in.source.length

        def result (resultOffset: Int, next: Reader, onStopChar: Boolean = false) = {
          if (resultOffset - in.offset >= min)
            Success((in.source.subSequence(in.offset, resultOffset).toString, onStopChar), next)
          else
            Failure(msgProviderFactory.newProvider(next.offset - in.offset), in)
        }

        @tailrec
        def parse (input: Reader): ParseResult[(String,Boolean)] = {
          if (input.atEnd && failAtEof) Failure(Message.UnexpectedEOF, in)
          else parser(input) match {
            case Success(_, next) => result(input.offset, next)
            case Failure(_, _)    =>
              if (input.offset == maxOffset) result(input.offset, input)
              else if (isStopChar(input.first)) result(input.offset, input, onStopChar = true)
              else parse(input.rest)
          }
        }

        parse(in)
      }
    }
    
    new TextParser(newParser, mustFailAtEOF = true)
  }
  
  
  /** Fully parses the specified input string and returns the result. 
   *  This function is expected to always succeed, errors would be considered a bug
   *  in this library, as the parsers treat all unknown or malformed markup as regular
   *  text.
   */
  def parseMarkup [T] (parser: Parser[T], source: String): T = {
    parseAll(parser, source) match {
      case Success(result,_) => result
      case f: Failure        => throw new MarkupParserException(f)
    }
  }
  
  /** Fully parses the input from the specified reader and returns the result. 
   *  This function is expected to always succeed, errors would be considered a bug
   *  in this library, as the parsers treat all unknown or malformed markup as regular
   *  text.
   */
  def parseMarkup [T] (parser: Parser[T], reader: Reader): T = {
    parseAll(parser, reader) match {
      case Success(result,_) => result
      case ns: Failure       => throw new MarkupParserException(ns)
    }
  }
  
  
  /** Exception thrown when parsing a text markup document or fragment fails.
   *  This can only happen due to a bug in this library, as the behaviour of the parser
   *  is to treat all unknown or malformed markup as regular text and always succeed.
   *  The result property holds the `NoSuccess` instance that caused the failure.
   */
  class MarkupParserException (val result: Failure) extends RuntimeException(result.toString)
  
  
}