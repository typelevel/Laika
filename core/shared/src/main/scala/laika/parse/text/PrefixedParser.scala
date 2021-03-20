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

package laika.parse.text

import cats.data.NonEmptySet
import laika.ast.~
import laika.parse.{Parsed, Parser, SourceCursor, SourceFragment}

/** A parser that is associated with a non-empty set of trigger 
  * characters for performance optimizations.
  * 
  * There is usually no need to create such a parser manually,
  * as some of the basic building blocks in `TextParsers` create
  * such a parser (e.g. the `literal`, `oneOf` or `someOf`
  * parsers).
  *
  * This set only has only an effect when this parser is used in
  * an optimized parser for recursive spans, meaning it is 
  * either registered as a top-level parser (with `SpanParser.standalone`
  * or `SpanParser.recursive`) or passed to a custom span parser
  * with `InlineParser.embed`. In all other use cases this
  * parser behaves just like plain parser.
  * 
  * @author Jens Halm
  */
trait PrefixedParser[+T] extends Parser[T] { self =>

  /** The set of trigger characters that can start this parser.
    */
  def startChars: NonEmptySet[Char]

  /** The underlying parser that may be optimized based on the specified
    * start characters.
    */
  def underlying: Parser[T]

  def parse (in: SourceCursor): Parsed[T] = underlying.parse(in)


  override def ~ [U] (p: Parser[U]): PrefixedParser[T ~ U]       = PrefixedParser(startChars)(super.~(p))
  override def ~> [U] (p: Parser[U]): PrefixedParser[U]          = PrefixedParser(startChars)(super.~>(p))
  override def <~[U] (p: Parser[U]): PrefixedParser[T]           = PrefixedParser(startChars)(super.<~(p))

  override def ~ (value: String): PrefixedParser[T ~ String]     = this.~(TextParsers.literal(value))
  override def ~> (value: String): PrefixedParser[String]        = this.~>(TextParsers.literal(value))
  override def <~ (value: String): PrefixedParser[T]             = this.<~(TextParsers.literal(value))
  
  override def flatMap[U] (f: T => Parser[U]): PrefixedParser[U] = PrefixedParser(startChars)(super.flatMap(f))
  override def >>[U] (fq: T => Parser[U]): PrefixedParser[U]     = PrefixedParser(startChars)(super.flatMap(fq))
  override def map[U] (f: T => U): PrefixedParser[U]             = PrefixedParser(startChars)(super.map(f))
  override def ^^[U] (f: T => U): PrefixedParser[U]              = PrefixedParser(startChars)(super.map(f))
  override def as[U] (v: => U): PrefixedParser[U]               = PrefixedParser(startChars)(super.as(v))
  
  override def evalMap[U] (f: T => Either[String, U]): PrefixedParser[U] = PrefixedParser(startChars)(super.evalMap(f))

  override def collect[U, V >: T] (f: PartialFunction[T, U],
                                   error: V => String = (r:V) => s"Constructor function not defined at $r"): PrefixedParser[U] =
    PrefixedParser(startChars)(super.collect(f, error))

  /**  Applies the specified parser when this parser fails.
    *
    *  `a.orElse(b)` succeeds if either of the parsers succeeds.
    *
    *  This is a specialized variant of the `orElse` method of the
    *  base trait that preserves the nature of the `PrefixedParser`
    *  if both original parsers implement this trait.
    */
  def orElse[U >: T] (p: => PrefixedParser[U]): PrefixedParser[U] = PrefixedParser(startChars  ++ p.startChars)(super.orElse(p))

  /**  Applies the specified parser when this parser fails.
    *
    *  `a | b` succeeds if either of the parsers succeeds.
    *
    *  This is a specialized variant of the `|` method of the
    *  base trait that preserves the nature of the `PrefixedParser`
    *  if both original parsers implement this trait.
    */
  def | [U >: T] (p: => PrefixedParser[U]): PrefixedParser[U] = PrefixedParser(startChars ++ p.startChars)(super.orElse(p))

  override def | (value: String)(implicit ev: T <:< String): PrefixedParser[String] = map(ev).orElse(TextParsers.literal(value))

  override def withCursor: PrefixedParser[(T, SourceFragment)] = PrefixedParser(startChars)(super.withCursor)
  
  override def cursor: PrefixedParser[SourceFragment] = PrefixedParser(startChars)(super.cursor)
  
  override def source: PrefixedParser[String] = PrefixedParser(startChars)(super.source)

  
  @deprecated("use withCursor which contains the input string", "0.17.0")
  override def withSource: PrefixedParser[(T, String)] = withCursor.map(t => (t._1, t._2.input))
}

/** Factories and utilities for creating or processing PrefixedParser instances.
  */
object PrefixedParser {
  
  import cats.implicits._

  /** Creates a new parser that is only triggered when a character in the specified 
    * set is seen on the input.
    */
  def apply[U] (sc: NonEmptySet[Char])(p: Parser[U]): PrefixedParser[U] = new PrefixedParser[U] {
    def startChars: NonEmptySet[Char] = sc
    override def underlying = p
  }

  /** Creates a new parser that is only triggered when one of the specified characters 
    * is seen on the input.
    */
  def apply[U] (char: Char, chars: Char*)(p: Parser[U]): PrefixedParser[U] = new PrefixedParser[U] {
    def startChars: NonEmptySet[Char] = NonEmptySet.of(char, chars:_*)
    override def underlying = p
  }

  /** Creates a mapping from start characters to their corresponding parser
    * from the specified sequence of PrefixedParsers. If a character is
    * a trigger for more than one parser they will be combined using `orElse`
    * where the parser which comes first in the sequence has higher precedence.
    */
  def mapAndMerge[T] (parsers: Seq[PrefixedParser[T]]): Map[Char, Parser[T]] = parsers
    .flatMap { parserDef =>
      parserDef.startChars.toList.map(c => (c, parserDef))
    }
    .groupBy(_._1)
    .map {
      case (char, definitions) => (char, definitions.map(_._2).reduceLeft(_ | _))
    }
  
  private[laika] def fromLegacyMap[T] (map: Map[Char, Parser[T]]): Seq[PrefixedParser[T]] =
    map.toSeq.map { case (c, p) => PrefixedParser(c)(p) }
  
}
