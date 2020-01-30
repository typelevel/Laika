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
import laika.parse.{Parsed, Parser, ParserContext}

/**
  * @author Jens Halm
  */
trait PrefixedParser[+T] extends Parser[T] { self =>

  
  def startChars: NonEmptySet[Char]
  
  def underlying: Parser[T]

  def parse (in: ParserContext): Parsed[T] = underlying.parse(in)


  override def ~ [U] (p: Parser[U]): PrefixedParser[T ~ U]       = PrefixedParser(startChars)(super.~(p))
  override def ~> [U] (p: Parser[U]): PrefixedParser[U]          = PrefixedParser(startChars)(super.~>(p))
  override def <~[U] (p: Parser[U]): PrefixedParser[T]           = PrefixedParser(startChars)(super.<~(p))
  override def flatMap[U] (f: T => Parser[U]): PrefixedParser[U] = PrefixedParser(startChars)(super.flatMap(f))
  override def >>[U] (fq: T => Parser[U]): PrefixedParser[U]     = PrefixedParser(startChars)(super.flatMap(fq))
  override def map[U] (f: T => U): PrefixedParser[U]             = PrefixedParser(startChars)(super.map(f))
  override def ^^[U] (f: T => U): PrefixedParser[U]              = PrefixedParser(startChars)(super.map(f))
  override def ^^^[U] (v: => U): PrefixedParser[U]               = PrefixedParser(startChars)(super.^^^(v))
  
  override def ^^?[U] (f: T => Either[String, U]): PrefixedParser[U] = PrefixedParser(startChars)(super.^^?(f))
  override def ^?[U] (f: PartialFunction[T, U], 
                      error: T => String = r => s"Constructor function not defined at $r"): PrefixedParser[U] = 
    PrefixedParser(startChars)(super.^?(f, error))
  

  def orElse[U >: T] (p: => PrefixedParser[U]): PrefixedParser[U] = PrefixedParser(startChars  ++ p.startChars)(super.orElse(p))

  def | [U >: T] (p: => PrefixedParser[U]): PrefixedParser[U] = PrefixedParser(startChars ++ p.startChars)(super.orElse(p))


}

object PrefixedParser {
  
  import cats.implicits._
  
  def apply[U] (sc: NonEmptySet[Char])(p: Parser[U]): PrefixedParser[U] = new PrefixedParser[U] {
    def startChars: NonEmptySet[Char] = sc
    override def underlying = p
  }

  def apply[U] (char: Char, chars: Char*)(p: Parser[U]): PrefixedParser[U] = new PrefixedParser[U] {
    def startChars: NonEmptySet[Char] = NonEmptySet.of(char, chars:_*)
    override def underlying = p
  }
  
}
