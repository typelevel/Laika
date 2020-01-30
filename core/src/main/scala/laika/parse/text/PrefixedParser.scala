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


  override def ~ [U] (p: Parser[U]): PrefixedParser[T ~ U]       = PrefixedParser(super.~(p), startChars)
  override def ~> [U] (p: Parser[U]): PrefixedParser[U]          = PrefixedParser(super.~>(p), startChars)
  override def <~[U] (p: Parser[U]): PrefixedParser[T]           = PrefixedParser(super.<~(p), startChars)
  override def flatMap[U] (f: T => Parser[U]): PrefixedParser[U] = PrefixedParser(super.flatMap(f), startChars)
  override def >>[U] (fq: T => Parser[U]): PrefixedParser[U]     = PrefixedParser(super.flatMap(fq), startChars)
  override def map[U] (f: T => U): PrefixedParser[U]             = PrefixedParser(super.map(f), startChars)
  override def ^^[U] (f: T => U): PrefixedParser[U]              = PrefixedParser(super.map(f), startChars)
  override def ^^^[U] (v: => U): PrefixedParser[U]               = PrefixedParser(super.^^^(v), startChars)
  
  override def ^^?[U] (f: T => Either[String, U]): PrefixedParser[U]                   = PrefixedParser(super.^^?(f), startChars)
  override def ^?[U] (f: PartialFunction[T, U], 
                      error: T => String = r => s"Constructor function not defined at $r"): PrefixedParser[U] = 
    PrefixedParser(super.^?(f, error), startChars)
  

  def orElse[U >: T] (p: => PrefixedParser[U]): PrefixedParser[U] = PrefixedParser(super.orElse(p), startChars ++ p.startChars)

  def | [U >: T] (p: => PrefixedParser[U]): PrefixedParser[U] = PrefixedParser(super.orElse(p), startChars ++ p.startChars)


}

object PrefixedParser {
  
  def apply[U] (p: Parser[U], sc: NonEmptySet[Char]): PrefixedParser[U] = new PrefixedParser[U] {
    def startChars: NonEmptySet[Char] = sc
    override def underlying = p
  }
  
}
