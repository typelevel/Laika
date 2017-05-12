/*
 * Copyright 2013-2017 the original author or authors.
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

package laika.parse.core.markup

import laika.parse.core.Parser
import laika.parse.core.text.DelimitedText
import laika.tree.Elements.{Block, Span}

/**
  * @author Jens Halm
  */
trait RecursiveParsers extends RecursiveSpanParsers {

  def recursiveBlocks (p: Parser[String]): Parser[Seq[Block]]

  def withRecursiveBlockParser [T] (p: Parser[T]): Parser[(String => List[Block], T)]

}

trait RecursiveSpanParsers extends EscapedTextParsers {

  def recursiveSpans (parser: Parser[String]): Parser[List[Span]]

  def recursiveSpans (parser: Parser[String],
                      additionalParsers: => Map[Char, Parser[Span]] = Map.empty): Parser[List[Span]]

  def recursiveSpans: Parser[List[Span]]

  def delimitedRecursiveSpans (textParser: DelimitedText[String]): Parser[List[Span]]

  def delimitedRecursiveSpans (textParser: DelimitedText[String],
                               additionalSpanParsers: => Map[Char, Parser[Span]]): Parser[List[Span]]

  def withRecursiveSpanParser [T] (p: Parser[T]): Parser[(String => List[Span], T)]

}

trait EscapedTextParsers {

  def escapedText(p: DelimitedText[String]): Parser[String]

  def escapedUntil(char: Char*): Parser[String]

}
