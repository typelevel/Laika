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

package laika.parse

import laika.ast.~
import laika.parse.text.{PrefixedParser, TextParsers}

/** Collection of extension methods that helps keeping parser definitions concise.
  * 
  * It includes extension methods on string that allows to use string literals in
  * parser definitions like `"{" ~ anyNot('}') ~ "}"` and extensions for parsers
  * for particular target types, e.g. `concat` for a `Parser[Seq[A], Seq[A]]`
  * and `mapN` for any parser `Parser[A ~ B ~ C]` (up to 5 concatenated elements).
  * 
  * @author Jens Halm
  */
object implicits {

  implicit class PrependParserOps[T] (val p: Parser[T ~ Seq[T]]) extends AnyVal {
    def concat: Parser[Seq[T]] = p.map { case x ~ xs => x +: xs }
  }

  implicit class PrependPrefixedParserOps[T] (val p: PrefixedParser[T ~ Seq[T]]) extends AnyVal {
    def concat: PrefixedParser[Seq[T]] = p.map { case x ~ xs => x +: xs }
  }

  implicit class Prepend2ParserOps[T] (val p: Parser[T ~ Seq[T] ~ Seq[T]]) extends AnyVal {
    def concat: Parser[Seq[T]] = p.map { case x ~ xs ~ ys => x +: xs ++: ys }
  }

  implicit class Prepend2PrefixedParserOps[T] (val p: PrefixedParser[T ~ Seq[T] ~ Seq[T]]) extends AnyVal {
    def concat: PrefixedParser[Seq[T]] = p.map { case x ~ xs ~ ys => x +: xs ++: ys }
  }

  implicit class Seq2ParsersOps[T] (val p: Parser[Seq[T] ~ Seq[T]]) extends AnyVal {
    def concat: Parser[Seq[T]] = p.map { case x ~ xs => x ++ xs }
  }

  implicit class Seq2PrefixedParsersOps[T] (val p: PrefixedParser[Seq[T] ~ Seq[T]]) extends AnyVal {
    def concat: PrefixedParser[Seq[T]] = p.map { case xs ~ ys => xs ++ ys }
  }

  implicit class Seq3ParsersOps[T] (val p: Parser[Seq[T] ~ Seq[T] ~ Seq[T]]) extends AnyVal {
    def concat: Parser[Seq[T]] = p.map { case xs ~ ys ~ zs => xs ++ ys ++ zs }
  }

  implicit class Seq3PrefixedParsersOps[T] (val p: PrefixedParser[Seq[T] ~ Seq[T] ~ Seq[T]]) extends AnyVal {
    def concat: PrefixedParser[Seq[T]] = p.map { case xs ~ ys ~ zs => xs ++ ys ++ zs }
  }

  implicit class Seq4ParsersOps[T] (val p: Parser[Seq[T] ~ Seq[T] ~ Seq[T] ~ Seq[T]]) extends AnyVal {
    def concat: Parser[Seq[T]] = p.map { case s1 ~ s2 ~ s3 ~ s4 => s1 ++ s2 ++ s3 ++ s4 }
  }

  implicit class Seq4PrefixedParsersOps[T] (val p: PrefixedParser[Seq[T] ~ Seq[T] ~ Seq[T] ~ Seq[T]]) extends AnyVal {
    def concat: PrefixedParser[Seq[T]] = p.map { case s1 ~ s2 ~ s3 ~ s4 => s1 ++ s2 ++ s3 ++ s4 }
  }

  implicit class SeqStringParserOps[T] (val p: Parser[Seq[String]]) extends AnyVal {
    def mkLines: Parser[String] = p.map { _.mkString("\n") }
  }

  implicit class SeqStringPrefixedParserOps[T] (val p: PrefixedParser[Seq[String]]) extends AnyVal {
    def mkLines: PrefixedParser[String] = p.map { _.mkString("\n") }
  }

  implicit class StringParserOps[T] (val p: Parser[String]) extends AnyVal {
    def trim: Parser[String] = p.map(_.trim)
  }

  implicit class PrefixedStringParserOps[T] (val p: PrefixedParser[String]) extends AnyVal {
    def trim: PrefixedParser[String] = p.map(_.trim)
  }
  
  implicit class Map2Ops[A, B] (val p: Parser[A ~ B]) extends AnyVal {
    def mapN[Z] (f: (A,B) => Z): Parser[Z] = p.map { case a ~ b => f(a,b) }
  }

  implicit class PrefixedMap2Ops[A, B] (val p: PrefixedParser[A ~ B]) extends AnyVal {
    def mapN[Z] (f: (A,B) => Z): PrefixedParser[Z] = p.map { case a ~ b => f(a,b) }
  }

  implicit class Map3Ops[A, B, C] (val p: Parser[A ~ B ~ C]) extends AnyVal {
    def mapN[Z] (f: (A,B,C) => Z): Parser[Z] = p.map { case a ~ b ~ c => f(a,b,c) }
  }

  implicit class PrefixedMap3Ops[A, B, C] (val p: PrefixedParser[A ~ B ~ C]) extends AnyVal {
    def mapN[Z] (f: (A,B,C) => Z): PrefixedParser[Z] = p.map { case a ~ b ~ c => f(a,b,c) }
  }

  implicit class Map4Ops[A, B, C, D] (val p: Parser[A ~ B ~ C ~ D]) extends AnyVal {
    def mapN[Z] (f: (A,B,C,D) => Z): Parser[Z] = p.map { case a ~ b ~ c ~ d => f(a,b,c,d) }
  }

  implicit class PrefixedMap4Ops[A, B, C, D] (val p: PrefixedParser[A ~ B ~ C ~ D]) extends AnyVal {
    def mapN[Z] (f: (A,B,C,D) => Z): PrefixedParser[Z] = p.map { case a ~ b ~ c ~ d => f(a,b,c,d) }
  }

  implicit class Map5Ops[A, B, C, D, E] (val p: Parser[A ~ B ~ C ~ D ~ E]) extends AnyVal {
    def mapN[Z] (f: (A,B,C,D,E) => Z): Parser[Z] = p.map { case a ~ b ~ c ~ d ~ e => f(a,b,c,d,e) }
  }

  implicit class PrefixedMap5Ops[A, B, C, D, E] (val p: PrefixedParser[A ~ B ~ C ~ D ~ E]) extends AnyVal {
    def mapN[Z] (f: (A,B,C,D,E) => Z): PrefixedParser[Z] = p.map { case a ~ b ~ c ~ d ~ e => f(a,b,c,d,e) }
  }

  implicit class LiteralStringOps (val str: String) extends AnyVal {
    def ~ [U] (p: Parser[U]): PrefixedParser[String ~ U] = TextParsers.literal(str) ~ p
    def ~ [U] (value: String): PrefixedParser[String ~ String] = TextParsers.literal(str) ~ TextParsers.literal(value)
    def <~ [U] (p: Parser[U]): PrefixedParser[String] = TextParsers.literal(str) <~ p
    def <~ [U] (value: String): PrefixedParser[String] = TextParsers.literal(str) <~ TextParsers.literal(value)
    def ~> [U] (p: Parser[U]): PrefixedParser[U] = TextParsers.literal(str) ~> p
    def ~> [U] (value: String): PrefixedParser[String] = TextParsers.literal(str) ~> TextParsers.literal(value)
    def | (p: PrefixedParser[String]): PrefixedParser[String] = TextParsers.literal(str) | p
    def | (p: Parser[String]): Parser[String] = TextParsers.literal(str) | p
    def | (value: String): PrefixedParser[String] = TextParsers.literal(str) | TextParsers.literal(value)
  }
  
}
