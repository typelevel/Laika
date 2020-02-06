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
import laika.parse.text.PrefixedParser

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
  
}
