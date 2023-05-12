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

package laika.parse.code.common

import cats.data.NonEmptySet
import laika.ast.{ CategorizedCode, CodeSpan, CodeSpanSequence }
import laika.parse.code.CodeSpanParser
import laika.parse.text.PrefixedParser

/** Convenient base trait that allows for passing the implementing
  * instances anywhere either a `PrefixedParser[Seq[CodeSpan]]` or
  * a `CodeSpanParser` is required for easier composition.
  *
  * @author Jens Halm
  */
trait CodeParserBase extends PrefixedParser[Seq[CodeSpan]] with CodeSpanParser {

  def underlying: PrefixedParser[Seq[CodeSpan]]

  override def startChars: NonEmptySet[Char] = underlying.startChars

  override def parsers: Seq[PrefixedParser[CategorizedCode]] = Seq(map(CodeSpanSequence(_)))

}
