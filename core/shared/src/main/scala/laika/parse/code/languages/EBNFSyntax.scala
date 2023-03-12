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

package laika.parse.code.languages

import cats.data.NonEmptyList
import laika.bundle.SyntaxHighlighter
import laika.parse.code.{ CodeCategory, CodeSpanParser }
import laika.parse.code.common.{ Identifier, StringLiteral }
import laika.parse.code.implicits._
import laika.parse.builders._
import laika.parse.implicits._

/** This highlighter is very loosely defined with the aim to work for most of
  * the dozens of flavors of EBNF that are widely used.
  *
  * @author Jens Halm
  */
object EBNFSyntax extends SyntaxHighlighter {

  val language: NonEmptyList[String] = NonEmptyList.of("ebnf")

  val declarationName: CodeSpanParser = CodeSpanParser.onLineStart {
    val declName = Identifier.alphaNum.withCategory(CodeCategory.DeclarationName)
    (declName ~ ws.asCode() ~ ("::=" | "=").asCode(CodeCategory.Tag.Punctuation)).mapN {
      Seq(_, _, _)
    }
  }

  val spanParsers: Seq[CodeSpanParser] = Seq(
    StringLiteral.singleLine("‘", "’"),
    StringLiteral.singleLine("\"", "\""),
    StringLiteral.singleLine("'", "'"),
    declarationName,
    Identifier.alphaNum
  )

}
