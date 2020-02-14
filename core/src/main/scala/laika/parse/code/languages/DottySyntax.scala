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
import laika.parse.builders._
import laika.parse.implicits._
import laika.parse.code.{CodeCategory, CodeSpanParser}
import laika.parse.code.common.{CharLiteral, Keywords}

/**
  * @author Jens Halm
  */
object DottySyntax extends SyntaxHighlighter {

  import ScalaSyntax._

  val language: NonEmptyList[String] = NonEmptyList.of("dotty")

  /** Soft keywords would require too much context to be fully accurate.
    * For example, we do not even attempt to detect inline matches.
    * It should be sufficient to be right for the most basic cases.
    */
  val softKeywords: CodeSpanParser = CodeSpanParser(CodeCategory.Keyword) {
    "opaque"    <~ lookAhead(ws ~ "type") |
    "open"      <~ lookAhead(ws ~ "class") |
    "as"        <~ lookAhead(ws ~ identifier) |
    "derives"   <~ lookAhead(ws ~ identifier) |
    "extension" <~ lookAhead(ws ~ opt(identifier ~ ws) ~ "on") |
    "inline"    <~ lookAhead(ws ~ ("def" | "val" | "if")) |
    "using"     <~ lookAhead(ws ~ identifier)
  }

  val spanParsers: Seq[CodeSpanParser] = Seq(
    comment,
    CharLiteral.standard.embed(charEscapes),
    symbol,
    backtickId,
    stringLiteral,
    JavaSyntax.annotation,
    keywords,
    Keywords("enum", "export", "given", "then"), // keywords added in Dotty/Scala3
    softKeywords,
    identifier,
    numberLiteral
  )

}
