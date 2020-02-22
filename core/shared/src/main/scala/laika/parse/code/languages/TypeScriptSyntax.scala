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
import laika.parse.code.CodeCategory.{BooleanLiteral, LiteralValue, TypeName}
import laika.parse.code.CodeSpanParser
import laika.parse.code.common._

/**
  * @author Jens Halm
  */
object TypeScriptSyntax extends SyntaxHighlighter {
  
  val stringEmbeds: CodeSpanParser = 
    JavaScriptSyntax.unicodeCodePointEscape ++
    StringLiteral.Escape.unicode ++
    StringLiteral.Escape.hex ++
    StringLiteral.Escape.char

  val language: NonEmptyList[String] = NonEmptyList.of("typescript")

  val spanParsers: Seq[CodeSpanParser] = Seq(
    Comment.singleLine("//"),
    Comment.multiLine("/*", "*/"),
    StringLiteral.singleLine('"').embed(stringEmbeds),
    StringLiteral.singleLine('\'').embed(stringEmbeds),
    StringLiteral.multiLine("`").embed(stringEmbeds),
    RegexLiteral.standard,
    Keywords(BooleanLiteral)("true", "false"),
    Keywords(LiteralValue)("null", "undefined", "NaN", "Infinity"),
    Keywords("abstract", "declare", "enum", "get", "implements", "interface", "namespace", 
      "package", "public", "private", "protected", "set", "type"),
    JavaScriptSyntax.keywords,
    Keywords(TypeName)("any", "number", "boolean", "string", "symbol", "void"),
    Identifier.alphaNum.withIdStartChars('_','$'),
    JavaScriptSyntax.number(NumberLiteral.binary),
    JavaScriptSyntax.number(NumberLiteral.octal),
    JavaScriptSyntax.number(NumberLiteral.hex),
    JavaScriptSyntax.number(NumberLiteral.decimalFloat),
    JavaScriptSyntax.number(NumberLiteral.decimalInt),
  )

  object TSX extends SyntaxHighlighter {

    val language: NonEmptyList[String] = NonEmptyList.of("tsx")
    
    lazy val spanParsers: Seq[CodeSpanParser] = TypeScriptSyntax.spanParsers ++ Seq(
      JavaScriptSyntax.JSX.element,
      JavaScriptSyntax.JSX.emptyJsxTag
    )

  }
  
}
