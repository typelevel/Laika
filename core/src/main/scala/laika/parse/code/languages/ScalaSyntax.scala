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
import laika.parse.code.CodeCategory.{BooleanLiteral, LiteralValue}
import laika.parse.code.{CodeCategory, CodeSpanParsers}
import laika.parse.code.common.{CharLiteral, Comment, Identifier, Keywords, NumberLiteral, NumericSuffix, StringLiteral}
import laika.parse.text.Characters
import laika.parse.text.TextParsers._

/**
  * @author Jens Halm
  */
object ScalaSyntax extends SyntaxHighlighter {

  import NumberLiteral._
  
  val language: NonEmptyList[String] = NonEmptyList.of("scala")
  
  private val interpolatedStartChars: Set[Char] = ('a' to 'z').toSet ++ ('A' to 'Z').toSet
  
  val symbolParser: CodeSpanParsers = CodeSpanParsers('\'') {
    Identifier.alphaNum
      .withCategoryChooser(_ => CodeCategory.SymbolLiteral)
      .standaloneParser
      .map(Seq(_))
  }
  
  val backtickIdParser: CodeSpanParsers = CodeSpanParsers(CodeCategory.Identifier, '`') {
    (anyBut('\n', '`') ~ anyOf('`').take(1)).concat
  }
  
  val charEscapes: CodeSpanParsers = StringLiteral.Escape.unicode ++ StringLiteral.Escape.char
  
  val stringPrefixChar: Characters[String] = anyIn('a' to 'z', 'A' to 'Z')

  val substitutions: CodeSpanParsers = 
    StringLiteral.Substitution.between("${", "}") ++
    StringLiteral.Substitution('$')(anyIn('a' to 'z', 'A' to 'Z', '0' to '9', '_').min(1))
  
  val spanParsers: Seq[CodeSpanParsers] = Seq(
    Comment.singleLine("//"),
    Comment.multiLine("/*", "*/"),
    CharLiteral.standard.embed(charEscapes),
    symbolParser,
    backtickIdParser,
    StringLiteral.multiLine("\"\"\""),
    StringLiteral.multiLine(interpolatedStartChars, "\"\"\"").withPrefix((stringPrefixChar ~ "\"\"\"").concat).embed(
      substitutions
    ),
    StringLiteral.singleLine('"').embed(charEscapes),
    StringLiteral.singleLine(interpolatedStartChars, '\"').withPrefix((stringPrefixChar ~ "\"").concat).embed(
      charEscapes,
      substitutions
    ),
    Keywords(BooleanLiteral)("true", "false"),
    Keywords(LiteralValue)("null"),
    Keywords("abstract", "break", "case", "catch", "class", "continue", "default", "def", "else", "extends",
      "finally", "final", "forSome", "for", "if", "implicit", "import", "lazy", "match",
      "new", "object", "override", "package", "private", "protected", "return", "sealed", "super",
      "this", "throw", "throws", "trait", "try", "type", "yield", "val", "var", "while", "with"),
    Identifier.alphaNum.withIdStartChars('_','$').withCategoryChooser(Identifier.upperCaseTypeName),
    NumberLiteral.hex.withUnderscores.withSuffix(NumericSuffix.long),
    NumberLiteral.decimalFloat.withUnderscores.withSuffix(NumericSuffix.float),
    NumberLiteral.decimalInt.withUnderscores.withSuffix(NumericSuffix.long | NumericSuffix.float),
  )
  
}
