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
import laika.parse.code.{CodeCategory, CodeSpanParser}
import laika.parse.code.common.{CharLiteral, Comment, Identifier, Keywords, NumberLiteral, NumericSuffix, StringLiteral}
import laika.parse.text.{CharGroup, PrefixedParser}
import laika.parse.builders._
import laika.parse.implicits._
import laika.parse.code.common.Identifier.IdParser
import laika.parse.code.implicits._

/**
  * @author Jens Halm
  */
object ScalaSyntax extends SyntaxHighlighter {

  
  val language: NonEmptyList[String] = NonEmptyList.of("scala")
  
  val comment: CodeSpanParser = Comment.singleLine("//") ++ Comment.multiLine("/*", "*/")
  
  val symbol: CodeSpanParser = Identifier.alphaNum
    .withPrefix(literal("'"))
    .withCategory(CodeCategory.SymbolLiteral)
  
  val backtickId: CodeSpanParser = CodeSpanParser(CodeCategory.Identifier) {
    (oneOf('`') ~ anyNot('\n', '`') ~ oneOf('`')).source
  }
  
  val charEscapes: CodeSpanParser = StringLiteral.Escape.unicode ++ StringLiteral.Escape.char
  
  val stringPrefixChar: PrefixedParser[String] = someOf(CharGroup.alpha)

  val substitutions: CodeSpanParser = 
    StringLiteral.Substitution.between("${", "}") ++
    StringLiteral.Substitution(("$" ~ someOf(CharGroup.alphaNum.add('_'))).source)
  
  val stringLiteral: CodeSpanParser = 
    StringLiteral.multiLine("\"\"\"") ++
    StringLiteral.multiLine((stringPrefixChar ~ "\"\"\"").source, literal("\"\"\"")).embed(substitutions) ++
    StringLiteral.singleLine('"').embed(charEscapes) ++
    StringLiteral.singleLine((stringPrefixChar ~ "\"").source, literal("\"")).embed(
      charEscapes,
      substitutions
    )
  
  val numberLiteral: CodeSpanParser = 
    NumberLiteral.hex.withUnderscores.withSuffix(NumericSuffix.long) ++
    NumberLiteral.decimalFloat.withUnderscores.withSuffix(NumericSuffix.float) ++
    NumberLiteral.decimalInt.withUnderscores.withSuffix(NumericSuffix.long | NumericSuffix.float)
  
  /** Keywords for both Scala2 and Dotty/Scala3
    * Keywords for Scala 3: https://dotty.epfl.ch/docs/internals/syntax.html#regular-keywords
    */
  val keywords: CodeSpanParser = 
    Keywords(BooleanLiteral)("true", "false") ++
    Keywords(LiteralValue)("null") ++
    Keywords("abstract", "case", "catch", "class", "def", "do", "else", "enum", "export", "extends",
      "finally", "final", "for", "given", "if", "implicit", "import", "lazy", "match",
      "new", "object", "override", "package", "private", "protected", "return", "sealed", "super",
      "then", "this", "throw", "trait", "try", "type", "yield", "val", "var", "while", "with")
  
  val identifier: IdParser = Identifier.alphaNum
    .withIdStartChars('_','$')
    .withCategoryChooser(Identifier.upperCaseTypeName)
  
  val declaration: CodeSpanParser = CodeSpanParser {
    val keyword = literal("def").asCode(CodeCategory.Keyword)
    val name = identifier.withCategory(CodeCategory.DeclarationName)
    (keyword ~ ws.asCode() ~ name).mapN { Seq(_,_,_) }
  }
  
  val spanParsers: Seq[CodeSpanParser] = Seq(
    comment,
    CharLiteral.standard.embed(charEscapes),
    symbol,
    backtickId,
    stringLiteral,
    JavaSyntax.annotation,
    declaration,
    keywords,
    Keywords("break", "continue", "default", "forSome", "throws"), // keywords removed in Dotty/Scala3
    identifier,
    numberLiteral
  )
  
}
