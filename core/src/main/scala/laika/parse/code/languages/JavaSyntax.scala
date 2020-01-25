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
import laika.parse.code.CodeSpanParsers
import laika.parse.code.common.{CharLiteral, Comment, Identifier, Keywords, NumberLiteral, NumericSuffix, StringLiteral}

/**
  * @author Jens Halm
  */
object JavaSyntax extends SyntaxHighlighter {

  val language: NonEmptyList[String] = NonEmptyList.of("java")

  val charEscapes: CodeSpanParsers = 
    StringLiteral.Escape.unicode ++
    StringLiteral.Escape.octal ++
    StringLiteral.Escape.char
  
  val spanParsers: Seq[CodeSpanParsers] = Seq(
    Comment.singleLine("//"),
    Comment.multiLine("/*", "*/"),
    CharLiteral.standard.embed(charEscapes),
    StringLiteral.singleLine('"').embed(charEscapes),
    Keywords(BooleanLiteral)("true", "false"),
    Keywords(LiteralValue)("null"),
    Keywords(TypeName)("boolean", "byte", "char", "double", "float", "int", "long", "short"),
    Keywords("abstract", "assert", "break", "case", "catch", "class", "const", "continue",
      "default", "do", "else", "enum", "extends", "finally", "final", "for", "if", 
      "implements", "import", "instanceof", "interface", "module", "native", "new", "package", "private", 
      "protected", "public", "requires", "return", "static", "strictfp", "super", "switch", "synchronized",
      "this", "throws", "throw", "transient", "try", "var", "void", "volatile", "while"),
    Identifier.standard.withIdStartChars('_','$').withCategoryChooser(Identifier.upperCaseTypeName),
    NumberLiteral.binary.withUnderscores.withSuffix(NumericSuffix.long),
    NumberLiteral.octal.withUnderscores.withSuffix(NumericSuffix.long),
    NumberLiteral.hexFloat.withUnderscores.withSuffix(NumericSuffix.float),
    NumberLiteral.hex.withUnderscores.withSuffix(NumericSuffix.long),
    NumberLiteral.decimalFloat.withUnderscores.withSuffix(NumericSuffix.float),
    NumberLiteral.decimalInt.withUnderscores.withSuffix(NumericSuffix.long | NumericSuffix.float),
  )
  
}
