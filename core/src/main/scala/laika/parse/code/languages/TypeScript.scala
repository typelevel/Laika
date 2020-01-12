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

import laika.bundle.SyntaxHighlighter
import laika.parse.code.CodeCategory.{BooleanLiteral, LiteralValue}
import laika.parse.code.common.{Comment, Identifier, Keywords, NumberLiteral, RegexLiteral, StringLiteral}

/**
  * @author Jens Halm
  */
object TypeScript {

  lazy val highlighter: SyntaxHighlighter = SyntaxHighlighter.build("typescript")(
    Comment.singleLine("//"),
    Comment.multiLine("/*", "*/"),
    StringLiteral.singleLine('"').embed(
      JavaScript.unicodeCodePointEscape,
      StringLiteral.Escape.unicode,
      StringLiteral.Escape.hex,
      StringLiteral.Escape.char,
    ).build,
    StringLiteral.singleLine('\'').embed(
      JavaScript.unicodeCodePointEscape,
      StringLiteral.Escape.unicode,
      StringLiteral.Escape.hex,
      StringLiteral.Escape.char,
    ).build,
    StringLiteral.multiLine("`").embed(
      JavaScript.unicodeCodePointEscape,
      StringLiteral.Escape.unicode,
      StringLiteral.Escape.hex,
      StringLiteral.Escape.char,
      StringLiteral.Substitution.between("${", "}"),
    ).build,
    RegexLiteral.standard,
    Keywords(BooleanLiteral)("true", "false"),
    Keywords(LiteralValue)("null", "undefined", "NaN", "Infinity"),
    Keywords("abstract", "class", "declare", "enum", "extends", "get", "implements", "namespace", 
      "public", "private", "protected", "set", "type"),
    JavaScript.keywords,
    Identifier.standard.withIdStartChars('_','$').build,
    JavaScript.number(NumberLiteral.binary),
    JavaScript.number(NumberLiteral.octal),
    JavaScript.number(NumberLiteral.hex),
    JavaScript.number(NumberLiteral.decimalFloat),
    JavaScript.number(NumberLiteral.decimalInt),
  )
  
}
