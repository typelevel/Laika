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
import laika.parse.code.CodeCategory.{BooleanLiteral, LiteralValue, TypeName}
import laika.parse.code.common.{Comment, Keywords, NumberLiteral, NumericSuffix, StringLiteral}

/**
  * @author Jens Halm
  */
object Java {

  lazy val highlighter: SyntaxHighlighter = SyntaxHighlighter.build("java")(
    Comment.singleLine("//"),
    Comment.multiLine("/*", "*/"),
    StringLiteral.singleLine('"').embed(
      StringLiteral.Escape.unicode,
      StringLiteral.Escape.octal,
      StringLiteral.Escape.char,
    ).build,
    Keywords(BooleanLiteral)("true", "false"),
    Keywords(LiteralValue)("null"),
    Keywords(TypeName)("boolean", "byte", "char", "double", "float", "int", "long", "short"),
    Keywords("abstract", "assert", "break", "case", "catch", "const", "continue",
      "default", "do", "else", "enum", "exports", "finally", "final", "for", "if", 
      "import", "instanceof", "module", "native", "package", "private", "protected", 
      "public", "requires", "static", "strictfp", "super", "switch", "synchronized",
      "this", "throws","transient", "try", "var", "void", "volatile", "while"),
    NumberLiteral.binary.withUnderscores.withSuffix(NumericSuffix.long).build,
    NumberLiteral.octal.withUnderscores.withSuffix(NumericSuffix.long).build,
    NumberLiteral.hexFloat.withUnderscores.withSuffix(NumericSuffix.float).build,
    NumberLiteral.hex.withUnderscores.withSuffix(NumericSuffix.long).build,
    NumberLiteral.decimalFloat.withUnderscores.withSuffix(NumericSuffix.float).build,
    NumberLiteral.decimalInt.withUnderscores.withSuffix(NumericSuffix.long | NumericSuffix.float).build,
  )
  
}
