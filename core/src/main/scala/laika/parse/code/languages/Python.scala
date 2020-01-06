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
import laika.parse.code.common.{Comment, Keywords}

/**
  * @author Jens Halm
  */
object Python {

  lazy val highlighter: SyntaxHighlighter = SyntaxHighlighter.build("python", "py")(Seq(
    Comment.singleLine("#")
  ) ++
    Keywords(BooleanLiteral)("True", "False") ++
    Keywords(LiteralValue)("None") ++
    Keywords("and", "assert", "async", "as", "await", "break", "class", "continue", "def", "del", "elif", "else", 
      "except", "exec", "finally", "for", "from", "global", "if", "import", "in", "is", "lambda", 
      "nonlocal", "not", "or", "pass", "print", "raise", "return", "try", "with", "while", "yield")
  )
  
}
