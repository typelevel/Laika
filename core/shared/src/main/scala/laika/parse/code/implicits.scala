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

package laika.parse.code

import laika.ast.CodeSpan
import laika.parse.Parser
import laika.parse.text.PrefixedParser

/**
  * @author Jens Halm
  */
object implicits {

  implicit class CodeParserOps[T] (val p: Parser[String]) extends AnyVal {
    def asCode(categories: CodeCategory*): Parser[CodeSpan] = p.map(CodeSpan(_, categories.toSet))
    def asCode(categories: Set[CodeCategory]): Parser[CodeSpan] = p.map(CodeSpan(_, categories))
  }

  implicit class CodeStringParserOps[T] (val p: PrefixedParser[String]) extends AnyVal {
    def asCode(categories: CodeCategory*): PrefixedParser[CodeSpan] = p.map(CodeSpan(_, categories.toSet))
    def asCode(categories: Set[CodeCategory]): PrefixedParser[CodeSpan] = p.map(CodeSpan(_, categories))
  }
  
}
