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

package laika.parse.code.common

import laika.ast.{CodeSpan, CodeSpans, ~}
import laika.parse.code.{CodeCategory, CodeSpanParsers}
import laika.parse.text.TextParsers._

/**
  * @author Jens Halm
  */
object CharLiteral {
  
  case class CharParser(delim: Char,
                        embedded: Seq[CodeSpanParsers] = Nil) {
    
    val defaultCategories: Set[CodeCategory] = Set(CodeCategory.CharLiteral)

    def embed(childSpans: CodeSpanParsers*): CharParser = {
      copy(embedded = embedded ++ childSpans)
    }

    def build: CodeSpanParsers = CodeSpanParsers(delim) {

      val plainChar = lookBehind(1, anyBut('\'', '\n').take(1)).map(CodeSpan(_, defaultCategories))
      val closingDelim = anyOf(delim).take(1).map(CodeSpan(_, defaultCategories))
      
      any.take(1).flatMap { char =>
        (EmbeddedCodeSpans.parserMap(embedded).getOrElse(char.head, plainChar) ~ closingDelim).map { 
          case span ~ closingDel => 
            val codeSpans = CodeSpans.extract(defaultCategories)(span) :+ closingDel
            CodeSpans.merge(delim, codeSpans, defaultCategories) 
        }
      }

    }

  }
  
  def standard: CharParser = CharParser('\'')
  
}
