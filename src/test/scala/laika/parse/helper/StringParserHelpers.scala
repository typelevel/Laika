/*
 * Copyright 2013 the original author or authors.
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

package laika.parse.helper

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.CharSequenceReader

trait StringParserHelpers { self: Parsers =>
  
  override type Elem = Char

  class Source (value: String) {
    def using [T] (parser: Parser[T]) = parser(new CharSequenceReader(value)) 
  }
  
  object Parsing {
  	def apply (source: String) = new Source(source)
  }
  
}