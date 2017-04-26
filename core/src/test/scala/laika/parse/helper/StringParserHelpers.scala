/*
 * Copyright 2013-2016 the original author or authors.
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

import laika.parse.core.{CharSequenceReader, ParseResult, Parser, Parsers}

trait StringParserHelpers { self: Parsers =>
  
  class Source (value: String) {
    def using [T] (parser: Parser[T]): ParseResult[T] = parser(new CharSequenceReader(value)) 
  }
  
  object Parsing {
    def apply (source: String): Source = new Source(source)
  }
  
}
