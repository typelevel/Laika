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

package laika.parse

import laika.parse.markup.{ BlockParsers, InlineParsers }
import laika.parse.text.TextParsers

/** Grouping of all parser builders that allows to construct most common parsers with a single import.
  *
  * These include the base parsers like `opt` and `not`, the text parsers like `literal` and `anyOf(Char*)`,
  * as well as the more specialized parsers for text markup like `spans` and `blocks`.
  *
  * Alternatively these groups can be brought into scope individually.
  *
  * @author Jens Halm
  */
object builders extends TextParsers with InlineParsers with BlockParsers
