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

package laika.parse.helper

import laika.ast.Block
import laika.parse.Parser
import laika.parse.combinator.Parsers._
import laika.parse.markup.DefaultRecursiveParsers

/** @author Jens Halm
  */
trait EmptyRecursiveParsers extends DefaultRecursiveParsers {

  override protected def rootBlock: Parser[Block] = failure("not implemented")

  override protected def nestedBlock: Parser[Block] = failure("not implemented")

  override protected def fallbackBlock: Parser[Block] = failure("not implemented")

}
