/*
 * Copyright 2013-2017 the original author or authors.
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

package laika.parse.core.markup

import laika.parse.core.Parser
import laika.tree.Elements.{Block, Span}

/**
  * @author Jens Halm
  */
trait RecursiveParsers extends RecursiveBlockParsers with RecursiveSpanParsers

trait RecursiveBlockParsers {

  def recursiveBlocks (p: Parser[String]): Parser[Seq[Block]]

}

trait RecursiveSpanParsers {

  def recursiveSpans (p: Parser[String]): Parser[Seq[Span]]

}
