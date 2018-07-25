/*
 * Copyright 2013-2018 the original author or authors.
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

package laika.parse.markdown

import laika.rewrite.TreeUtil
import laika.tree.Elements.{Block, Header, Id}

/**
  * @author Jens Halm
  */
object HeaderIdInsertion extends (Seq[Block] => Seq[Block]) {

  def apply (blocks: Seq[Block]): Seq[Block] = blocks.map {
    case h: Header => h.copy(options = h.options + Id(h.extractText.replaceAll("[\n ]+", " ").toLowerCase))
    case other => other
  }

}
