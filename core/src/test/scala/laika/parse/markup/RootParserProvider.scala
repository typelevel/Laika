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

package laika.parse.markup

import laika.bundle.{BlockParserBuilder, MarkupExtensions, ParserBundle, SpanParserBuilder}
import laika.factory.MarkupFormat

/**
  * @author Jens Halm
  */
object RootParserProvider {

  def forParsers (blockParsers: Seq[BlockParserBuilder] = Nil,
                  spanParsers: Seq[SpanParserBuilder] = Nil,
                  markupExtensions: MarkupExtensions = ParserBundle().markupExtensions): RootParser = {

    val bp = blockParsers
    val sp = spanParsers

    object Parser extends MarkupFormat {
      val fileSuffixes = Set.empty[String]
      val blockParsers = bp
      val spanParsers = sp
      val extensions = Seq()
    }

    new RootParser(Parser, markupExtensions)

  }

}
