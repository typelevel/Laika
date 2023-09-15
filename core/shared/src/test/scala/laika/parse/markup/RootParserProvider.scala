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

package laika.parse.markup

import laika.api.format.MarkupFormat
import MarkupFormat.MarkupParsers
import laika.api.bundle.{ BlockParserBuilder, MarkupExtensions, ParserBundle, SpanParserBuilder }

/** @author Jens Halm
  */
object RootParserProvider {

  def forParsers(
      blockParsers: Seq[BlockParserBuilder] = Nil,
      spanParsers: Seq[SpanParserBuilder] = Nil,
      markupExtensions: MarkupExtensions = new ParserBundle().markupExtensions
  ): RootParserWrapper = {

    val bp = blockParsers
    val sp = spanParsers

    object Parser extends MarkupFormat {
      val fileSuffixes = Set.empty[String]
      val blockParsers = new MarkupParsers[BlockParserBuilder] {
        val all: Seq[BlockParserBuilder] = bp
      }
      val spanParsers  = new MarkupParsers[SpanParserBuilder] {
        val all: Seq[SpanParserBuilder] = sp
      }
      val extensions   = Seq()
    }

    new RootParserWrapper(Parser, markupExtensions)

  }

  class RootParserWrapper(markupParser: MarkupFormat, markupExtensions: MarkupExtensions)
      extends RootParser(markupParser, markupExtensions) {
    lazy val standaloneSpanParser = defaultSpanParser
  }

}
