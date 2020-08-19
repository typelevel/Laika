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

package laika.helium.builder

import laika.ast._
import laika.helium.config.PDFSettings

/**
  * @author Jens Halm
  */
private[laika] object HeliumRewriteRules {

  private def estimateLines (blocks: Seq[Block]): Int = blocks.collect {
    case sp: SpanContainer => sp.extractText.count(_ == '\n')
    case bc: BlockContainer => estimateLines(bc.content) // TODO - handle lists and tables
  }.sum

  def build (pdfSettings: PDFSettings): Seq[DocumentCursor => RewriteRules] = Seq(_ => RewriteRules.forBlocks {
    case cb: CodeBlock if cb.extractText.count(_ == '\n') <= pdfSettings.pdfLayout.keepTogetherDecoratedLines =>
      Replace(cb.mergeOptions(Style.keepTogether))
    case bs: BlockSequence if bs.options.styles.contains("callout") && estimateLines(bs.content) <= pdfSettings.pdfLayout.keepTogetherDecoratedLines =>
      Replace(bs.mergeOptions(Style.keepTogether))
  })
  
}
