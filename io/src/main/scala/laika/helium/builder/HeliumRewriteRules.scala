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
private[helium] object HeliumRewriteRules {

  /** This is only a rough estimate as it counts *source* lines and not output lines.
    */
  def estimateLines (blocks: Seq[Block]): Int = blocks.collect {
    case s: Section => estimateLines(s.content) + 1
    case sp: SpanContainer  => sp.extractText.count(_ == '\n') + 1
    case bc: BlockContainer => estimateLines(bc.content)
    case ni: NavigationItem => 1 + estimateLines(ni.content)
    case lc: ListContainer  =>  lc.content.map {
      case bc: BlockContainer => estimateLines(bc.content)
      case ni: NavigationItem => 1 + estimateLines(ni.content)
      case _                  => 1
    }.sum
    case table: Table       => (table.head.content ++ table.body.content).map { row =>
      row.content.map(cell => estimateLines(cell.content)).max
    }.sum
  }.sum

  def build (pdfSettings: PDFSettings): RewriteRules = RewriteRules.forBlocks {
    case cb: CodeBlock if cb.extractText.count(_ == '\n') <= pdfSettings.layout.keepTogetherDecoratedLines =>
      Replace(cb.mergeOptions(Style.keepTogether))
    case bs: BlockSequence if bs.options.styles.contains("callout") && estimateLines(bs.content) <= pdfSettings.layout.keepTogetherDecoratedLines =>
      Replace(bs.mergeOptions(Style.keepTogether))
  }
  
}
