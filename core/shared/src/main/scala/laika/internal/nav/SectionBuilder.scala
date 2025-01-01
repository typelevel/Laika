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

package laika.internal.nav

import laika.ast.RewriteRules.RewriteRulesBuilder
import laika.ast.*
import laika.api.config.Config.ConfigResult
import laika.config.{ AutonumberConfig, LaikaKeys }
import laika.internal.collection.Stack

import scala.collection.mutable.ListBuffer

/** Rewrite rules responsible for building the section structure of a document based on the header elements it contains
  * and their level.
  *
  * @author Jens Halm
  */
private[laika] object SectionBuilder extends RewriteRulesBuilder {

  private class DefaultRule(
      autonumberConfig: AutonumberConfig,
      extractTitle: Boolean,
      position: TreePosition
  ) {

    def addNumber(spans: Seq[Span], position: TreePosition): Seq[Span] = position.toSpan +: spans

    class Builder(header: Header, val position: TreePosition) {

      val titleWithNumber: Seq[Span] =
        if (autonumberConfig.sections && position.depth <= autonumberConfig.maxDepth)
          addNumber(header.content, position)
        else header.content

      val styledHeader: Header = header.copy(
        content = titleWithNumber,
        options = header.options + Style.section
      )

      private val buffer = new ListBuffer[Block]

      def += (block: Block): Unit = buffer += block

      def >= (level: Int): Boolean = header.level >= level

      def toSection: Section = Section(styledHeader, buffer.toList)

    }

    def buildSections(document: RootElement): RootElement = {

      def flatten(blocks: Seq[Block]): Seq[Block] = blocks.flatMap {
        case seq: BlockSequence if seq.options.styles.isEmpty && !seq.hasId => flatten(seq.content)
        case b: Block                                                       => Seq(b)
      }

      val flattenedDocument = flatten(document.content)

      val docPosition = if (autonumberConfig.documents) position else TreePosition.root

      val (titleSection, rest) =
        if (!extractTitle) (Nil, flattenedDocument)
        else {

          val title = flattenedDocument.collectFirst { case h: Header =>
            if (autonumberConfig.documents)
              Title(addNumber(h.content, docPosition), h.options + Style.title)
            else Title(h.content, h.options + Style.title)
          }

          title.fold((flattenedDocument, Seq.empty[Block])) { titleBlock =>
            val (preface, rest) =
              flattenedDocument.splitAt(flattenedDocument.indexWhere(_.isInstanceOf[Header]))
            (preface :+ titleBlock, rest.tail)
          }
        }

      val sectionStructure: Seq[Block] = {

        val stack = new Stack[Builder]
        stack.push(new Builder(Header(0, Nil), docPosition))

        def closeSections(toLevel: Int): Int = {
          var nextPos = 1
          while (stack.nonEmpty && stack.top >= toLevel) {
            val section = stack.pop
            nextPos = section.position.toSeq.last + 1
            stack.top += section.toSection
          }
          nextPos
        }

        rest.foreach {
          case h @ Header(level, _, _) =>
            val nextPos = closeSections(level)
            stack.push(new Builder(h, stack.top.position.forChild(nextPos)))
          case block                   =>
            stack.top += block
        }

        closeSections(1)

        stack.pop.toSection.content
      }

      RootElement(titleSection ++ sectionStructure)
    }

    val rewrite: RewriteRules = RewriteRules.forBlocks { case root: RootElement =>
      RewriteAction.Replace(buildSections(root))
    }

  }

  /** Provides the default rewrite rules for building the section structure
    *  for the specified document (without applying them).
    */
  def apply(cursor: DocumentCursor): ConfigResult[RewriteRules] = for {
    autonumbering      <- cursor.config.getOpt[AutonumberConfig].map(
      _.getOrElse(AutonumberConfig.disabled)
    )
    firstHeaderAsTitle <- cursor.config.get(
      LaikaKeys.firstHeaderAsTitle,
      !cursor.config.get[Boolean](LaikaKeys.orphan).getOrElse(false)
    )
  } yield new DefaultRule(autonumbering, firstHeaderAsTitle, cursor.position).rewrite

}
