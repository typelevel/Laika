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

package laika.markdown.github

import laika.ast._
import laika.bundle.{BlockParser, BlockParserBuilder}
import laika.parse.Parser
import laika.parse.text.TextParsers._

/** Parser for the table extension of GitHub Flavored Markdown.
  *
  * For the spec see [[https://github.github.com/gfm/#table]]
  *
  * @author Jens Halm
  */
object Table {

  val parser: BlockParserBuilder = BlockParser.forStartChar('|').withSpans { spanParsers =>

    val cell: Parser[Cell] = spanParsers.recursiveSpans(anyBut('|','\n').map(_.trim)) ^^ { spans =>
      Cell(HeadCell, Seq(Paragraph(spans)))
    }

    val finalCell: Parser[Cell] = spanParsers.recursiveSpans(textLine.map(_.trim)) ^^ { spans =>
      Cell(HeadCell, Seq(Paragraph(spans)))
    }

    val row: Parser[Row] = (cell <~ '|').rep ~ (finalCell.map(Some(_)) | restOfLine ^^^ None) ^^ {
      case cells ~ optFinal => Row(cells ++ optFinal.toSeq)
    }

    val separator: Parser[Unit] = ws ~> anyOf('-').min(1).^ <~ ws

    val sepRow: Parser[Int] = opt('|') ~> (separator ~ '|').rep.map(_.size) ~ opt(separator).map(_.fold(0)(_ => 1)) ^^ {
      case sep ~ finalSep => sep + finalSep
    }

    val header: Parser[Row] = row ~ sepRow ^? {
      case row ~ sepRow if row.content.size == sepRow => row
    }

    header ^^ { row => laika.ast.Table(TableHead(Seq(row)), TableBody(Nil)) }
  }

}
