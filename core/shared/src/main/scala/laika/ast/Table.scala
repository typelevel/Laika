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

package laika.ast

/** A table consisting of a head and a body part and optional caption and column specification.
  */
case class Table (head: TableHead, body: TableBody, caption: Caption = Caption(), columns: Columns = Columns(Nil), options: Options = NoOpt) extends Block
  with ElementTraversal with RewritableContainer {
  type Self = Table

  def rewriteChildren (rules: RewriteRules): Table = copy(head = head.rewriteChildren(rules), body = body.rewriteChildren(rules), caption = caption.rewriteChildren(rules))

  override def toString: String = FormattedElementString.render(this)

  def withOptions (options: Options): Table = copy(options = options)
}

object Table {

  /** Creates a table without header rows, all specified rows will 
    * be treated as rows of the table body.
    */
  def apply(row: Row, rows: Row*): Table = Table(TableHead(Nil), TableBody(row +: rows.toList))
}

/** A table element, like a row, cell or column.
  */
trait TableElement extends Element { type Self <: TableElement }

/** A container of table elements.
  */
trait TableContainer extends TableElement with ElementContainer[TableElement]  { type Self <: TableContainer }

/** Contains the header rows of a table.
  */
case class TableHead (content: Seq[Row], options: Options = NoOpt) extends TableElement with TableContainer with RewritableContainer {
  type Self = TableHead
  def rewriteChildren (rules: RewriteRules): TableHead = copy(content = content.map(_.rewriteChildren(rules)))
  def withOptions (options: Options): TableHead = copy(options = options)
}

/** Contains the body rows of a table.
  */
case class TableBody (content: Seq[Row], options: Options = NoOpt) extends TableElement with TableContainer with RewritableContainer {
  type Self = TableBody
  def rewriteChildren (rules: RewriteRules): TableBody = copy(content = content.map(_.rewriteChildren(rules)))
  def withOptions (options: Options): TableBody = copy(options = options)
}

/** The table caption.
  */
case class Caption (content: Seq[Span] = Nil, options: Options = NoOpt) extends TableElement with SpanContainer {
  type Self = Caption
  def withContent (newContent: Seq[Span]): Caption = copy(content = newContent)
  def withOptions (options: Options): Caption = copy(options = options)
}
object Caption extends SpanContainerCompanion {
  type ContainerType = Caption
  protected def createSpanContainer (spans: Seq[Span]): Caption = Caption(spans)
}

/** Contains the (optional) column specification of a table.
  */
case class Columns (content: Seq[Column], options: Options = NoOpt) extends TableElement with TableContainer {
  type Self = Columns
  def withOptions (options: Options): Columns = copy(options = options)
}

/** Convenient factory for creating a `Columns` instance based on the options
  *  for the individual columns.
  */
object Columns {
  def options (options: Options*): Columns = Columns(options.map(Column.apply))
}

/** The options (like styles) for a column table.
  */
case class Column (options: Options = NoOpt) extends TableElement {
  type Self = Column
  def withOptions (options: Options): Column = copy(options = options)
}

/** A single table row. In case some of the previous rows contain
  *  cells with a colspan greater than 1, this row may contain
  *  fewer cells than the number of columns in the table.
  */
case class Row (content: Seq[Cell], options: Options = NoOpt) extends TableElement with TableContainer with RewritableContainer {
  type Self = Row
  def rewriteChildren (rules: RewriteRules): Row = copy(content = content.map(_.rewriteChildren(rules)))
  def withOptions (options: Options): Row = copy(options = options)
}
object Row {
  def apply(cell: Cell, cells: Cell*): Row = Row(cell +: cells.toList)
}

/** A single cell, potentially spanning multiple rows or columns, containing
  *  one or more block elements.
  */
case class Cell (cellType: CellType, content: Seq[Block], colspan: Int = 1, rowspan: Int = 1, options: Options = NoOpt) extends TableElement
  with BlockContainer {
  type Self = Cell
  def withContent (newContent: Seq[Block]): Cell = copy(content = newContent)
  def withOptions (options: Options): Cell = copy(options = options)
}

/** The cell type specifies which part of the table the cell belongs to.
  */
sealed abstract class CellType

/** A cell in the head part of the table.
  */
case object HeadCell extends CellType with BlockContainerCompanion {
  type ContainerType = Cell
  def apply(blocks: Seq[Block]): Cell = Cell(this, blocks)
  protected def createBlockContainer (blocks: Seq[Block]) = Cell(this, blocks)
}

/** A cell in the body part of the table.
  */
case object BodyCell extends CellType with BlockContainerCompanion {
  type ContainerType = Cell
  def apply(blocks: Seq[Block]): Cell = Cell(this, blocks)
  protected def createBlockContainer (blocks: Seq[Block]) = Cell(this, blocks)
}
