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

package laika.rst

import cats.data.NonEmptyChain
import laika.ast._
import laika.bundle.BlockParserBuilder
import laika.collection.Stack
import laika.collection.TransitionalCollectionOps.Zip3Iterator
import laika.parse.builders._
import laika.parse.implicits._
import laika.parse._
import laika.parse.markup.RecursiveParsers

import scala.annotation.nowarn
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Provides parsers for the two table types supported by reStructuredText.
  *
  * @author Jens Halm
  */
object TableParsers {

  private abstract class TableElement

  private abstract class TableDecoration extends TableElement

  private case object Intersection extends TableDecoration {
    override def toString = "+"
  }

  private case object RowSeparator  extends TableDecoration
  private case object TableBoundary extends TableDecoration

  private case class CellSeparator(decoration: String) extends TableDecoration {
    override def toString = decoration
  }

  private case class CellElement(text: LineSource) extends TableElement {
    override def toString = text.input
  }

  private class CellBuilder(recParser: RecursiveParsers) {

    private val seps                                 = new ListBuffer[TableElement]
    private val previousLines                        = new ListBuffer[LineSource]
    private var currentLine: Option[LineSource]      = None
    private def allLines: mutable.Buffer[LineSource] = previousLines ++ currentLine.toBuffer

    var rowSpan = 1
    var colSpan = 1

    var removed: Boolean = false

    def nextLine(sep: TableElement, line: LineSource, nextRow: Boolean): Unit = {
      seps += sep
      currentLine.foreach(previousLines += _)
      currentLine = Some(line)
      if (nextRow) rowSpan += 1
    }

    def currentLine(sep: TableElement, line: LineSource): Unit = {
      currentLine.foreach { current =>
        currentLine = Some(LineSource(current.input + sep.toString + line.input, current.parent))
      }
    }

    def merge(rightBuilder: CellBuilder): Unit = if (currentLine.isDefined) {
      val newLines = Zip3Iterator(allLines, rightBuilder.seps, rightBuilder.allLines).map {
        case (left, sep, right) =>
          LineSource(left.input + sep.toString + right.input, left.parent)
      }.toSeq
      previousLines.clear()
      previousLines ++= newLines.tail
      currentLine = newLines.headOption
      colSpan += 1
    }

    @nowarn("cat=deprecation")
    def trimmedCellContent: Option[BlockSource] = {
      NonEmptyChain.fromSeq(allLines.toSeq).map { nonEmptyLines =>
        val minIndent    = nonEmptyLines.map { line =>
          if (line.input.trim.isEmpty) Int.MaxValue
          else line.input.prefixLength(_ == ' ')
        }.iterator.min
        val trimmedLines = nonEmptyLines.map { line =>
          if (line.input.trim.isEmpty) LineSource("", line.parent)
          else {
            val padding = " " * (line.input.prefixLength(_ == ' ') - minIndent)
            LineSource(padding + line.input.trim, line.parent.consume(minIndent))
          }
        }
        BlockSource(trimmedLines)
      }
    }

    def parsedCellContent: Seq[Block] = trimmedCellContent.fold[Seq[Block]](Nil)(src =>
      recParser.recursiveBlocks.parse(src).getOrElse(Nil)
    )

    def toCell(ct: CellType): Cell = Cell(ct, parsedCellContent, colSpan, rowSpan)
  }

  private class CellBuilderRef(val cell: CellBuilder, val mergedLeft: Boolean = false)

  private class RowBuilder {
    private val cells = new ListBuffer[CellBuilder]

    def addCell(cell: CellBuilder): Unit = cells += cell

    def toRow(ct: CellType): Row = Row(cells.filterNot(_.removed).map(_.toCell(ct)).toList)
  }

  private class ColumnBuilder(left: Option[ColumnBuilder], recParser: RecursiveParsers) {

    private var rowSpan = 1 // only used for sanity checks

    private val cells = new Stack[CellBuilderRef]

    def currentCell: CellBuilder = cells.top.cell

    def previousCell: CellBuilder = cells.elements(1).cell

    def nextCell: CellBuilder = {
      if (cells.nonEmpty && cells.top.mergedLeft && rowspanDif != 0)
        throw new MalformedTableException("Illegal merging of rows with different cellspans")
      val cell = new CellBuilder(recParser)
      cells push new CellBuilderRef(cell)
      cell
    }

    private def removeCell: CellBuilder = {
      val cell = cells.pop.cell
      cell.removed = true
      cell
    }

    def mergeLeft(previous: Boolean = false): Unit = {
      if (rowspanDif != 0)
        throw new MalformedTableException("Illegal merging of cells with different rowspans")
      val leftCell = if (previous) left.get.previousCell else left.get.currentCell
      leftCell.merge(removeCell)
      cells push new CellBuilderRef(leftCell, true)
    }

    def rowspanDif: Int = left.get.rowSpan - rowSpan

    def addLine(sep: TableElement, line: LineSource, nextRow: Boolean): Unit = {
      val ref = cells.top
      if (ref.mergedLeft) {
        if (nextRow && rowspanDif != 1)
          throw new MalformedTableException("Illegal merging of rows with different cellspans")
        ref.cell.currentLine(sep, line)
      }
      else {
        ref.cell.nextLine(sep, line, nextRow)
        sep match {
          case CellElement(_) => mergeLeft()
          case _              => ()
        }
      }
      if (nextRow) rowSpan += 1
    }

  }

  private class TableBuilder(columnWidths: List[Int], recParser: RecursiveParsers) {

    private object ColumnFactory {
      var lastColumn: Option[ColumnBuilder] = None
      val columnWidthIt                     = columnWidths.iterator
      def next = { lastColumn = Some(new ColumnBuilder(lastColumn, recParser)); lastColumn.get }
    }

    val columns: List[ColumnBuilder] = List.fill(columnWidths.length)(ColumnFactory.next)
    private val rows                 = new ListBuffer[RowBuilder]

    private def init(): Unit = {
      val row = nextRow
      columns.foreach(col => row.addCell(col.nextCell))
    }

    init()

    def nextRow: RowBuilder = {
      val row = new RowBuilder
      rows += row
      row
    }

    def toRowList(ct: CellType): List[Row] = rows.map(_.toRow(ct)).toList
  }

  private def flattenElements(result: Any): List[TableElement] = result match {
    case x: TableElement => List(x)
    case x ~ y           => flattenElements(x) ::: flattenElements(y)
    case _               => Nil
  }

  /** Parses a grid table.
    *
    *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#grid-tables]].
    */
  lazy val gridTable: BlockParserBuilder = BlockParserBuilder.recursive { recParsers =>
    val intersectChar = '+'
    val intersect     = oneOf(intersectChar).as(Intersection)

    val rowSep    = someOf('-').count
    val topBorder = intersect ~> (rowSep <~ intersect).rep.min(1) <~ wsEol

    val colSep       = oneOf('|').as(CellSeparator("|")) | intersect
    val colSepOrText = colSep | oneChar.line.map(CellElement.apply)

    topBorder >> { cols =>
      val separators  = colSep :: List.fill(cols.length - 1)(colSepOrText)
      val colsWithSep = Zip3Iterator(separators, cols, separators.reverse)

      def rowSep(width: Int): Parser[Any] =
        intersect ~ anyOf('-').take(width).as(RowSeparator) <~ nextIn(intersectChar)

      def boundaryPart(width: Int): Parser[Any] =
        intersect ~ anyOf('=').take(width).as(TableBoundary) <~ nextIn(intersectChar)

      def cell(sepL: Parser[Any], width: Int, sepR: Parser[Any]): Parser[Any] =
        sepL ~ anyChars.take(width).line.map(CellElement.apply) <~ lookAhead(sepR)

      val row = colsWithSep.map { case (separatorL, colWidth, separatorR) =>
        rowSep(colWidth) | cell(separatorL, colWidth, separatorR)
      }
        .reduceRight(_ ~ _)
        .map(flattenElements)

      val tableBoundary: Parser[TableDecoration] =
        cols.map(boundaryPart).reduceRight(_ ~ _).as(TableBoundary)

      def isSeparatorRow(row: List[TableElement]): Boolean = {
        row.forall {
          case RowSeparator => true
          case Intersection => true
          case _            => false
        }
      }

      def buildRowList(rows: List[List[TableElement]], ct: CellType): List[Row] = {

        val tableBuilder =
          new TableBuilder(cols map (_ + 1), recParsers) // column width includes separator

        rows foreach { row =>
          val hasSeparator  = row exists { case RowSeparator => true; case _ => false }
          val newRowBuilder = if (hasSeparator) Some(tableBuilder.nextRow) else None

          row.sliding(2, 2).zip(tableBuilder.columns.iterator).foreach {
            case (_ :: RowSeparator :: Nil, column) => newRowBuilder.get.addCell(column.nextCell)
            case (sep :: CellElement(text) :: Nil, column) =>
              column.addLine(sep, text, hasSeparator)
            case _ => () // cannot happen, just to avoid the warning
          }
        }
        tableBuilder.toRowList(ct)
      }

      def validateLastRow(rows: List[List[TableElement]]): Unit = {
        if (rows.isEmpty || !isSeparatorRow(rows.last))
          throw new MalformedTableException("Table not terminated correctly")
      }

      val boundaryRow = tableBoundary <~ oneChar ~ wsEol
      val tablePart   = (not(tableBoundary) ~> row <~ oneChar ~ wsEol).rep
      (tablePart ~ opt(boundaryRow ~> tablePart)).evalMap { result =>
        /* Need to fail for certain illegal constructs in the interim model,
         * so that the next parser can pick up the (broken) table input */
        try {
          val table = result match {
            case head ~ Some(body) =>
              validateLastRow(body);
              Table(
                TableHead(buildRowList(head, HeadCell)),
                TableBody(buildRowList(body.init, BodyCell))
              )
            case body ~ None       =>
              validateLastRow(body);
              Table(TableHead(Nil), TableBody(buildRowList(body.init, BodyCell)))
          }
          Right(table)
        }
        catch {
          case ex: MalformedTableException => Left(ex.getMessage)
        }
      }
    }

  }

  /** Parses a simple table.
    *
    *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#simple-tables]].
    */
  lazy val simpleTable: BlockParserBuilder = BlockParserBuilder.recursive { recParsers =>
    val intersect   = someOf(' ').count
    val tableBorder = someOf('=').count
    val columnSpec  = tableBorder ~ opt(intersect) ^^ {
      case col ~ Some(sep) => (col, sep)
      case col ~ None      => (col, 0)
    }
    val topBorder   = columnSpec.rep.min(2) <~ wsEol

    topBorder >> { cols =>
      val (rowColumns, boundaryColumns): (Seq[Parser[Any]], Seq[Parser[Any]]) =
        (cols map { case (col, sep) =>
          val cellText   =
            if (sep == 0) anyNot('\n', '\r').line.map(CellElement.apply)
            else anyChars.take(col).line.map(CellElement.apply)
          val separator  = anyOf(' ').take(sep).map(CellSeparator.apply)
          val textInSep  = anyChars.take(sep).map(CellSeparator.apply)
          val textColumn = cellText ~ (separator | textInSep)

          val rowSep    = anyOf('-').take(col).as(RowSeparator)
          val merged    = anyOf('-').take(sep).as(RowSeparator)
          val split     = anyOf(' ').take(sep).as(Intersection)
          val underline = rowSep ~ (split | merged)

          val bCell    = anyOf('=').take(col).as(TableBoundary)
          val bMerged  = anyOf('=').take(sep).as(TableBoundary)
          val bSplit   = anyOf(' ').take(sep).as(Intersection)
          val boundary = bCell ~ (bSplit | bMerged)

          (underline | not(boundary) ~> textColumn, boundary)
        }).unzip

      val row: Parser[Any]      = (rowColumns reduceRight (_ ~ _)) <~ wsEol
      val boundary: Parser[Any] = (boundaryColumns reduceRight (_ ~ _)) <~ wsEol
      val blank: Parser[Any]    = not(eof) ~> blankLine

      val tablePart: Parser[List[Any]] = ((blank | row).rep ~ boundary).map {
        case rows ~ boundary => rows :+ boundary
      }

      def buildRowList(rows: List[Any], ct: CellType): List[Row] = {

        val tableBuilder = new TableBuilder(cols map { col => col._1 + col._2 }, recParsers)

        def addBlankLines(acc: ListBuffer[List[TableElement]], parentSource: SourceCursor): Unit =
          acc += cols.flatMap { case (cell, sep) =>
            List(CellElement(LineSource(" " * cell, parentSource)), CellSeparator(" " * sep))
          }

        def addRowSeparators(acc: ListBuffer[List[TableElement]]): Unit =
          acc += (cols flatMap { _ => List(RowSeparator, Intersection) })

        /* in contrast to the grid table, some rows need to be processed in context,
         * as their exact behaviour depends on preceding or following lines. */
        val rowBuffer = rows.foldLeft((ListBuffer[List[TableElement]](), 0, false)) {
          case ((acc, blanks, rowOpen), row) =>
            row match {
              case result: ~[_, _] =>
                val row = flattenElements(result)
                row.head match {
                  case RowSeparator      => (acc += row, 0, false)
                  case TableBoundary     => (acc += row, 0, false)
                  case CellElement(text) =>
                    if (text.input.trim.isEmpty)
                      for (_ <- 1 to blanks) addBlankLines(acc, text.parent)
                    else if (rowOpen) addRowSeparators(acc)
                    (acc += row, 0, true)
                  case _ => (acc, blanks, rowOpen) // cannot happen, just to avoid the warning
                }
              case _               => (acc, blanks + 1, rowOpen) // blank line
            }
        }._1

        rowBuffer foreach { row =>
          def foreachColumn(
              row: List[TableElement]
          )(f: ((List[TableElement], ColumnBuilder)) => Any): Unit = {
            row.tail.dropRight(1).sliding(2, 2).zip(tableBuilder.columns.tail.iterator).foreach(f)
          }
          row.head match {
            case RowSeparator      =>
              val newRowBuilder = tableBuilder.nextRow
              newRowBuilder.addCell(tableBuilder.columns.head.nextCell)
              foreachColumn(row) {
                case (Intersection :: RowSeparator :: Nil, column) =>
                  newRowBuilder.addCell(column.nextCell)
                case (RowSeparator :: RowSeparator :: Nil, column) =>
                  column.mergeLeft(true)
                  newRowBuilder.addCell(column.nextCell)
                case _                                             => ()
              }
            case TableBoundary     =>
              foreachColumn(row) {
                case (Intersection :: TableBoundary :: Nil, _)       => ()
                case (TableBoundary :: TableBoundary :: Nil, column) => column.mergeLeft()
                case _                                               => ()
              }
            case CellElement(text) =>
              tableBuilder.columns.head.addLine(CellSeparator(""), text, nextRow = false)
              foreachColumn(row) {
                case (sep :: CellElement(text) :: Nil, column) =>
                  column.addLine(sep, text, nextRow = false)
                case _                                         => ()
              }
            case _                 => ()
          }
        }

        tableBuilder.toRowList(ct)
      }

      tablePart ~ opt(tablePart) ^^ {
        case head ~ Some(body) =>
          Table(TableHead(buildRowList(head, HeadCell)), TableBody(buildRowList(body, BodyCell)))
        case body ~ None       => Table(TableHead(Nil), TableBody(buildRowList(body, BodyCell)))
      }

    }

  }

  /** Internal control-flow exception. */
  private class MalformedTableException(msg: String) extends RuntimeException(msg)

}
