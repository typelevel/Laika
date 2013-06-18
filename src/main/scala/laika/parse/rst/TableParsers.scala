/*
 * Copyright 2013 the original author or authors.
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

package laika.parse.rst

import laika.tree.Elements._
import laika.parse.rst.Elements._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack

/** Provides parsers for the two table types supported by reStructuredText.
 * 
 * @author Jens Halm
 */
trait TableParsers extends laika.parse.BlockParsers { self: InlineParsers => 

  
  private abstract class TableElement
  
  private abstract class TableDecoration extends TableElement
  private case object Intersection extends TableDecoration {
    override def toString = "+"
  }
  private case object RowSeparator extends TableDecoration
  private case object TableBoundary extends TableDecoration
  private case class CellSeparator (decoration: String) extends TableDecoration {
    override def toString = decoration
  }
  private case class CellElement (text: String) extends TableElement {
    override def toString = text
  }
      
  class CellBuilder (nestLevel: Int) {
    
    private val seps = new ListBuffer[TableElement]
    private val lines = new ListBuffer[StringBuilder]
    private var last: StringBuilder = new StringBuilder
    
    var rowSpan = 1
    var colSpan = 1
    
    var removed: Boolean = false
    
    def nextLine (sep: TableElement, line: String, nextRow: Boolean) = { 
      seps += sep
      last = new StringBuilder(line)
      lines += last
      if (nextRow) rowSpan += 1
    }
    def currentLine (sep: TableElement, line: String) = {
      last ++= sep.toString
      last ++= line
    }
    def merge (right: CellBuilder) = {
      (lines, right.seps, right.lines).zipped.foreach {
        case (left, sep, right) => left ++= sep.toString ++= right
      }
      colSpan += 1
    }
    
    def cellContent = lines map (_.toString) mkString "\n"
    
    def trimmedCellContent = {
      abstract class CellLine (val indent: Int) { def padTo (indent: Int): String }
      object BlankLine extends CellLine(Int.MaxValue) { def padTo (indent: Int) = "" }
      class TextLine (i: Int, text: String) extends CellLine(i) { def padTo (minIndent: Int) = " " * (indent - minIndent) + text }
      
      val cellLine = not(eof) ~> (((blankLine) ^^^ BlankLine) | 
        (ws ~ restOfLine) ^^ { case indent ~ text => new TextLine(indent.length, text.trim) }) 
      
      parseAll(cellLine*, cellContent) match {
        case Success(lines, _) => 
          val minIndent = lines map (_.indent) min;
          (minIndent, lines map (_.padTo(minIndent)))
        case _ => (0,Nil) // TODO - error handling for edge cases
      }
    }
    
    def parsedCellContent = {
      val (minIndent, lines) = trimmedCellContent
      parseNestedBlocks(lines, nestLevel)
    }
    
    def toCell (ct: CellType) = Cell(ct, parsedCellContent, colSpan, rowSpan)
  }
  
  class CellBuilderRef (val cell: CellBuilder, val mergedLeft: Boolean = false)
  
  class RowBuilder {
    private val cells = new ListBuffer[CellBuilder]
    
    def addCell (cell: CellBuilder) = cells += cell
     
    def toRow (ct: CellType) = Row(cells filterNot (_.removed) map (_.toCell(ct)) toList)
  }
  
  class ColumnBuilder (left: Option[ColumnBuilder], nestLevel: Int) {
    
    private var rowSpan = 1 // only used for sanity checks
    
    private val cells = new Stack[CellBuilderRef]
    
    def currentCell = cells.top.cell
    
    def previousCell = cells(1).cell
    
    def nextCell () = {
      if (!cells.isEmpty && cells.top.mergedLeft && rowspanDif != 0)
          throw new MalformedTableException("Illegal merging of rows with different cellspans")
      val cell = new CellBuilder(nestLevel)
      cells push new CellBuilderRef(cell)
      cell
    }
    
    private def removeCell () = {
      val cell = cells.pop.cell
      cell.removed = true
      cell
    }
    
    def mergeLeft (previous: Boolean = false) = {
      if (rowspanDif != 0)
          throw new MalformedTableException("Illegal merging of cells with different rowspans")
      val leftCell = if (previous) left.get.previousCell else left.get.currentCell
      leftCell.merge(removeCell)
      cells push new CellBuilderRef(leftCell, true)
    }
    
    def rowspanDif = {
      left.get.rowSpan - rowSpan
    }
    
    def addLine (sep: TableElement, line: String, nextRow: Boolean) = {
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
          case _ => ()
        }
      }
      if (nextRow) rowSpan += 1
    }
  }
  
  class TableBuilder (columnWidths: List[Int], nestLevel: Int) {
    object ColumnFactory {
      var lastColumn: Option[ColumnBuilder] = None
      val columnWidthIt = columnWidths.iterator
      def next = { lastColumn = Some(new ColumnBuilder(lastColumn, nestLevel)); lastColumn.get } 
    }
    val columns = List.fill(columnWidths.length)(ColumnFactory.next)
    private val rows = new ListBuffer[RowBuilder]
    
    private def init () = {
      val row = nextRow
      columns foreach (col => row.addCell(col.nextCell))
    }
    init()
    
    def nextRow = {
      val row = new RowBuilder
      rows += row
      row
    }
    
    def toRowList (ct: CellType) = rows map (_.toRow(ct)) toList
  }
  
      
  private def flattenElements (result: Any): List[TableElement] = result match {
    case x:TableElement => List(x)
    case x ~ y => flattenElements(x) ::: flattenElements(y)
  }
   
  /** Parses a grid table.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#grid-tables]].
   */
  def gridTable: Parser[Block] = {
    
    val intersect = (anyOf('+') take 1) ^^^ Intersection
    
    val rowSep = (anyOf('-') min 1) ^^ { _.length }
    val topBorder = intersect ~> ((rowSep <~ intersect)+) <~ ws ~ eol
    
    withNestLevel(topBorder) >> { case (nestLevel, cols) =>
      
      val colSep = ((anyOf('|') take 1) ^^^ CellSeparator("|")) | intersect
      val colSepOrText = colSep | ((any take 1) ^^ CellElement)
      
      val separators = colSep :: List.fill(cols.length - 1)(colSepOrText)
      val colsWithSep = (separators, cols, separators.reverse).zipped.toList
      
      def rowSep (width: Int): Parser[Any] = 
        (intersect ~ ((anyOf('-') take width) ^^^ RowSeparator) <~ guard(intersect))
        
      def boundaryPart (width: Int): Parser[Any] = 
        (intersect ~ ((anyOf('=') take width) ^^^ TableBoundary) <~ guard(intersect))
        
      def cell (sepL: Parser[Any], width: Int, sepR: Parser[Any]): Parser[Any] = 
        (sepL ~ ((any take width) ^^ CellElement) <~ guard(sepR))
      
      val row = (colsWithSep map { case (separatorL, colWidth, separatorR) => 
        rowSep(colWidth) | cell(separatorL, colWidth, separatorR)
      } reduceRight (_ ~ _)) ^^ flattenElements
      
      val tableBoundary = (cols map { col => boundaryPart(col) } reduceRight (_ ~ _)) ^^^ TableBoundary
      
      def isSeparatorRow (row: List[TableElement]) = {
        row.forall {
          case RowSeparator => true
          case Intersection => true
          case _ => false
        }
      }
      
      def buildRowList (rows: List[List[TableElement]], ct: CellType) = {
        
        val tableBuilder = new TableBuilder(cols map (_ + 1), nestLevel) // column width includes separator
            
        rows foreach { row =>
          val hasSeparator = row exists { case RowSeparator => true; case _ => false }
          val newRowBuilder = if (hasSeparator) Some(tableBuilder.nextRow) else None
          
          row.sliding(2,2).zip(tableBuilder.columns.iterator).foreach { 
            case (_ :: RowSeparator :: Nil, column) => newRowBuilder.get.addCell(column.nextCell)
            case (sep :: CellElement(text) :: Nil, column) => column.addLine(sep, text, hasSeparator)
            case _ => () // cannot happen, just to avoid the warning
          }
        }
        tableBuilder.toRowList(ct)
      }
      
      def validateLastRow (rows: List[List[TableElement]]) = {
        if (rows.isEmpty || !isSeparatorRow(rows.last)) throw new MalformedTableException("Table not terminated correctly")
      }
      
      val boundaryRow = tableBoundary <~ (any take 1) ~ ws ~ eol
      val tablePart = ((not(tableBoundary) ~> row <~ (any take 1) ~ ws ~ eol)*)
      (tablePart ~ opt(boundaryRow ~> tablePart)) ^^? { result =>
        
      /* Need to fail for certain illegal constructs in the interim model, 
       * so that the next parser can pick up the (broken) table input */
        try {
          val table = result match {
            case head ~ Some(body) => validateLastRow(body); Table(TableHead(buildRowList(head, HeadCell)), TableBody(buildRowList(body.init, BodyCell)))
            case body ~ None       => validateLastRow(body); Table(TableHead(Nil), TableBody(buildRowList(body.init, BodyCell)))
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
  def simpleTable: Parser[Block] = {
    
    val intersect = (anyOf(' ') min 1) ^^ { _.length }
    val tableBorder = (anyOf('=') min 1) ^^ { _.length }
    val columnSpec = tableBorder ~ opt(intersect) ^^ {
      case col ~ Some(sep) => (col, sep)
      case col ~ None      => (col, 0)
    }
    val topBorder = repMin(2, columnSpec) <~ ws ~ eol
    
    withNestLevel(topBorder) >> { case (nestLevel, cols) =>
      
      val (rowColumns, boundaryColumns) = (cols map { case (col, sep) =>
        val cellText = if (sep == 0) (anyBut('\n','\r')) ^^ CellElement 
                       else (any take col) ^^ CellElement 
        val separator = (anyOf(' ') take sep) ^^ CellSeparator
        val textInSep = (any take sep) ^^ CellSeparator
        val textColumn = cellText ~ (separator | textInSep)
        
        val rowSep = (anyOf('-') take col) ^^^ RowSeparator
        val merged = (anyOf('-') take sep) ^^^ RowSeparator
        val split =  (anyOf(' ') take sep) ^^^ Intersection
        val underline = rowSep ~ (split | merged)
        
        val bCell = (anyOf('=') take col) ^^^ TableBoundary
        val bMerged = (anyOf('=') take sep) ^^^ TableBoundary
        val bSplit =  (anyOf(' ') take sep) ^^^ Intersection
        val boundary = bCell ~ (bSplit | bMerged)
        
        ((underline | not(boundary) ~> textColumn).asInstanceOf[Parser[Any]], boundary.asInstanceOf[Parser[Any]]) 
      }).unzip
      
      val row = (rowColumns reduceRight (_ ~ _)) <~ ws ~ eol
      val boundary = (boundaryColumns reduceRight (_ ~ _)) <~ ws ~ eol
      val blank = not(eof) ~> blankLine
      
      val tablePart = (((blank | row)*) ~ boundary) ^^ { case rows ~ boundary => rows :+ boundary }
      
      
      def buildRowList (rows: List[Any], ct: CellType) = {
        
        val tableBuilder = new TableBuilder(cols map { col => col._1 + col._2 }, nestLevel)
        
        def addBlankLines (acc: ListBuffer[List[TableElement]]) = 
            acc += (cols map { case (cell, sep) => List(CellElement(" " * cell), CellSeparator(" " * sep)) }).flatten
        
        def addRowSeparators (acc: ListBuffer[List[TableElement]]) = 
          acc += (cols map { _ => List(RowSeparator, Intersection) }).flatten
      
        /* in contrast to the grid table, some rows need to be processed in context,
         * as their exact behaviour depends on preceding or following lines. */
        val rowBuffer = ((ListBuffer[List[TableElement]](), 0, false) /: rows) { case ((acc, blanks, rowOpen), row) =>
          row match {
            case result: ~[_,_] => 
              val row = flattenElements(result)
              row.head match {
                case RowSeparator => (acc += row, 0, false)
                case TableBoundary => (acc += row, 0, false)
                case CellElement(text) => 
                  if (text.trim.isEmpty) for (_ <- 1 to blanks) addBlankLines(acc)
                  else if (rowOpen) addRowSeparators(acc)
                  (acc += row, 0, true)
                case _ => (acc, blanks, rowOpen) // cannot happen, just to avoid the warning 
              }
            case _ => (acc, blanks + 1, rowOpen) // blank line 
          }
        }._1
        
        rowBuffer foreach { row =>
          
          def foreachColumn (row: List[TableElement])(f: ((List[TableElement], ColumnBuilder)) => Any) = {
            row.tail.dropRight(1).sliding(2,2).zip(tableBuilder.columns.tail.iterator).foreach(f)
          }
          row.head match {
            case RowSeparator => 
              val newRowBuilder = tableBuilder.nextRow
              newRowBuilder.addCell(tableBuilder.columns.head.nextCell)
              foreachColumn(row) {
                case (Intersection :: RowSeparator :: Nil, column) => newRowBuilder.addCell(column.nextCell)
                case (RowSeparator :: RowSeparator :: Nil, column) => column.mergeLeft(true); newRowBuilder.addCell(column.nextCell)
                case _ => ()
              }
            case TableBoundary =>
              foreachColumn(row) {
                case (Intersection :: TableBoundary :: Nil, column) => ()
                case (TableBoundary :: TableBoundary :: Nil, column) => column.mergeLeft()
                case _ => ()
              }
            case CellElement(text) =>
              tableBuilder.columns.head.addLine(CellSeparator(""), text, false)
              foreachColumn(row) {
                case (sep :: CellElement(text) :: Nil, column) => column.addLine(sep, text, false)
                case _ => ()
              }
            case _ => ()
          }
        }
        
        tableBuilder.toRowList(ct)
      }
      
      tablePart ~ opt(tablePart) ^^ { 
        case head ~ Some(body) => Table(TableHead(buildRowList(head, HeadCell)), TableBody(buildRowList(body, BodyCell)))
        case body ~ None       => Table(TableHead(Nil), TableBody(buildRowList(body, BodyCell)))
      }
      
    }
    
  }
  
  class MalformedTableException (msg: String) extends RuntimeException(msg)
  
}