package laika.parse.rst

import laika.tree.Elements._
import laika.parse.rst.Elements._
import laika.parse.InlineParsers
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack

trait TableParsers extends BlockBaseParsers { self: InlineParsers => // TODO - probably needs to be rst.InlineParsers 

  
  // TODO - Option should be renamed in rst.Elements to avoid scala prefix for SDK Option
  
  
  sealed abstract class TableElement
  
  sealed abstract class TableDecoration extends TableElement
  case object Intersection extends TableDecoration {
    override def toString = "+"
  }
  case object RowSeparator extends TableDecoration
  case object TableBoundary extends TableDecoration
  case class CellSeparator (decoration: String) extends TableDecoration {
    override def toString = decoration
  }
  case class CellElement (text: String) extends TableElement {
    override def toString = text
  }
      
  class CellBuilder (pos: BlockPosition) {
    
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
      
      val cellLine = ((not(eof) ~ blankLine) ^^^ BlankLine) | 
        (indent(pos.column) ~ restOfLine) ^^ { case indent ~ text => new TextLine(indent, text.trim) } 
      
      parseAll(cellLine*, cellContent) match {
        case Success(lines, _) => 
          val minIndent = lines map (_.indent) min;
          (minIndent, lines map (_.padTo(minIndent)) mkString "\n")
      } 
    }
    
    def parsedCellContent = {
      val (minIndent, text) = trimmedCellContent
      //val parser = ((standardRstBlock(pos.indent(minIndent)) | paragraph)*) // TODO - base parser should include this standard block parser
      val parser = ((paragraph <~ opt(blankLines))*)
      parseMarkup(parser, text)
    }
    
    def toCell = Cell(BodyCell, parsedCellContent, colSpan, rowSpan)
  }
  
  class CellBuilderRef (val cell: CellBuilder, val mergedLeft: Boolean = false)
  
  class RowBuilder {
    private val cells = new ListBuffer[CellBuilder]
    
    def addCell (cell: CellBuilder) = cells += cell
     
    def toRow = Row(cells filterNot (_.removed) map (_.toCell) toList)
  }
  
  class ColumnBuilder (pos: BlockPosition, left: scala.Option[ColumnBuilder]) {
    
    private var rowSpan = 1 // only used for sanity checks
    
    private val cells = new Stack[CellBuilderRef]
    
    def currentCell = cells.top.cell
    
    def previousCell = cells(1).cell
    
    def nextCell () = {
      if (!cells.isEmpty && cells.top.mergedLeft && rowspanDif != 0)
          throw new MalformedTableException("Illegal merging of rows with different cellspans")
      val cell = new CellBuilder(pos)
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
  
  class TableBuilder (pos: BlockPosition, columnWidths: List[Int]) {
    object ColumnFactory {
      var lastColumn: scala.Option[ColumnBuilder] = None
      var nextPos = new BlockPosition(pos.nestLevel, pos.column + 1)
      val columnWidthIt = columnWidths.iterator
      def setNextPos () = nextPos = new BlockPosition(pos.nestLevel, pos.column + columnWidthIt.next)
      def next = { lastColumn = Some(new ColumnBuilder(nextPos, lastColumn)); setNextPos(); lastColumn.get } 
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
    
    def toTable = Table(Nil, rows map (_.toRow) toList) // TODO - process header cells
  }
  
      
  def flatten (result: Any): List[TableElement] = result match {
    case x:TableElement => List(x)
    case x ~ y => flatten(x) ::: flatten(y)
  }
      
  def gridTable (pos: BlockPosition): Parser[Block] = {
    
    val intersect = (anyOf('+') take 1) ^^^ Intersection
    
    val rowSep = (anyOf('-') min 1) ^^ { _.length }
    val topBorder = intersect ~> ((rowSep <~ intersect)+) <~ ws ~ eol
    
    topBorder >> { cols =>
      
      val colSep = ((anyOf('|') take 1) ^^^ CellSeparator("|")) | intersect
      val colSepOrText = colSep | ((any take 1) ^^ CellElement)
      
      val separators = colSep :: List.fill(cols.length - 1)(colSepOrText)
      val colsWithSep = (separators, cols, separators.reverse).zipped.toList
      
      def rowSep (width: Int): Parser[Any] = 
        (intersect ~ ((anyOf('-') take width) ^^^ RowSeparator) <~ guard(intersect))
        
      def cell (sepL: Parser[Any], width: Int, sepR: Parser[Any]): Parser[Any] = 
        (sepL ~ ((any take width) ^^ CellElement) <~ guard(sepR))
      
      val row = colsWithSep map { case (separatorL, colWidth, separatorR) => 
        rowSep(colWidth) | cell(separatorL, colWidth, separatorR)
      } reduceRight (_ ~ _)
      
      def isSeparatorRow (row: Any) = {
        flatten(row).forall {
          case RowSeparator => true
          case Intersection => true
          case _ => false
        }
      }
      
      ((row <~ (any take 1) ~ ws ~ eol)*) >> { rows =>
        
        /* this parser does not actually parse anything, but we need to fail for certain illegal
         * constructs in the interim model, so that the next parser can pick up the (broken) table input */
        Parser { in =>
          
          if (rows.isEmpty || !isSeparatorRow(rows.last)) Failure("Table not terminated correctly", in)
          else {
            val tableBuilder = new TableBuilder(pos, cols map (_ + 1)) // column width includes separator
            
            try {
              rows.init foreach { result =>
                val row = flatten(result)
                val hasSeparator = row exists { case RowSeparator => true; case _ => false }
                val newRowBuilder = if (hasSeparator) Some(tableBuilder.nextRow) else None
                
                row.sliding(2,2).zip(tableBuilder.columns.iterator).foreach { 
                  case (_ :: RowSeparator :: Nil, column) => newRowBuilder.get.addCell(column.nextCell)
                  case (sep :: CellElement(text) :: Nil, column) => column.addLine(sep, text, hasSeparator)
                  case _ => () // cannot happen, just to avoid the warning
                }
              }
              Success(tableBuilder.toTable, in)
            }
            catch {
              case ex: MalformedTableException => Failure(ex.getMessage, in)
            }
          }
        }
      }      
      
    }
    
  }
  
  def simpleTable (pos: BlockPosition): Parser[Block] = {
    
    val intersect = (anyOf(' ') min 1) ^^ { _.length }
    val tableBorder = (anyOf('=') min 1) ^^ { _.length }
    val columnSpec = tableBorder ~ opt(intersect) ^^ {
      case col ~ Some(sep) => (col, sep)
      case col ~ None      => (col, 0)
    }
    val topBorder = repMin(2, columnSpec) <~ ws ~ eol
    
    topBorder >> { cols =>
      
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
      
      (((blank | row)*) ~ boundary) ^^ { case rows ~ boundary =>
        
        val tableBuilder = new TableBuilder(pos, cols map { col => col._1 + col._2 })
        
        def addBlankLines (acc: ListBuffer[List[TableElement]]) = 
            acc += (cols map { case (cell, sep) => List(CellElement(" " * cell), CellSeparator(" " * sep)) }).flatten
        
        def addRowSeparators (acc: ListBuffer[List[TableElement]]) = 
          acc += (cols map { _ => List(RowSeparator, Intersection) }).flatten
      
        /* in contrast to the grid table, some rows need to be processed in context,
         * as their exact behaviour depends on preceding or following lines. 
         * TODO: this preprocessing might get eliminated in a refactoring */
        val rowBuffer = ((ListBuffer[List[TableElement]](), 0, false) /: (rows :+ boundary)) { case ((acc, blanks, rowOpen), row) =>
          row match {
            case result: ~[_,_] => 
              val row = flatten(result)
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

        tableBuilder.toTable
      }
      
    }
    
  }
  
  class MalformedTableException (msg: String) extends RuntimeException(msg)
  
}