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
  case object CellSeparator extends TableDecoration {
    override def toString = "|"
  }
  case class CellElement (text: String) extends TableElement {
    override def toString = text
  }
      
  class CellBuilder (pos: BlockPosition) {
    
    private val seps = new ListBuffer[TableElement]
    private val lines = new ListBuffer[StringBuilder]
    private var last: StringBuilder = new StringBuilder
    
    private var rowSpan = 1
    private var colSpan = 1
    
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
        (indent(pos.column) ~ restOfLine) ^^ { case indent ~ text => new TextLine(indent, text) } 
      
      parseAll(cellLine*, cellContent) match {
        case Success(lines, _) => 
          val minIndent = lines map (_.indent) min;
          (minIndent, lines map (_.padTo(minIndent)) mkString "\n") // TODO - trim trailing ws
      } 
    }
    
    def parsedCellContent = {
      val (minIndent, text) = trimmedCellContent
      //val parser = ((standardRstBlock(pos.indent(minIndent)) | paragraph)*) // TODO - base parser should include this standard block parser
      val parser = ((paragraph)*)
      parseMarkup(parser, text)
    }
    
    def toCell = Cell(BodyCell, parsedCellContent, colSpan, rowSpan)
  }
  
  class CellBuilderRef (val cell: CellBuilder, val mergedLeft: Boolean = false)
  
  class RowBuilder {
    private val cells = new ListBuffer[CellBuilder]
    
    def addCell (cell: CellBuilder) = cells += cell
     
    def toRow = Row(cells filterNot (_.removed) map (_.toCell))
  }
  
  class ColumnBuilder (pos: BlockPosition, left: scala.Option[ColumnBuilder]) {
    
    private val cells = new Stack[CellBuilderRef]
    
    def currentCell = cells.top.cell
    
    def nextCell () = {
      val cell = new CellBuilder(pos)
      cells push new CellBuilderRef(cell)
      cell
    }
    
    private def removeCell () = {
      val cell = cells.pop.cell
      cell.removed = true
      cell
    }
    
    private def mergeLeft () = {
      val leftCell = left.get.currentCell
      leftCell.merge(removeCell)
      cells push new CellBuilderRef(leftCell, true)
    }
    
    def addLine (sep: TableElement, line: String, nextRow: Boolean) = {
      val ref = cells.top
      if (ref.mergedLeft) ref.cell.currentLine(sep, line)
      else {
        ref.cell.nextLine(sep, line, nextRow)
        sep match {
          case CellElement(_) => mergeLeft()
          case _ => ()
        }
      }
    }
  }
  
  class TableBuilder (pos: BlockPosition, columnWidths: List[Int]) {
    object ColumnFactory {
      var lastColumn: scala.Option[ColumnBuilder] = None
      var nextPos = new BlockPosition(pos.nestLevel, pos.column + 1)
      val columnWidthIt = columnWidths.iterator
      def setNextPos () = nextPos = new BlockPosition(pos.nestLevel, pos.column + 1 + columnWidthIt.next)
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
    //case None => Nil
    //case Some(x) => flatten(x)
  }
      
  def gridTable (pos: BlockPosition): Parser[Block] = {
    
    val intersect = (anyOf('+') take 1) ^^^ Intersection
    
    val rowSep = (anyOf('-') min 1) ^^ { _.length }
    val topBorder = intersect ~> ((rowSep <~ intersect)+) <~ ws ~ eol
    
    topBorder >> { cols =>
      
      val columnCount = cols.length
      
      val colSep = (anyOf('|') take 1) ^^^ CellSeparator
      val colSepOrText = colSep | intersect | ((any take 1) ^^ CellElement)
      
      val separators = colSep :: List.fill(columnCount - 1)(colSepOrText)
      val colsWithSep = (separators, cols, separators.reverse).zipped.toList
      
      def rowSep (width: Int): Parser[Any] = 
        (intersect ~ ((anyOf('-') take width) ^^^ RowSeparator) <~ guard(intersect))
        
      def cell (sepL: Parser[Any], width: Int, sepR: Parser[Any]): Parser[Any] = 
        (sepL ~ ((any take width) ^^ CellElement) <~ guard(sepR))
      
      val row = colsWithSep map { case (separatorL, colWidth, separatorR) => 
        rowSep(colWidth) | cell(separatorL, colWidth, separatorR)
      } reduceRight (_ ~ _)
      
      ((row <~ (any take 1) ~ ws ~ eol)*) ^^ { rows =>
        val tableBuilder = new TableBuilder(pos, cols)
        
        rows.dropRight(1) foreach { result =>
          val row = flatten(result)
          val hasSeparator = row exists { case RowSeparator => true; case _ => false }
          val newRowBuilder = if (hasSeparator) Some(tableBuilder.nextRow) else None
          
          row.sliding(2,2).zip(tableBuilder.columns.iterator).foreach { 
            case (_ :: RowSeparator :: Nil, column) => newRowBuilder.get.addCell(column.nextCell) // TODO - handle misplaced separators
            case (sep :: CellElement(text) :: Nil, column) => column.addLine(sep, text, hasSeparator)
            case _ => () // cannot happen, just to avoid the warning
          }
        }
        
        tableBuilder.toTable
      }      
      
      // TODO - verify last line correctly closes all cells
    }
    
  }
  
  
  
}