import laika.ast.{Block, BlockContainer, RawContent, SpanContainer}

object FoColorBoxes {
  
  def render (block: BlockContainer): String = {
    
    val columnCount = block.content.head.asInstanceOf[BlockContainer].content.size
    
    val columns: String = 
      (1 to columnCount)
        .map(_ => """  <fo:table-column column-width="proportional-column-width(1)"/>""")
        .mkString("\n")
    
    def cell (block: Block): String = {
      val cell = block.asInstanceOf[BlockContainer]
      val text = cell.content.head.asInstanceOf[SpanContainer].extractText.trim
      val color = cell.content.last.asInstanceOf[RawContent].content.split('#').last.take(6)
      val margin = if (columnCount == 4) 1 else 0.6
      s"""
        |      <fo:table-cell padding-top="2mm">
        |        <fo:block font-family="Lato" font-size="10pt" font-weight="bold" line-height="1.5" space-after="3mm" text-align="center">$text</fo:block>
        |        <fo:block-container text-align="center" background-color="#$color" width="2cm" height="2cm" margin-left="${margin}cm" margin-bottom="6mm">
        |          <fo:block><fo:leader/></fo:block>
        |        </fo:block-container>
        |      </fo:table-cell>
      """.stripMargin
    }
    
    def row (block: Block): String = {
      val row = block.asInstanceOf[BlockContainer]
      s"""
        |    <fo:table-row>
        |${row.content.map(cell).mkString("\n")}
        |    </fo:table-row>""".stripMargin
    }
    
    s"""
      |<fo:table space-after="5mm" table-layout="fixed" width="100%">
      |          
      |$columns
      |        
      |  <fo:table-body>
      |  
      |${block.content.map(row).mkString("\n")}
      |          
      |  </fo:table-body>
      |  
      |</fo:table>
    """.stripMargin
  }
  
}
