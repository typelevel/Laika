import cats.data.NonEmptySet
import laika.ast.{Block, BlockContainer, BlockSequence, Paragraph, RawContent, Styles}
import laika.bundle.RenderOverrides
import laika.directive.{Blocks, DirectiveRegistry}
import laika.format.XSLFO

object ManualBundle extends DirectiveRegistry {

  private val colorsDirective = Blocks.create("colors") {
    import cats.implicits._
    Blocks.dsl.rawBody.map { body =>
      val lines = body.split('\n').map(_.trim)

      def createColorCell (line: String): Block = {
        val (color, name) = line.span(_ != ' ')
        val text = Paragraph(name)
        val boxHTML = s"""<div class="color-box" style="background-color: #$color"> </div>"""
        val colorBox = RawContent(NonEmptySet.of("html", "epub"), boxHTML)
        BlockSequence(Seq(text, colorBox), Styles("color-cell"))
      }

      val (rows, lastRow) = lines.foldLeft((Seq.empty[Block], Option.empty[BlockSequence])) {
        case ((previousRows, currentRow), line) =>
          if (line.isEmpty) {
            (previousRows ++ currentRow.toSeq, None)
          }
          else {
            val curSpan = currentRow.getOrElse(BlockSequence.empty.withStyle("color-row"))
            (previousRows, Some(curSpan.withContent(curSpan.content :+ createColorCell(line))))
          }
      }
      BlockSequence(rows ++ lastRow.toSeq, Styles("color-block"))
    }
  }

  val blockDirectives = Seq(colorsDirective)
  val spanDirectives = Nil
  val templateDirectives = Nil
  val linkDirectives = Nil

  override def renderOverrides: Seq[RenderOverrides] = Seq(XSLFO.Overrides {
    case (_, bc: BlockContainer) if bc.hasStyle("color-block") => FoColorBoxes.render(bc)
  })
}
