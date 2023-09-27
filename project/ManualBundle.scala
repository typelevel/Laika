import cats.data.NonEmptySet
import laika.ast.Path.Root
import laika.ast.*
import laika.api.bundle.{ BlockDirectives, RenderOverrides, DirectiveRegistry }
import laika.format.{ HTML, XSLFO }

object ManualBundle extends DirectiveRegistry {

  private val colorsDirective = BlockDirectives.create("colors") {
    BlockDirectives.dsl.rawBody.map { body =>
      val lines = body.split('\n').map(_.trim)

      def createColorCell(line: String): Block = {
        val (color, name) = line.span(_ != ' ')
        val text          = Paragraph(name)
        val boxHTML       = s"""<div class="color-box" style="background-color: #$color"> </div>"""
        val colorBox      = RawContent(NonEmptySet.of("html", "epub"), boxHTML)
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

  val blockDirectives    = Seq(colorsDirective)
  val spanDirectives     = Nil
  val templateDirectives = Nil
  val linkDirectives     = Nil

  override def renderOverrides: Seq[RenderOverrides] = Seq(
    XSLFO.Overrides {
      case (_, bc: BlockContainer) if bc.hasStyle("color-block") => FoColorBoxes.render(bc)
    },
    HTML.Overrides {
      // use low-res cover images as thumbnails for PDF downloads
      case (fmt, Image(InternalTarget.Resolved(abs, rel, formats), _, _, alt, _, _))
          if abs.isSubPath(Root / "img" / "pdf") =>
        fmt.child(
          Image(
            InternalTarget.Resolved(
              abs.parent.parent / "cover" / abs.name,
              rel.parent.parent / "cover" / rel.name,
              formats
            ),
            None,
            None,
            alt
          )
        )
    }
  )

}
