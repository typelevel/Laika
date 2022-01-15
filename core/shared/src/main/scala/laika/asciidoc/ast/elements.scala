package laika.asciidoc.ast

import laika.ast.SpanContainer
import laika.ast.NoOpt
import laika.ast.Options
import laika.ast.Span
import laika.ast.SpanContainerCompanion

case class Mark (content: Seq[Span], options: Options = NoOpt) extends Span with SpanContainer {
  type Self = Mark
  def withContent (newContent: Seq[Span]): Mark = copy(content = newContent)
  def withOptions (options: Options): Mark = copy(options = options)
}
object Mark extends SpanContainerCompanion {
  type ContainerType = Mark
  protected def createSpanContainer (spans: Seq[Span]): Mark = Mark(spans)
}
