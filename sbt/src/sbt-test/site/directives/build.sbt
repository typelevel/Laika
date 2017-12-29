import laika.tree.Elements._
import laika.directive.Directives
import laika.directive.Directives._
import laika.tree.Templates.TemplateString

name := "site-directives"

version := "0.1"

scalaVersion := "2.10.6"

enablePlugins(LaikaPlugin)

spanDirectives in Laika += Spans.create("span") {
  Spans.Combinators.attribute(Directives.Default) map (Literal(_))
}

blockDirectives in Laika += Blocks.create("block") {
  Blocks.Combinators.attribute(Directives.Default) map (LiteralBlock(_))
}

templateDirectives in Laika += Templates.create("directive") {
  Templates.Combinators.attribute(Directives.Default) map { TemplateString(_) }
}
