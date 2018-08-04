import laika.directive.DirectiveRegistry
import laika.tree.Elements._
import laika.directive.Directives
import laika.directive.Directives._
import laika.tree.Templates.TemplateString

name := "site-directives"

version := "0.1"

scalaVersion := "2.12.6"

val TestDirectives = new DirectiveRegistry {
  val spanDirectives = Seq(Spans.create("span") {
    Spans.Combinators.attribute(Directives.Default) map (Literal(_))
  })
  val blockDirectives = Seq(Blocks.create("block") {
    Blocks.Combinators.attribute(Directives.Default) map (LiteralBlock(_))
  })
  val templateDirectives = Seq(Templates.create("directive") {
    Templates.Combinators.attribute(Directives.Default) map { TemplateString(_) }
  })
}

enablePlugins(LaikaPlugin)

laikaExtensions += TestDirectives
