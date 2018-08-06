import laika.ast._
import laika.directive.{Blocks, DirectiveRegistry, Spans, Templates}

name := "site-directives"

version := "0.1"

scalaVersion := "2.12.6"

val TestDirectives = new DirectiveRegistry {
  val spanDirectives = Seq(Spans.create("span") {
    import Spans.dsl._
    attribute(Default) map (Literal(_))
  })
  val blockDirectives = Seq(Blocks.create("block") {
    import Blocks.dsl._
    attribute(Default) map (LiteralBlock(_))
  })
  val templateDirectives = Seq(Templates.create("directive") {
    import Templates.dsl._
    attribute(Default) map { TemplateString(_) }
  })
}

enablePlugins(LaikaPlugin)

laikaExtensions += TestDirectives
