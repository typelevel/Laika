import laika.ast._
import laika.api.bundle.{ BlockDirectives, DirectiveRegistry, SpanDirectives, TemplateDirectives }

name := "site-directives"

version := "0.1"

scalaVersion := "2.12.6"

val TestDirectives = new DirectiveRegistry {

  val spanDirectives = Seq(SpanDirectives.create("span") {
    import SpanDirectives.dsl._
    attribute(0).as[String] map (Literal(_))
  })

  val blockDirectives = Seq(BlockDirectives.create("block") {
    import BlockDirectives.dsl._
    attribute(0).as[String] map (LiteralBlock(_))
  })

  val templateDirectives = Seq(TemplateDirectives.create("directive") {
    import TemplateDirectives.dsl._
    attribute(0).as[String] map { TemplateString(_) }
  })

  val linkDirectives = Nil
}

enablePlugins(LaikaPlugin)

laikaExtensions += TestDirectives
