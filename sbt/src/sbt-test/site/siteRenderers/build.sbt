import laika.ast.Emphasized

name := "site-renderers"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(LaikaPlugin)

laikaExtensions += laikaHtmlRenderer { case (fmt, Emphasized(content, opt)) =>
  fmt.element("em", opt, content, "class" -> "foo")
}
