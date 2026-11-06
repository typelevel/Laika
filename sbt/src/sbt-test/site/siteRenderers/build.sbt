import laika.ast.Emphasized

name := "site-renderers"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(LaikaPlugin)

Laika / target := baseDirectory.value / "target" / "docs"

laikaExtensions += laikaHtmlRenderer { case (fmt, e: Emphasized) =>
  fmt.element("em", e, "class" -> "foo")
}
