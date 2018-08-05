import laika.ast.Emphasized

name := "site-renderers"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(LaikaPlugin)

laikaExtensions += laikaHtmlRenderer { out => {
  case Emphasized(content, _) => out << """<em class="foo">""" << content << "</em>"
}}
