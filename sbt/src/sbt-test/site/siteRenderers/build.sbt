import laika.tree.Elements.Emphasized

name := "site-renderers"

version := "0.1"

scalaVersion := "2.10.6"

enablePlugins(LaikaPlugin)

laikaSiteRenderers += laikaSiteRenderer { out => {
  case Emphasized(content, _) => out << """<em class="foo">""" << content << "</em>"
}}
