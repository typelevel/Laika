import LaikaKeys._
import laika.tree.Elements.Emphasized

name := "site-renderers"

version := "0.1"

scalaVersion := "2.10.3"

LaikaPlugin.defaults

siteRenderers in Laika += siteRenderer { out => {
  case Emphasized(content, _) => out << """<em class="foo">""" << content << "</em>" 
}}
