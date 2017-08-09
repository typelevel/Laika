import laika.tree.Elements.Emphasized
import laika.tree.Elements.Strong

name := "site-rewriteRules"

version := "0.1"

scalaVersion := "2.10.6"

enablePlugins(LaikaPlugin)

rewriteRules in Laika += rewriteRule {
  case Emphasized(content,_) => Some(Strong(content))
}
