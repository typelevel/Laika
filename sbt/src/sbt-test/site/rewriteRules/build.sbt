import LaikaKeys._
import laika.tree.Elements.Emphasized
import laika.tree.Elements.Strong

name := "site-rewriteRules"

version := "0.1"

scalaVersion := "2.10.3"

LaikaPlugin.defaults

rewriteRules in Laika += rewriteRule { 
  case Emphasized(content,_) => Some(Strong(content))
}
