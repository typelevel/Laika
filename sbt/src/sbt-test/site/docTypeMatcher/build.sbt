import LaikaKeys._
import laika.tree.Documents._

name := "site-docTypeMatcher"

version := "0.1"

scalaVersion := "2.10.3"

LaikaPlugin.defaults

docTypeMatcher in Laika := Some(_.name match { 
  case "hello.md"   => Markup
  case "hello2.md"  => Static
})