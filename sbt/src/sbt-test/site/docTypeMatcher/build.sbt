import laika.io.DocumentType._
import laika.tree.Paths.Path

name := "site-docTypeMatcher"

version := "0.1"

scalaVersion := "2.10.6"

enablePlugins(LaikaPlugin)

laikaDocTypeMatcher := Some({ case path: Path => path.name match {
  case "hello.md"   => Markup
  case "hello2.md"  => Static
}})
