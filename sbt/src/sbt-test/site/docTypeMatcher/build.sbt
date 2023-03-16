import laika.ast.DocumentType._
import laika.ast.Path

name := "site-docTypeMatcher"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(LaikaPlugin)

laikaExtensions += laikaDocTypeMatcher {
  case path: Path if path.name == "hello.md"  => Markup
  case path: Path if path.name == "hello2.md" => Static()
}
