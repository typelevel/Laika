import laika.io.DocumentType._

name := "site-docTypeMatcher"

version := "0.1"

scalaVersion := "2.10.6"

enablePlugins(LaikaPlugin)

laikaDocTypeMatcher in Laika := Some(_.name match {
  case "hello.md"   => Markup
  case "hello2.md"  => Static
})
