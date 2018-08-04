name := "site-target"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(LaikaPlugin)

target in Laika := baseDirectory.value / "docs"
