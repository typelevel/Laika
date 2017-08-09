name := "site-target"

version := "0.1"

scalaVersion := "2.10.6"

enablePlugins(LaikaPlugin)

target in Laika := baseDirectory.value / "docs"
