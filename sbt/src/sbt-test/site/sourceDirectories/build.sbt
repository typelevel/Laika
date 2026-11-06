name := "site-sourceDirectories"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(LaikaPlugin)

Laika / target := baseDirectory.value / "target" / "docs"

Laika / sourceDirectories += baseDirectory.value / "shared"
