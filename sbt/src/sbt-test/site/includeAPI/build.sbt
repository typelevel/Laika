name := "site-includeAPI"

version := "0.1"

scalaVersion := "2.12.21"

enablePlugins(LaikaPlugin)

Laika / target := baseDirectory.value / "target" / "docs"

laikaIncludeAPI := true
