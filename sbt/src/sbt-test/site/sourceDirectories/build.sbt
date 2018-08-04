name := "site-sourceDirectories"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(LaikaPlugin)

sourceDirectories in Laika += baseDirectory.value / "shared"
