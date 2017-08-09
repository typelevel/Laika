name := "site-targetAPI"

version := "0.1"

scalaVersion := "2.10.6"

enablePlugins(LaikaPlugin)

target in copyAPI in Laika := (target in site in Laika).value / "latest" / "api"

includeAPI in Laika := true
