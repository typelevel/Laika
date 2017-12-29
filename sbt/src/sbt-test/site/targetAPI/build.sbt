name := "site-targetAPI"

version := "0.1"

scalaVersion := "2.10.6"

enablePlugins(LaikaPlugin)

target in laikaCopyAPI in Laika := (target in laikaSite in Laika).value / "latest" / "api"

laikaIncludeAPI in Laika := true
