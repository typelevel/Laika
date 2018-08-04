name := "site-targetAPI"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(LaikaPlugin)

target in laikaCopyAPI := (target in laikaSite).value / "latest" / "api"

laikaIncludeAPI := true
