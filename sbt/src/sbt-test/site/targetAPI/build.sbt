name := "site-targetAPI"

version := "0.1"

scalaVersion := "2.10.6"

enablePlugins(LaikaPlugin)

target in laikaCopyAPI := (target in laikaSite).value / "latest" / "api"

laikaIncludeAPI := true
