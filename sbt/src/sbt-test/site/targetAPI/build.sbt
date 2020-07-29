name := "site-targetAPI"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(LaikaPlugin)

laikaCopyAPI / target := (laikaSite / target).value / "latest" / "api"

laikaIncludeAPI := true
