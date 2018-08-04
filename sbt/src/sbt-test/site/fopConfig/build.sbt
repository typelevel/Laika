name := "site-fopConfig"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(LaikaPlugin)

fopConfig := Some(baseDirectory.value / "customFop.xconf")
