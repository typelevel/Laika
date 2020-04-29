
name := "site-config"

version := "0.1"

scalaVersion := "2.12.6"

laikaConfig := LaikaConfig.defaults.withConfigValue("prop", "World")

enablePlugins(LaikaPlugin)
