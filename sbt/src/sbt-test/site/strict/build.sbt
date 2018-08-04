name := "site-strict"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(LaikaPlugin)

laikaConfig := LaikaConfig(strict = true)
