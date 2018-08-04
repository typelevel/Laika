import laika.tree.Elements.Warning

name := "site-renderMessageLevel"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(LaikaPlugin)

laikaConfig := LaikaConfig(renderMessageLevel = Warning)
