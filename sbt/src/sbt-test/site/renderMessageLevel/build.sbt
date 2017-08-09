import laika.tree.Elements.Warning

name := "site-renderMessageLevel"

version := "0.1"

scalaVersion := "2.10.6"

enablePlugins(LaikaPlugin)

renderMessageLevel in Laika := Some(Warning)
