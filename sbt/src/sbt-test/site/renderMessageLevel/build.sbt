import laika.ast.MessageFilter

name := "site-renderMessageLevel"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(LaikaPlugin)

laikaConfig := LaikaConfig.defaults
  .renderMessages(MessageFilter.Warning)
  .failOnMessages(MessageFilter.None)
