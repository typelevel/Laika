import laika.config.MessageFilters

name := "site-renderMessageLevel"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(LaikaPlugin)

laikaConfig := LaikaConfig.defaults
  .withMessageFilters(MessageFilters.forVisualDebugging)
